"""
Create timeseries of non-usable capacity due to unplanned outage from outage events data. Source: EEX
Unplanned outage is assumed to be "effective" until the end of the day if the event occurred before the Day-ahead auction.
If it occurred after the auction, the effective end is the end of the next day.
This approach is based on Hagemann (2013).
"""

import pendulum
import polars as pl
from src.preprocess.generation_unavailability.utils import (
    parse_capacity_series,
    get_da_gate_closure,
    get_outage_effective_max_end,
)


def create_unplanned_outage_15min_timeseries(
    start: pendulum.datetime, end: pendulum.datetime
):
    outage_data = (
        pl.read_csv(
            "data/generation_unavailability_from_eex.csv",
            columns=[
                "NUMStartDate",
                "NUMEndDate",
                "EventID",
                "Type",
                "Status",
                "CapacitySeries",
            ],
        )
        # Convert to datetime
        .with_columns(pl.col("^.*Date$").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("NUMStartDate").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
        # Keep only active, unplanned unavailability
        .filter((pl.col("Status") != "Dismissed") & (pl.col("Type") == "Unplanned"))
    )

    # Separate EventID to unique event ID and version number
    outage_data = (
        outage_data.with_columns(
            (
                pl.col("EventID").str.split("#", inclusive=True).list[0]
                + pl.col("EventID").str.split("#").list[1].str.split("_").list[0]
            ).alias("unique_event_id"),
            pl.col("EventID")
            .str.split("#")
            .list[1]
            .str.split("_")
            .list.to_struct()
            .alias("temp"),
        )
        .unnest("temp")
        .rename({"field_1": "version_num"})
        .with_columns(pl.col("version_num").cast(pl.Int64))
        .drop("EventID", "field_0")
    )

    # Keep only the final version
    outage_data = outage_data.filter(
        (pl.col("version_num") == pl.col("version_num").max().over("unique_event_id"))
        | pl.col("version_num").is_null()
    )

    # Get effective interval end
    # If an event occurred before DA auction, the effective end is the end of that day
    # If after, the effective end is the end of the next day
    # Truncate NUMEndDate at effective interval end
    outage_data = (
        outage_data.with_columns(
            get_da_gate_closure(pl.col("NUMStartDate")).alias("da_gate_closure")
        )
        .with_columns(
            pl.when(
                pl.col("NUMStartDate") >= pl.col("da_gate_closure"),
            )
            .then(get_outage_effective_max_end(pl.col("NUMStartDate"), after_da=True))
            .otherwise(
                get_outage_effective_max_end(pl.col("NUMStartDate"), after_da=False)
            )
            .alias("effective_interval_end")
        )
        .with_columns(
            pl.min_horizontal("NUMEndDate", "effective_interval_end").alias(
                "effective_interval_end"
            )
        )
    )

    # Parse nested CapacitySeries
    outage_data = (
        outage_data.with_columns(
            pl.col("CapacitySeries").str.split("|").alias("series_elements")
        )
        .explode("series_elements")
        .with_columns(
            pl.col("series_elements")
            .map_elements(parse_capacity_series, return_dtype=pl.Struct)
            .alias("parsed_series"),
        )
        .unnest("parsed_series")
        .with_columns(
            pl.col("AvailableCapacity").cast(pl.Float64),
            pl.col("NUMCapacity").cast(pl.Float64),
            pl.col("IntervalStart")
            .str.to_datetime("%Y-%m-%dT%H:%M:%SZ")
            .dt.replace_time_zone("UTC"),
            pl.col("IntervalEnd")
            .str.to_datetime("%Y-%m-%dT%H:%M:%SZ")
            .dt.replace_time_zone("UTC"),
        )
        .drop("series_elements", "CapacitySeries")
        .with_columns(
            pl.cum_count("IntervalStart")
            .over("unique_event_id", order_by="IntervalStart")
            .alias("series_index")  # helper column
        )
    )

    # Create a list of timestamps and values to expand to timeseries
    outage_data = (
        outage_data.with_columns(
            pl.col("IntervalStart")
            .dt.truncate("1m")
            .alias("floored_interval_start"),  # truncate at minutely resolution
            pl.col("effective_interval_end")
            .dt.truncate("1m")
            .alias("floored_interval_end"),
        )
        .with_columns(
            # If the second of effective_interval_end is zero, shift one minute back to get interval start of the minute in which
            # the outage ended.
            pl.when(pl.col("effective_interval_end").dt.second() == 0)
            .then(pl.col("floored_interval_end") - pl.duration(minutes=1))
            .otherwise("floored_interval_end")
            .alias("floored_interval_end")
        )
        .with_columns(
            (
                pl.col("floored_interval_start")
                == pl.col("floored_interval_start").max().over("unique_event_id")
            ).alias("is_last_start")
        )
        .with_columns(
            # For each unique event, take floored_interval_end of the final row as the last timestamp of the event timeseries
            # First create a list of timestamps and values, and then expand
            pl.when("is_last_start")
            .then(pl.concat_list("floored_interval_start", "floored_interval_end"))
            .otherwise(pl.concat_list("floored_interval_start"))
            .alias("timestamp"),
            pl.when("is_last_start")
            .then(pl.concat_list("NUMCapacity", "NUMCapacity"))
            .otherwise(pl.concat_list("NUMCapacity"))
            .alias("values"),
        )
        .explode("timestamp", "values")
    )

    ts_data = (
        outage_data.select(
            "timestamp",
            "values",
            "unique_event_id",
        )
        .rename(
            {
                "timestamp": "delivery_start",
                "values": "non_usable_capacity",
            }
        )
        # non-unique event * timestamp occurs when the duration of event is 1min. Simply remove duplicates
        .unique(["delivery_start", "unique_event_id"])
        .sort("unique_event_id", "delivery_start")
    )

    # Upsample tp 1min timeseries
    ts_data_expanded = (
        ts_data.sort("delivery_start")
        .upsample(
            time_column="delivery_start",
            every="1m",
            group_by="unique_event_id",
        )
        .select(pl.all().forward_fill())
    )

    # Average 1min outage capacity to 15min for each unique event
    outage_ts_15min = (
        ts_data_expanded.sort("unique_event_id", "delivery_start")
        .group_by_dynamic(
            "delivery_start", every="15m", closed="left", group_by="unique_event_id"
        )
        .agg(pl.col("non_usable_capacity").mean())
    )

    # Sum over all unique events for each 15min
    outage_ts_15min_agg = (
        outage_ts_15min.group_by("delivery_start")
        .agg(pl.col("non_usable_capacity").sum())
        .sort("delivery_start")
    )

    return outage_ts_15min_agg
