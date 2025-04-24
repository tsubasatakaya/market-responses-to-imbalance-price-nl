"""
Process balance delta data from tennet.eu.
TenneT publishes minutely activation data and quarter-hourly data on settled balancing energy. In theory,
aggregating the former to quarter-hourly should generate identical values, but there are conflicts in many cases.
This script processes these inconsistencies and flags invalid entries. The balance delta data is used to
calculate activation probabilities, which are applied as weights for aFRR price proxies.
"""

import pendulum


def process_afrr(
    data,
    activated_suffix: str,
    balance_delta_suffix: str
):
    """
    Deal with conflicting aFRR volumes and determine in-sample values according to the following steps:

    1. Calculate the difference between aFRR from aggregated balance delta and aFRR from settled balancing energy
    2. Remove rows with an absolute difference larger than 3 standard deviation from the average difference
    3. For 2021-12-01, use values from aggregated balance delta because settled aFRR values are invalid (same
       volume in both directions)
    4. For 2024-05-06, drop all SPs because there is constant -10MW for settled downward aFRR from 16:15 to EOD.
    5. From 2023-10-04 - 2023-10-20 10:45 (exclusive), drop all SPs because balance delta is missing (not clear if the
       market could react as normal).
    """
    data = data.with_columns(
        (
            pl.col(f"afrr_up_{activated_suffix}")
            - pl.col(f"afrr_up_{balance_delta_suffix}")
        ).alias("afrr_up_diff"),
        (
            pl.col(f"afrr_down_{activated_suffix}")
            - pl.col(f"afrr_down_{balance_delta_suffix}")
        ).alias("afrr_down_diff"),
    )
    up_diff_thresh = max(
        data.select(
            (pl.mean("afrr_up_diff") - 3 * pl.std("afrr_up_diff")).abs()
        ).item(),
        data.select(
            (pl.mean("afrr_up_diff") + 3 * pl.std("afrr_up_diff")).abs()
        ).item(),
    )
    down_diff_thresh = max(
        data.select(
            (pl.mean("afrr_down_diff") - 3 * pl.std("afrr_down_diff")).abs()
        ).item(),
        data.select(
            (pl.mean("afrr_down_diff") + 3 * pl.std("afrr_down_diff")).abs()
        ).item(),
    )

    data = (
        data.with_columns(
            pl.when(
                (
                    (pl.col("afrr_up_diff").abs() > up_diff_thresh)
                    | (pl.col("afrr_down_diff").abs() > down_diff_thresh)
                )
                | pl.col("afrr_up_diff").is_null()
                | pl.col("afrr_down_diff").is_null()
            )
            .then(None)
            .otherwise(pl.lit("activated_balancing_energy"))
            .alias("in_sample_afrr_source")
        )
        .with_columns(
            # For 2021-12-01, use values from aggregated balance delta
            pl.when(
                pl.col("delivery_day_cet")
                == pendulum.date(
                    2021,
                    12,
                    1,
                )
            )
            .then(pl.lit("balance_delta"))
            .otherwise(pl.col("in_sample_afrr_source"))
            .alias("in_sample_afrr_source")
        )
        .with_columns(
            # For 2024-05-06, drop all SPs
            pl.when(pl.col("delivery_day_cet") == pendulum.date(2024, 5, 6))
            .then(None)
            .otherwise(pl.col("in_sample_afrr_source"))
            .alias("in_sample_afrr_source")
        )
        .with_columns(
            # From 2023-10-04 - 2023-10-20 10:45 (exclusive), drop all SPs
            pl.when(
                pl.col("delivery_start").is_between(
                    pendulum.datetime(2023, 10, 4, tz="CET").in_timezone("UTC"),
                    pendulum.datetime(2023, 10, 20, 10, 45, tz="CET").in_timezone(
                        "UTC"
                    ),
                    closed="left",
                )
            )
            .then(None)
            .otherwise(pl.col("in_sample_afrr_source"))
            .alias("in_sample_afrr_source")
        )
    )

    return data.drop("afrr_up_diff", "afrr_down_diff")


def process_balance_delta(
    start: pendulum.datetime,
    end: pendulum.datetime,
):
    # Make full time-series to avoid missing 15min (i.e. 2023-03-30)
    data = pl.DataFrame().with_columns(
        delivery_start=pl.datetime_range(
            start=start.in_timezone("UTC"),
            end=end.in_timezone("UTC"),
            interval="15m",
            closed="left",
        )
    )

    # Load settled balancing energy data
    be_data = (
        pl.read_csv(
            "data/settled_balancing_energy_from_tennet_eu.csv",
            columns=["delivery_start", "afrr_up_settled", "afrr_down_settled"],
        )
        # Convert to datetime
        .with_columns(pl.col("delivery_start").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("delivery_start").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
        # Convert kwh to MW
        .with_columns((pl.exclude("delivery_start") * 4 / 1000).round(3))
    )

    balance_delta_data = (
        pl.read_csv(
            "data/balance_delta_from_tennet_eu.csv",
            columns=["datetime", "afrr_up_balance_delta", "afrr_down_balance_delta"],
        )
        # Convert to datetime
        .with_columns(pl.col("datetime").str.to_datetime())
        # Create delivery_start for 15min settlement periods
        .with_columns(
            pl.col("datetime").dt.truncate("15m").alias("delivery_start"),
        )
        # Change sign of downward aFRR
        .with_columns(
            pl.col("afrr_down_balance_delta").mul(-1).alias("afrr_down_balance_delta")
        )
    )

    merged_15min_data = (
        data.join(be_data, on="delivery_start", how="left", validate="1:1")
        .join(
            (
                balance_delta_data
                # Aggregate to 15min
                .group_by_dynamic("delivery_start", every="15m", closed="left").agg(
                    pl.exclude("datetime").mean().round(3),
                )
            ),
            on="delivery_start",
            how="left",
            validate="1:1",
        )
        .with_columns(
            pl.col("delivery_start")
            .dt.convert_time_zone("CET")
            .dt.date()
            .alias("delivery_day_cet")
        )
    )

    # Process aFRR
    # Drop settlement periods with significantly inconsistent aFRR volumes between aggregated balance delta and
    # settled balancing energy. Significant difference is defined as mean difference +/- 3 * SE (symmetric
    # threshold is applied). Drop the entire day if many SPs have significant differences
    merged = process_afrr(
        merged_15min_data,
        activated_suffix="settled",
        balance_delta_suffix="balance_delta",
    )

    # Processed balance delta
    balance_delta_processed = balance_delta_data.join(
        merged["delivery_start", "in_sample_afrr_source"],
        on="delivery_start",
        how="left",
        validate="m:1",
    )

    return balance_delta_processed


if __name__ == "__main__":
    import polars as pl
    import pendulum

    start = pendulum.datetime(2021, 1, 1, tz="CET").in_timezone("UTC")
    end = pendulum.datetime(2024, 10, 1, tz="CET").in_timezone("UTC")

    data = process_balance_delta(start, end)
