"""
Create quarter-hourly timeseries from the following balancing-related data:
   1. Imbalance settlement price data. Source: tennet.eu
   2. Settled imbalance volumes data. Source: tennet.eu
"""

import pendulum
import polars as pl


def create_balancing_15min_timeseries(
    start: pendulum.datetime,
    end: pendulum.datetime,
):
    # Make full time-series to avoid missing 15min
    data = pl.DataFrame().with_columns(
        delivery_start=pl.datetime_range(
            start=start,
            end=end,
            interval="15m",
            closed="left",
        )
    )

    isp_data = (
        pl.read_csv(
            "data/settlement_prices_from_tennet_eu.csv",
            columns=["delivery_start", "regulation_state", "shortage_imbalance_price"],
        )
        .rename({"shortage_imbalance_price": "imbalance_price"})
        # Convert to datetime
        .with_columns(pl.col("delivery_start").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("delivery_start").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
    )

    imb_data = (
        pl.read_csv(
            "data/settled_imbalance_volumes_from_tennet_eu.csv",
            columns=["delivery_start", "surplus", "shortage"],
        )
        # Convert to datetime
        .with_columns(pl.col("delivery_start").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("delivery_start").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
        # Create imbalance variable from surplus and shortage (positive = shortage)
        .with_columns(
            pl.when(
                # sign convention changes at this time
                pl.col("delivery_start")
                >= pendulum.datetime(2022, 7, 16, 0, 15, tz="CET").in_timezone("UTC")
            )
            .then(pl.col("shortage") - pl.col("surplus"))
            .otherwise(pl.col("surplus") - pl.col("shortage"))
            .alias("imbalance")
        )
        .with_columns(
            # Convert kwh to MW
            (pl.col("imbalance") * 4 / 1000).round(2).alias("imbalance")
        )
        .select("delivery_start", "imbalance")
    )

    merged = (
        data.join(
            isp_data,
            on="delivery_start",
            how="left",
            validate="1:1",
        )
        .join(
            imb_data,
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

    return merged
