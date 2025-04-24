"""
Create timeseries of day-ahead prices and id1 traded volumes. Source: EPEX SPOT
"""

import pendulum
import polars as pl


def create_wholesale_price_timeseries(
    start: pendulum.datetime,
    end: pendulum.datetime,
):
    # Make full time-series to avoid missing 15min
    data = pl.DataFrame().with_columns(
        delivery_start=pl.datetime_range(
            start=start.in_timezone("UTC"),
            end=end.in_timezone("UTC"),
            interval="15m",
            closed="left",
        )
    )

    # Day-ahead prices
    da_data = (
        pl.read_csv(
            "data/day_ahead_prices_from_epex.csv",
        )
        # Convert to datetime
        .with_columns(pl.col("delivery_start").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("delivery_start").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
    )

    # Intraday indices
    idc_data = (
        pl.read_csv(
            "data/intraday_indices_from_epex.csv",
            columns=["delivery_start", "id1_vol_quarter_hourly"]
        )
        # Convert to datetime
        .with_columns(pl.col("delivery_start").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("delivery_start").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
    )

    merged = (data
              .join(
        da_data, on="delivery_start", how="left", validate="1:1")
              .join(
        idc_data, on="delivery_start", how="left", validate="1:1")
              )

    return merged