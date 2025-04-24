"""
Create forecast error timeseries. The data source is as follows:
  - Solar: private data source
  - Wind: ENTSO-E Transparency
  - Load: ENTSO-E Transparency

Forecast error is defined as (day-ahead forecasts) - (actual values)
"""

import pendulum
import polars as pl


def create_forecast_error_timeseries(
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

    # Solar forecast errors
    solar_data = (
        pl.read_csv(
            "data/solar_historical_data_from_private.csv",
            columns=["delivery_start", "solar_da", "solar_actual"],
        )
        # Convert to datetime
        .with_columns(pl.col("delivery_start").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("delivery_start").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
        .with_columns(
            (pl.col("solar_da") - pl.col("solar_actual")).alias("solar_forecast_error")
        )
        .select("delivery_start", "solar_forecast_error")
    )

    # Wind and load forecast errors
    wind_load_data = (
        pl.read_csv(
            "data/wind_load_historical_data_from_entsoe.csv",
        )
        # Convert to datetime
        .with_columns(pl.col("delivery_start").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("delivery_start").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
        .with_columns(
            (pl.col("wind_da") - pl.col("wind_actual")).alias("wind_forecast_error"),
            (pl.col("load_da") - pl.col("load_actual")).alias("load_forecast_error"),
        )
        .select("delivery_start", "^.*error$")
    )

    merged = (data
              .join(
        solar_data, on="delivery_start", how="left", validate="1:1")
              .join(
        wind_load_data, on="delivery_start", how="left", validate="1:1")
              )

    return merged
