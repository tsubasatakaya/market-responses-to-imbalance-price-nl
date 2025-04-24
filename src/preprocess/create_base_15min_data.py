import os
import polars as pl
import pendulum
import holidays

from src.preprocess.balancing.create_balancing_15min_timeseries import create_balancing_15min_timeseries
from src.preprocess.balancing.process_balance_delta import process_balance_delta
from src.preprocess.afrr_energy_prices.process_mol import process_mol
from src.preprocess.afrr_energy_prices.create_afrr_energy_price_proxy import create_afrr_energy_price_proxy
from src.preprocess.generation_unavailability.create_unplanned_outage_timeseries import create_unplanned_outage_15min_timeseries
from src.preprocess.fundamentals.create_forecast_error_timeseries import create_forecast_error_timeseries
from src.preprocess.wholesale_price.create_wholesale_price_timeseries import create_wholesale_price_timeseries

def create_base_15min_data(
    start: pendulum.datetime,
    end: pendulum.datetime,
    save_disc: bool = True):

    nl_holidays = holidays.Netherlands(range(start.year, end.year + 1))

    data = pl.from_dict(
        {
            "delivery_start": pl.datetime_range(
                start=start,
                end=end,
                interval="15m",
                closed="left",
                eager=True,
                time_zone="UTC",
            )
        }
    )

    bal_15min_data = create_balancing_15min_timeseries(
        start=start,
        end=end,
    )
    balance_delta_data = process_balance_delta(
        start=start,
        end=end
    )
    mol_data = process_mol(start, end, cont_conflict_tolerance=5)

    afrr_energy_price_data = create_afrr_energy_price_proxy(
        mol_data=mol_data, bal_data=balance_delta_data,
    )
    unplanned_outage_15min_data = create_unplanned_outage_15min_timeseries(
        start=start,
        end=end,
    )
    forecast_error_data = create_forecast_error_timeseries(
        start=start,
        end=end,
    )
    wholesale_price_data = create_wholesale_price_timeseries(
        start=start,
        end=end
    )

    data = (
        data
        .join(bal_15min_data, on="delivery_start", how="left", validate="1:1")
        .join(afrr_energy_price_data, on="delivery_start", how="left", validate="1:1")
        .join(unplanned_outage_15min_data, on="delivery_start", how="left", validate="1:1")
        .join(forecast_error_data, on="delivery_start", how="left", validate="1:1")
        .join(wholesale_price_data, on="delivery_start", how="left", validate="1:1")
        .with_columns(pl.col("non_usable_capacity").fill_null(0.0))
    )

    data = (
        data
        .with_columns(pl.col("delivery_start").dt.convert_time_zone("CET"))
        # Add time-related fixed effects
        .with_columns(
            pl.col("delivery_start").dt.year().alias("year"),
            pl.col("delivery_start").dt.month().alias("month"),
            pl.col("delivery_start").dt.date()
            .is_in([date for date in nl_holidays])
            .cast(pl.Int64).alias("holiday"),
            pl.col("delivery_start").dt.weekday().alias("dow"),
            pl.col("delivery_start").dt.hour().alias("hour"),
            (pl.col("delivery_start").dt.minute() / 15 + 1)
            .cast(pl.Int64)
            .alias("qh"),
        )
        # Add lags
        .with_columns(
            *[pl.col(col).shift(lag).alias(f"{col}_lag_{lag}_sp")
              for col in ["imbalance_price"]
              for lag in [1, 2, 3, 4, 96]
              ]
        )
    )

    if save_disc:
        os.makedirs("analysis/data", exist_ok=True)
        data.write_csv("analysis/data/base_15min_data.csv")
    else:
        return data