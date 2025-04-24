"""
Create aFRR energy price proxy from MOL data and balance delta data in the following steps:
   1. Categorize activated aFRR volumes to 10 MW ticks
   2. Derive activation probabilities for discrete and continuous categories
   3. Calculate weighted sum of aFRR prices for each delivery start, weighted by activation probabilities
      (separately for discrete and continuous)
"""

import pendulum
import numpy as np
import pandas as pd
import polars as pl


def create_afrr_energy_price_proxy(
    mol_data: pl.DataFrame,
    bal_data: pl.DataFrame,
):
    # Change the sign of afrr_down of balance_delta because the following code assumes nonnegative volume
    bal_data = bal_data.with_columns(pl.col("afrr_down_balance_delta") * (-1)).filter(
        pl.col("in_sample_afrr_source").is_not_null()
    )

    discrete_vol_breaks = sorted(
        [
            0,
            100,
            300,
            np.ceil(bal_data.select("afrr_down_balance_delta").max().item() / 10) * 10
            + 10,
        ]
    )
    cont_vol_breaks = sorted(
        np.arange(
            0,
            np.ceil(bal_data.select("afrr_down_balance_delta").max().item() / 10) * 10
            + 10,
            10,
        ).tolist()
    )

    ####################################################################################################################
    # Categorize activated aFRR volumes
    ####################################################################################################################
    # discrete categorization: (0, 100] -> 100, (100, 300] -> 300, (300, Inf] -> 600 (same for downreg)
    # discrete categorization: (0, 10] -> 10, (10, 20] -> 20 ... (1030, 1040] -> 1040 (same for downreg)

    # Define directions and types
    directions = ["up", "down"]

    # Convert to pandas dataframe to use pd.cut
    bal_data = bal_data.to_pandas()
    for cat_type, breaks in zip(
        ["discrete", "continuous"], [discrete_vol_breaks, cont_vol_breaks]
    ):
        if cat_type == "discrete":
            labels = breaks[1:-1] + [600]
        elif cat_type == "continuous":
            labels = breaks[1:]
        for direc in directions:
            bal_data[f"{direc}_{cat_type}_cat"] = np.where(
                bal_data[f"afrr_{direc}_balance_delta"] > 0,
                pd.cut(
                    bal_data[f"afrr_{direc}_balance_delta"],
                    bins=breaks,
                    labels=labels,
                    right=True,
                ),
                0,
            )
    # Convert back to polars dataframe
    bal_data = pl.from_pandas(bal_data)

    ####################################################################################################################
    # Derive activation probabilities for discrete and continuous categories
    ####################################################################################################################
    # Discrete
    # Calculate activation probabilities for each category as relative frequencies
    discrete_weights = []
    for direc in directions:
        discrete_weights.append(
            (
                bal_data[f"{direc}_discrete_cat"]
                .value_counts(normalize=True)
                .rename(
                    {
                        f"{direc}_discrete_cat": "volume",
                        "proportion": f"{direc}_discrete_activated_prob",
                    }
                )
            )
        )

    # Calculate cumulative activation probabilities from higher volume to lower volume
    discrete_weights = (
        pl.concat(discrete_weights, how="align")
        .with_columns(pl.col("volume").cast(pl.Float64))
        .sort("volume", descending=True)
        .with_columns(
            *[
                pl.col(f"{direc}_discrete_activated_prob")
                .cum_sum()
                .alias(f"{direc}_discrete_activated_cum_prob")
                for direc in directions
            ],
        )
        .filter(pl.col("volume") != 0.0)
    )

    # Continuous
    # Calculate activation probabilities for each category as relative frequencies
    continuous_weights = []
    for direc in directions:
        continuous_weights.append(
            (
                bal_data[f"{direc}_continuous_cat"]
                .value_counts(normalize=True)
                .rename(
                    {
                        f"{direc}_continuous_cat": "volume",
                        "proportion": f"{direc}_continuous_activated_prob",
                    }
                )
            )
        )

    # Calculate cumulative activation probabilities from higher volume to lower volume
    continuous_weights = (
        pl.concat(continuous_weights, how="align")
        .fill_null(0.0)  # null = 0% activation probability
        .with_columns(pl.col("volume").cast(pl.Float64))
        .sort("volume", descending=True)
        .with_columns(
            *[
                pl.col(f"{direc}_continuous_activated_prob")
                .cum_sum()
                .alias(f"{direc}_continuous_activated_cum_prob")
                for direc in directions
            ],
        )
        .filter(pl.col("volume") != 0.0)
    )

    # Merge
    weights_data = discrete_weights.join(
        continuous_weights, on="volume", how="full", coalesce=True
    )

    ####################################################################################################################
    # Create aggregate aFRR prices weighted by discrete activation probabilities
    ####################################################################################################################
    mol_data_discrete = (
        mol_data.join(
            weights_data.select("volume", "^.*discrete.*$"),
            on="volume",
            how="left",
            validate="m:1",
        )
        # Keep only 100, 300, 600 MW volume ticks (relevant after 2022-09-01)
        .drop_nulls(["up_discrete_activated_prob", "down_discrete_activated_prob"])
        .sort(["delivery_start", "volume"], descending=[False, True])
        .filter(
            # Apply filter
            ~pl.col("mol_out_sample")
        )
    )

    discrete_weighted_prices = (
        mol_data_discrete.with_columns(
            # If volume < max_vol, apply original weights
            # otherwise cumulative weights (for volume > max_vol, the product must be null)
            [
                pl.when(pl.col("volume") < pl.col(f"max_vol_{direc}"))
                .then(pl.col(f"{direc}_discrete_activated_prob"))
                .when(pl.col("volume") == pl.col(f"max_vol_{direc}"))
                .then(pl.col(f"{direc}_discrete_activated_cum_prob"))
                .otherwise(None)
                .alias(f"{direc}_discrete_weight")
                for direc in directions
            ]
        )
        .group_by("delivery_start")
        .agg(
            *[
                (pl.col(f"price_{direc}") * pl.col(f"{direc}_discrete_weight"))
                .sum()
                .alias(f"{direc}_discrete_weighted_price")
                for direc in directions
            ],
            pl.max("price_up").alias("max_up_price_discrete"),
            pl.min("price_down").alias("min_down_price_discrete"),
        )
    )

    ####################################################################################################################
    # Create aggregate aFRR prices weighted by continuous (10 MW interval) activation probabilities (from 2022-09-01)
    ####################################################################################################################
    mol_data_continuous = mol_data.filter(
        pl.col("delivery_start")
        >= pendulum.datetime(2022, 9, 1, tz="CET").in_timezone("UTC")
    )

    # Expand volumes of mol_data into the unique volumes of continuous_afrr_weights
    # to assign correct activation probabilities from continuous_afrr_weights
    # This step is necessary to take missing activation probabilities into account
    # so that the total activation probability sums up to the same probability for all settlement periods
    mol_data_continuous = (
        mol_data_continuous.select("delivery_start")
        .unique()
        # create a unique pair of delivery_start and volume with nonzero activation probability
        .join(
            pl.DataFrame({"volume": weights_data["volume"].unique().to_list()}),
            how="cross",
        )
        # expand the original mol_data to each delivery_start * volume pair
        .join(
            mol_data_continuous,
            on=["delivery_start", "volume"],
            how="left",
            validate="1:1",
        )
        # join weights
        .join(
            weights_data.select("volume", "^.*continuous.*$"),
            on="volume",
            how="left",
            validate="m:1",
        )
        .filter(
            # Filter out volumes above max(max_vol_up, max_vol_down)
            pl.col("volume") <= pl.max_horizontal("max_vol_up", "max_vol_down")
        )
        .sort(["delivery_start", "volume"], descending=[False, True])
    )

    continuous_weighted_prices = (
        mol_data_continuous.with_columns(
            [
                pl.when(pl.col("volume") < pl.col(f"max_vol_{direc}"))
                .then(pl.col(f"{direc}_continuous_activated_prob"))
                .when(pl.col("volume") == pl.col(f"max_vol_{direc}"))
                .then(pl.col(f"{direc}_continuous_activated_cum_prob"))
                .otherwise(None)
                .alias(f"{direc}_continuous_weight")
                for direc in directions
            ]
        )
        .group_by("delivery_start")
        .agg(
            *[
                (pl.col(f"price_{direc}") * pl.col(f"{direc}_continuous_weight"))
                .sum()
                .alias(f"{direc}_continuous_weighted_price")
                for direc in directions
            ],
            pl.max("price_up").alias("max_up_price_continuous"),
            pl.min("price_down").alias("min_down_price_continuous"),
        )
    )

    afrr_energy_price_data = (
        discrete_weighted_prices.join(
            continuous_weighted_prices,
            on="delivery_start",
            how="full",
            validate="1:1",
            coalesce=True,
        )
        .with_columns(
            # Combine discrete (before 2022-09-01) and continuous (after 2022-09-01) weighted price
            *[
                pl.when(
                    pl.col("delivery_start")
                    < pendulum.datetime(2022, 9, 1, tz="CET").in_timezone("UTC")
                )
                .then(f"{direc}_discrete_weighted_price")
                .otherwise(f"{direc}_continuous_weighted_price")
                .alias(f"{direc}_weighted_price")
                for direc in directions
            ]
        )
        .with_columns(
            *[
                pl.when(
                    pl.col("delivery_start")
                    < pendulum.datetime(2022, 9, 1, tz="CET").in_timezone("UTC")
                )
                .then(f"{stat}_{direc}_price_discrete")
                .otherwise(f"{stat}_{direc}_price_continuous")
                .alias(f"{stat}_{direc}_price")
                for stat, direc in zip(["max", "min"], ["up", "down"])
            ]
        )
        .select(
            "delivery_start",
            "up_weighted_price",
            "down_weighted_price",
            "max_up_price",
            "min_down_price",
        )
        .sort("delivery_start")
    )
    # Save weights data for Appendix figures
    weights_data.write_csv("output/data/afrr_activation_probability_data.csv")
    return afrr_energy_price_data
