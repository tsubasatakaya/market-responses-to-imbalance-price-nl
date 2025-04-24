"""
Process merit order list (MOL) data from tennet.eu in the following steps:
   1. Round up all volume ticks to multiples of 10 MW
   2. Derive maximum available volume tick for each direction and delivery period
   3. Filter out invalid MOL
"""

import pendulum
import polars as pl


def process_mol(
    start: pendulum.datetime,
    end: pendulum.datetime,
    cont_conflict_tolerance: float = 5,
):
    """
    Process raw merit-order-list (MOL) data in the following steps. To this end, I denote MOL that consists of price bids
    only at certain volume positions such as 100MW, 300MW, and 600MW as "sparse" MOL. After 2022-09-01 (except for
    a brief period shortly after that date), price bids are available at each 10MW interval. I denote this type of MOL
    as "continuous".

    1. Discard rows with volumes less than 10. This is essentially a bid at 1MW position. And then classify all volumes
       into multiples of 10MW. This will affect the maximum price bid of continuous MOL.
    2. Handle invalid MOL data. Sparse MOL is invalid when price for any volume < maximum vol is missing (e.g. price for
       300MW is missing when price for 600MW is available). For continuous MOL, forward-fill missing prices in the order of
       more extreme to less extreme prices.
    3. For continuous MOL, there are a few delivery periods in which the MOL curve is not monotonic. Sometimes the discrepancy
       is as small as a few cents, sometimes quite large. Discard the whole delivery periods if the non-monotonic difference
       is larger in absolute terms than cont_conflict_tolerance.
    """
    mol_data = (
        pl.read_csv(
            "data/merit_order_list_from_tennet_eu.csv",
            columns=["delivery_start", "isp", "volume", "price_down", "price_up"],
        )
        # Rename settlement period (isp: imbalance settlement period)
        .rename({"isp": "sp"})
        # Convert to datetime
        .with_columns(pl.col("delivery_start").str.to_datetime())
        # Filter by datetime range
        .filter(
            pl.col("delivery_start").is_between(
                lower_bound=start, upper_bound=end, closed="left"
            )
        )
    )

    directions = ["up", "down"]

    ####################################################################################################################
    # Round up volume ticks to multiples of 10 MW and get maximum volume for each direction and delivery_start
    ####################################################################################################################
    mol_data = (
        mol_data.filter(pl.col("volume") >= 10)
        .with_columns(pl.col("volume").truediv(10).ceil().mul(10).alias("volume"))
        .group_by("delivery_start", "sp", "volume")
        # Round-up can result in duplicate volumes for the same delivery_start (Either price_up or price_down will be missing)
        # aggregate duplicates into one by taking max (same result with min; the purpose is taking a non-null value)
        .agg(
            [
                pl.max(col).alias(col)
                for col in mol_data.select(
                    pl.exclude("delivery_start", "sp", "volume")
                ).columns
            ]
        )
        .sort("delivery_start", "volume", descending=[False, True])
        # Derive the volume with the maximum non-null price for up and down for each of delivery_start
        .with_columns(
            [
                pl.col(f"price_{direc}")
                .is_not_null()
                .arg_true()
                .first()
                .over("delivery_start")
                .alias(f"max_vol_index_{direc}")
                for direc in directions
            ]
        )
        .with_columns(
            [
                pl.col("volume")
                .gather(pl.col(f"max_vol_index_{direc}"))
                .over("delivery_start")
                .alias(f"max_vol_{direc}")
                for direc in directions
            ]
        )
        .with_columns(pl.len().over("delivery_start").alias("vol_tick_count"))
    )

    ####################################################################################################################
    # Round up volume ticks to multiples of 10 MW and get maximum volume for each direction and delivery_start
    ####################################################################################################################
    # Create filtering mask (vol_tick_count <= 3 -> sparse, otherwise continuous)
    # For sparse, drop if the price for any vol < max_vol is missing
    # For continuous, forward fill after sorting by volume in descending order and drop if there is a
    # discrepancy in monotonicity larger than tolerance level in EUR/MWh
    mol_data = (
        mol_data.sort(["delivery_start", "volume"], descending=[False, True])
        # Handle continuous irregularities
        .with_columns(
            # First forward fill the prices from larger volume to smaller
            [
                pl.when(pl.col("vol_tick_count") > 3)
                .then(
                    pl.col(f"price_{direc}")
                    .fill_null(strategy="forward")
                    .over("delivery_start")
                )
                .otherwise(f"price_{direc}")
                .alias(f"price_{direc}")
                for direc in directions
            ]
        )
        .with_columns(
            # Get the difference between every two adjacent rows
            # if there is a significant difference, that is the discrepancy
            [
                pl.col(f"price_{direc}")
                .diff()
                .over("delivery_start")
                .alias(f"{direc}_diff")
                for direc in directions
            ]
        )
        .with_columns(
            # If the (absolute) discrepancy is larger than cont_conflict_tolerance for continuous MOL, flag it
            pl.when(
                (
                    (pl.col("vol_tick_count") > 3)
                    & (
                        (pl.col("up_diff") > cont_conflict_tolerance)
                        | (pl.col("down_diff") < -cont_conflict_tolerance)
                    )
                )
                .any()
                .over("delivery_start")
            )
            .then(True)
            .otherwise(False)
            .alias("mol_out_sample")
        )
        # Handle sparse irregularities
        .with_columns(
            pl.when(
                (
                    (pl.col("vol_tick_count") <= 3)
                    & (
                        (
                            (pl.col("volume") < pl.col("max_vol_up"))
                            & (pl.col("price_up").is_null())
                        )
                        | (
                            (pl.col("volume") < pl.col("max_vol_down"))
                            & (pl.col("price_down").is_null())
                        )
                    )
                )
                .any()
                .over("delivery_start")
            )
            .then(True)
            .otherwise("mol_out_sample")
            .alias("mol_out_sample")
        )
        .drop("^.*_index_.*$", "^.*_diff$")
    )

    return mol_data


if __name__ == "__main__":
    import polars as pl
    import pendulum

    start = pendulum.datetime(2021, 1, 1, tz="CET").in_timezone("UTC")
    end = pendulum.datetime(2021, 10, 1, tz="CET").in_timezone("UTC")

    data = process_mol(start, end)
