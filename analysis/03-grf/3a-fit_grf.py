import os
import joblib
import polars as pl
import polars.selectors as cs
import pandas as pd
from econml.grf import CausalIVForest

data_path = "analysis/data"
data_save_path = "output/data/grf"
model_save_path = "output/models/grf"
os.makedirs(data_save_path, exist_ok=True)
os.makedirs(model_save_path, exist_ok=True)

########################################################################################################################
# Feature configuration
########################################################################################################################
feature_config = {
    "dv": "imbalance",
    "endo": "imbalance_price",
    "iv": {
        "pos": "up_weighted_price",
        "neg": "down_weighted_price",
    },
    "continuous_cov": [
        # Common cov
        "non_usable_capacity",
        "da_price",
        "max_up_price",
        "min_down_price",

        # Forecast error
        "solar_forecast_error",
        "wind_forecast_error",
        "load_forecast_error",

        # Lags
        "imbalance_price_lag_1_sp",
        "imbalance_lag_1_sp",
        "imbalance_price_lag_2_sp",
        "imbalance_lag_2_sp",
        "imbalance_price_lag_3_sp",
        "imbalance_lag_3_sp",
        "imbalance_price_lag_4_sp",
        "imbalance_lag_4_sp",
        "imbalance_price_lag_96_sp",
        "imbalance_lag_96_sp",
    ],
    "dummy_cov": [
        "holiday",
        "regulation_state_2_dummy_lag_3_sp",
    ],
    "categorical_cov": [
        "month",
        "dow",
        "hour",
        "qh",
    ]
}

########################################################################################################################
# Data preparation
########################################################################################################################
float_schema = {
    cs.exclude(
        "delivery_start",
        "regulation_state",
        "year",
        "month",
        "dow",
        "hour",
        "qh",
    ): pl.Float64
}
base_data = pl.read_csv(os.path.join(data_path, "base_15min_data.csv"),
                        try_parse_dates=True,).cast(float_schema)

lag_cols = [
    "regulation_state_2_dummy",
    "imbalance",
]

imb_data = (base_data
    # Remove outliers
            .with_columns(
    pl.when(
        (pl.col("imbalance") < pl.col("imbalance").mean() - pl.col("imbalance").std() * 3) |
        (pl.col("imbalance") > pl.col("imbalance").mean() + pl.col("imbalance").std() * 3)
    )
    .then(None)
    .otherwise("imbalance")
    .alias("imbalance"))

    # Add regulation state dummies
            .with_columns(
    pl.when(
        pl.col("regulation_state") == 2
    )
    .then(pl.lit(1))
    .otherwise(pl.lit(0))
    .alias("regulation_state_2_dummy"),
    pl.when(
        pl.col("regulation_state") == 1
    )
    .then(pl.lit(1))
    .otherwise(pl.lit(0))
    .alias("regulation_state_pos_dummy"))

    # Filter mismatched imbalance and regulation_state
            .filter(
    ((pl.col("regulation_state_pos_dummy") == 1) & (pl.col("imbalance") >= 0))
     | ((pl.col("regulation_state_pos_dummy") == 0) & (pl.col("imbalance") < 0)))

    # Add lags
            .with_columns(
    *[pl.col(col).shift(l).alias(f"{col}_lag_{l}_sp")
      for col in lag_cols for l in [1, 2, 3, 4, 96]])

    # Add dummies
            .to_dummies(
    feature_config["categorical_cov"], separator="_", drop_first=True,)
            )

cat_dummy_cov = (imb_data
                 .select([f"^{col}_.*$" for col in feature_config["categorical_cov"]])
                 .columns
                 )
all_cov = feature_config["continuous_cov"] + feature_config["dummy_cov"] + cat_dummy_cov

# Remove reg state = 0 or 2
# Remove rows with any null
imb_data = (imb_data
            .filter(
    ~pl.col("regulation_state").is_in([0, 2]))
            .drop_nulls([feature_config["dv"], feature_config["endo"]] + list(feature_config["iv"].values()) + all_cov)
            )

########################################################################################################################
# Fit model
########################################################################################################################
grf_models = {
    "pos": CausalIVForest(criterion="het", n_estimators=2000,
                           honest=True, verbose=0,
                           n_jobs=-1, random_state=123),
    "neg": CausalIVForest(criterion="het", n_estimators=2000,
                           honest=True, verbose=0,
                           n_jobs=-1, random_state=123),
}

for direc, model in grf_models.items():
    if direc == "pos":
        data_direc = (imb_data.filter(pl.col("regulation_state_pos_dummy") == 1))
        excl_cov = ["min_down_price"]
        all_cov_direc = [c for c in all_cov if c not in excl_cov]
    else:
        data_direc = imb_data.filter(pl.col("regulation_state_pos_dummy") == 0)
        excl_cov = ["max_up_price"]
        all_cov_direc = [c for c in all_cov if c not in excl_cov]

    ts = data_direc.select("delivery_start")
    X = data_direc[all_cov_direc]
    y = data_direc[feature_config["dv"]]
    T = data_direc[feature_config["endo"]]
    Z = data_direc[feature_config["iv"][direc]]

    print(f"Fitting GRF for {direc}")
    model.fit(X, T, y, Z=Z)
    varimp_df = pd.DataFrame(
        {"variable": X.columns,
         "var_imp": model.feature_importances_,}
    )

    joblib.dump(model, os.path.join(model_save_path, f"iv_forest_{direc}.pkl"))
    varimp_df.to_csv(os.path.join(data_save_path, f"{direc}_var_imp.csv"), index=False)
    ts.write_csv(os.path.join(data_save_path, f"{direc}_ts.csv"))
    X.write_csv(os.path.join(data_save_path, f"{direc}_X.csv"))
    y.to_frame().write_csv(os.path.join(data_save_path, f"{direc}_y.csv"))
    T.to_frame().write_csv(os.path.join(data_save_path, f"{direc}_T.csv"))
    Z.to_frame().write_csv(os.path.join(data_save_path, f"{direc}_Z.csv"))
