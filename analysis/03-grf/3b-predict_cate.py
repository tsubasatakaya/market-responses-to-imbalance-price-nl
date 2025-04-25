import os
import json
import joblib
import polars as pl
from src.model.utils.grf_utils import *

data_save_path = "output/data/grf"
model_save_path = "output/models/grf"

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
# Load model object and train data
########################################################################################################################
directions = ["pos", "neg"]
train_data_all = {
    "pos": {},
    "neg": {},
}

iv_forest_all = {
    "pos": None,
    "neg": None,
}

for direc in directions:
    for df in ["ts", "X", "y", "T", "Z"]:
        train_data_all[direc][df] = pl.read_csv(os.path.join(data_save_path, f"{direc}_{df}.csv"))
    iv_forest_all[direc] = joblib.load(os.path.join(model_save_path, f"iv_forest_{direc}.pkl"))

########################################################################################################################
# Save in-sample CATE
########################################################################################################################
cate_train_data_all = {}
for direc in directions:
    tau_hat, lower, upper = iv_forest_all[direc].predict(train_data_all[direc]["X"], interval=True, alpha=0.05)
    cate_df = create_cate_df(tau_hat, lower, upper)
    cate_train_data_all[direc] = cate_df

cate_train_data_all["pos"].write_csv(os.path.join(data_save_path, f"pos_cate_train_data.csv"))
cate_train_data_all["neg"].write_csv(os.path.join(data_save_path, f"neg_cate_train_data.csv"))

########################################################################################################################
# Estimate CATE by t-1 system imbalance and t-1 imbalance price
########################################################################################################################
cate_tag = "imb_1_isp_1"
vary_continuous_dict_all = {}
imb_isp_cate_test_data_all = {}
vary_continuous_covs = ["imbalance_lag_1_sp", "imbalance_price_lag_1_sp"]
for direc in directions:
    X_orig = train_data_all[direc]["X"]
    vary_continuous_quantile_vals = [
        np.quantile(X_orig[col].to_numpy(), np.arange(0.025, 0.980, 0.05))
        for col in vary_continuous_covs
    ]
    var_1, var_2 = np.meshgrid(*vary_continuous_quantile_vals)
    vary_continuous_dict = {
        col: vals for col, vals in zip(vary_continuous_covs, [var_1, var_2])
    }

    X_test = create_X_test(X_orig, feature_config, vary_continuous_dict,)

    tau_hat, lower, upper = iv_forest_all[direc].predict(X_test, interval=True, alpha=0.05)
    cate_test_data = (create_cate_df(tau_hat, lower, upper)
                      .with_columns(X_test[vary_continuous_covs])
                      )
    vary_continuous_dict_all[direc] = vary_continuous_dict
    imb_isp_cate_test_data_all[direc] = cate_test_data
    cate_test_data.write_csv(os.path.join(data_save_path, f"{direc}_cate_test_data_{cate_tag}_quantile.csv"))