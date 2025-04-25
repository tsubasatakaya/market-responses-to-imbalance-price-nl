import numpy as np
import polars as pl

def get_nearest_value(x, values):
    nearest_vals = values[np.argmin(np.abs(values - x))]
    return nearest_vals

def get_nearest_quantile_value(values, quantiles):
    quantile_vals = np.quantile(values, quantiles)
    nearest_vals = [values[np.argmin(np.abs(values - q))] for q in quantile_vals]
    return np.unique(nearest_vals)

def create_cate_df(tau_hat, lower, upper):
    cate_df = pl.from_dict(
        {"tau_hat": pl.Series(tau_hat.squeeze()),
         "lower": pl.Series(lower.squeeze()),
         "upper": pl.Series(upper.squeeze()),
         }
    )
    return cate_df