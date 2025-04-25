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

def create_X_test(X_orig,
                  feature_config,
                  vary_continuous_dict,
                  dummy_one_covs = [],
                     ):
    """
    Creates test X dataframe

    Parameters
    ----------
    X_orig : pl.DataFrame
       Original feature dataframe
    feature_config : dict
       Dictionary of features used to fit the model
    vary_continuous_dict : dict
       A pair of variable name and values to create X test data from. The values should be output from np.meshgrid()
    dummy_one_covs : list
       A list of dummy covariates that should be set to 1
    """
    fix_continuous_covs = [c for c in list(set(feature_config["continuous_cov"]).intersection(set(X_orig.columns)))
                           if c not in vary_continuous_dict.keys()]
    fix_continuous_vals = {col: get_nearest_quantile_value(X_orig[col].to_numpy(), [0.5])[0]
                           for col in fix_continuous_covs}
    dummy_one_vals = {col: 1 for col in [c for c in X_orig.columns if c in dummy_one_covs]}
    dummy_zero_vals = {col: 0 for col in [c for c in X_orig.columns
                                          if c not in feature_config["continuous_cov"] and c not in dummy_one_covs]}
    dummy_vals = {**dummy_one_vals, **dummy_zero_vals}

    X_test = (pl.from_dict(
        {col: var.flatten() for col, var in vary_continuous_dict.items()})
              .with_columns(
        *[pl.lit(val).alias(col) for col, val in fix_continuous_vals.items()],
        *[pl.lit(val).alias(col) for col, val in dummy_vals.items()],)
              .select(X_orig.columns)
              )

    return X_test