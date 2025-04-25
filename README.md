# market-responses-to-imbalance-price-nl

This repository contains the code to reproduce the analysis conducted for my master's thesis, Market responses to the imbalance settlement price:
Empirical evidence from the Dutch electricity market (2025), submitted for partial fulfillment of the requirements for the degree of Master of Public Policy at the Hertie School.  

The main folder is `analysis`, where all the scripts relevant to the analysis are stored. Each script does the following:  
- `00-prepare.py`: Process all the raw data and combine them into one base data (Python). Several destinations for figures, tables, model objects, and data are also defined.
- `01-descriptives.R`: Create a summary statistics table and a scatterplot of observed system imbalance and imbalance price (R).
- `02-iv_main.R`: Implement two-stage least squares (2SLS) for the main results (R).
- `03-grf`:
     1. `3a-fit_grf.py`: Fit generalized random forests (GRF) using `econml` package (Python).
     2. `3b-predict_cate.py`: Predict in-sample conditional average treatment effects (CATEs) and marginal CATEs by t-1 system imbalance and t-1 imbalance price (R).
     3. `3c-plot_cate.py`: Visualize the results from 2 (R).
     4. `3d-plot_variable_importance.R`: Plot variable importance of the fitted GRF (R).
- `04-robustness_check.R`: Conduct robustness checks with additional lags (R).
- `05-intraday_iv.R`: Run 2SLS on the ID traded volumes (R).
- `06-other_appendix_plots.R`: Generate other supplementary plots, namely the trend of ID1 traded volumes and partial autocorrelation functions of key variables (R).
- `07-afrr_proxy_example.R`: Create plots to explain how the aFRR price proxy is constructed.
