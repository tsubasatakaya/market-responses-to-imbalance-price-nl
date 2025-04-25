# market-responses-to-imbalance-price-nl

This repository contains the code to reproduce the analysis conducted for my master's thesis, Market responses to the imbalance settlement price:
Empirical evidence from the Dutch electricity market (2025), submitted for partial fulfillment of the requirements for the degree of Master of Public Policy at the Hertie School.  

The main folder is `analysis`, where all the scripts relevant to the analysis are stored. Each script does the following:  
- `00-prepare.py`: Process all the raw data and combine them into one base data (Python).
- `01-descriptives.R`: Create a summary statistics table and a scatterplot of observed system imbalance and imbalance price.
