packages <-  c(
  "broom", 
  "fastDummies",
  "fixest", 
  "forcats",
  "ggplot2",
  "ggpointdensity",
  "glue",
  "grf",
  "gridExtra",
  "gt",
  "modelsummary",
  "scales",
  "see",
  "tidytext",
  "tidyverse",
  "viridis",
  "vroom",
  "zoo")

package.check <- lapply(
  packages,
  FUN <-  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

data_path <- "analysis/data"