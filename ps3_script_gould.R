# 2.2 Read and Inspect
library(tidyverse)
retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)
# 2.3 Reshape date to long format
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")
#inspecting new tibble
glimpse(retail_raw)
nrow(retail_raw_long)
nrow(retail_raw_long) / nrow(retail_raw)
head(retail_raw_long) # NOTE: long version stacks the series into one column and adds a label column versus wide whichc has one column per series.
# 2.4 Building tsibble


