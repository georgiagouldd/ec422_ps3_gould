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
head(retail_raw_long) # NOTE: long version stacks the series into one column and adds a label column versus wide which has one column per series.
# 2.4 Building tsibble
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts) #NOTE: no gaps both ran false
# 2.5 Canadian Gas Production
head(canadian_gas) # Comparison: only displays yr, month, and volume. No seriesid column
key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

