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
# 2.6 Working with Dates in a tsibble
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent) #NOTE: filtering by date

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month)) #NOTE: extracting parts of a date

can_gas_recent |>
  as_tibble() |> 
  group_by(month) |>
  summarize(avg_volume = mean(Volume)) #NOTE: December has highest avg production, June has the lowest.

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume) 
#QUESTION 2.6d: We sum the monthly production values rather than mean because gas production is a flow. We want to know how much was produced in the whole quarter which is given by the sum of three months; mean would give average production per month within the quarter. Summing mutes the seasonal pattern but doesn't remove it entirely. We can still observe seasonality in the graph with the repeating occilations.

# 2.7 Plotting with autoplot()
retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
# QUESTION 2.7b: The RSAFSNA series has a clear pattern, spiking and dropping sharply at the beginning of each month then returning to a relatively constant level before spiking and dropping again. The SA series does not have this pattern. Both series follow the same path in the long run including recessions like 2008 and 2020. The SA series follows a more linear path, though.
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    subtitle = "Monthly, January 1960 to February 2005",
    x = "Month", y = "Billions of Cubic Meters"
  ) +
  theme_minimal()

can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Canadian Natural Gas Production",
    subtitle = "Monthly, with smoothed long-run trend",
    x = "Month",
    y = "Billions of cubic meters"
  ) +
  theme_minimal()
# QUESTION 2.7d: I chose to add the geom_smooth() function because the monthly series has pretty dense seasonal swings (especially surrounding Jan 2000), and the smooth line makes it easier for a reader to see the long-run production trend.

