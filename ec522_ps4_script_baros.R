library(BVAR)
library(tidyverse)
library(fpp3)


## Save results with saveRDS() 
## Load saved results with readRDS() 
## Commit the results folder so that the repository holds everything needed to render the write-up 


# ================================ #
# PART 2 
# ================================ #

# 2.1a Load in data 

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019-12-01"))

has_gaps(liquor)


# 2.1b Plot sales with autoplot and log(sales) with gg_season()

  # Autoplot sales 
liquor |> autoplot(sales) +
  labs(
    title = "U.S. Retail Sales: Beer, Wine, and Liquor Stores",
    subtitle = "Not Seasonally Asjusted",
    x = NULL, y = "Millions of Dollars", color = NULL
  ) + 
  theme_minimal()

  # Create log(sales) variable
liquor <- liquor |>
  mutate(
    log_sales = log(sales))


liquor |> gg_season(log_sales) + 
  labs(
    title = "U.S. Retail Log(sales): Beer, Wine, and Liquior Stores",
    subtitle = "Seasonal Plot",
    x = NULL, y = "log(sales)", color = NULL
  ) +
  theme_minimal()

## There appears to be a slight upward trend from January to December with the peak being in December and some small peaks in July and August. Over-time, consumption has increased as well to a higher sales level and following the same trend. 
## The seasonal swings do grow with the model. This suggests that using logs reveals the true rates of changes and sales patterns. 














