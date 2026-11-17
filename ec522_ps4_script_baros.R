library(BVAR)
library(tidyverse)
library(fpp3)


## Save results with saveRDS() 
## Load saved results with readRDS() 
## Commit the results folder so that the repository holds everything needed to render the write-up 


# ================================ #
# PART 2 
# ================================ #

# 2.1 The Data
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

# 2.2 Fit Both Models 
# 2.2a ARIMA and ETS
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

## For the specifications for the ARIMA model were ARIMA(1,0,2)(2,1,1)[12] with drift. This tells us there was a seasonal difference at a lag of 12, indicating the series was differenced once against the same month from the previous year. The constant 0.0036, and total differencing d+D=1, this indicates a drift term. This indicates the year-over-year growth is trending upward over time.
## The three letters indicate a Holt-Winters Additive model with additive error, additive trend, and additive seasonality. 
    ## Additive error means random forecast errors are added directly to predicted level, trend, and seasonal components with assumed $\sigma^2$ = 0.0007 across the log(sales) series. 
    ## Additive trend means the slope is added to the level at each time step. We see that log(sales) is increasing at a constant amount each period, the original sales variable is increasing by a constant percentage over time. The smoothing parameter $\beta$ = 0.0076 shows that the slope changes very slowly and remains nearly constant.
    ## Additive seasonality meand the seasonal effects are added to the level and trend, meaning there is a constant magnitude on log(sales). $\gamma$ = 0.0002 indicates the seasonal pattern is practically fixed over time.

# 2.2b 
liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

## The high lb_stat means the model failed to capture all the underlying patterns, therefore the residuals do not reflect white noise. 

# 2.3 The Horse Race 
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

## 

# 2.3b one-month ahead forecast 
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

## 

