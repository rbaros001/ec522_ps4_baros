library(BVAR)
library(tidyverse)
library(fpp3)


## Save results with saveRDS() 
## Load saved results with readRDS() 
## Commit the results folder so that the repository holds everything needed to render the write-up 


# ===================================================== #
# PART 2 - A BUSINESS FORECAST HORSE RACE: ARIMA VS ETS
# ===================================================== #

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
    ## Additive seasonality mean the seasonal effects are added to the level and trend, meaning there is a constant magnitude on log(sales). $\gamma$ = 0.0002 indicates the seasonal pattern is practically fixed over time.

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

## There are 216 different forecast origins in this TS-CV. 

# 2.3b one-month ahead forecast 
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

library(knitr)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  select(.model, RMSE, MAE) |>
  knitr::kable(digits = 2)


## The first and last forecasts for this are January 2002 to December 2019. The model that forecasts the most accurately is the ARIMA model. The ARIMA model wins for both the RMSE and MAE. 

# 2.4 Are the differences significant?
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

## The DM statistic is -2.9062 with a p-value of 0.004042. We can reject the null hypothesis that arima and ets have euqual froecast accuracy. The DM test statistic tells us that the ARIMA model produces significantly more accurate one-step ahead forecasts. 


# ================================ #
# PART 3 - FORECAST COMBINATION
# ================================ #

# 3a. Forecast Combination
liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

library(fable)
library(dplyr)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  select(Model = .model, RMSE, MAE) |>
  arrange(RMSE) |>
  kable(
    digits = 2,
    format.args = list(big.mark = ","),
    caption = "Forecast Accuracy Comparison"
  )

## Combo beats ETS but slightly falls short of ARIMA for both RMSE and MSE

# 3b. Correlation between ARIMA and ETS forecast errors 
cor(liquor_errs$arima, liquor_errs$ets)

## The correlation between the forecast errors report to be 0.8792931. Since the combination gives equal weights to both models, the ETS has larger RMSE and MAE with increases the errors of the combination. The DM statistic tells us the ARIMA model is significantly more accurate than the ETS model


# ==================================== #
# PART 4 - A MEDIUM-SCALE VAR VS. BVAR
# ==================================== #

# 4.2a load FRED-QD data
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

# 4.2b Build the series 
growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

# 4c. ts-cv with first 120 quarters training sample 
N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

range(qd$date, na.rm = TRUE)
# 1960-03-01 to 2019-12-01

## The date range of the first and last forecast is January 03, 1960 to January 10, 2019. The expanding window generates 120 forecasts per variable with a total of 960 forecasts. 

# 4.2 OLS VAR

var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

## VAR(1) forecasts better for all variables except for inflation rate (infl). The smaller forecast may be better because there is less noise and avoids out of sample estimation error and overfitting. 

# 4.3 BVAR
# 4.2a
bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")
fc_bvar_03 <- readRDS("~/ec522_ps4_baros/results/fc_bvar_03.rds")

# 4.3b
set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")
var_results <- readRDS("~/ec522_ps4_baros/results/var_results.rds")

var_results |> knitr::kable(digits = 3)

## The BVAR(4) with $\lambda$ = 0.3 has lower values compared to OLS VAR(4). The BVAR(4) adds a prior distribution which shrinks estimated parameters towards simpler baselines which reduces overfitting. Compared to VAR(1), the BVAR(4) with $\lambda$ = 0.3 estimates are much higher. This is most lively due to more noise from BVAR(4) since there are more time lags. VAR(1) has fewer degrees of freedom to estimate which keeps the forecast variance low. With BVAR(4) and $\lambda$ = 0.05, this tells the model could be suffering from over shrinkage causing the model to ignore real dynamic patterns.
## The RMSE for VAR(1) may not be a fair estimate of how accurately it will forecast in the future because the RMSE is still relatively large. 

# 4.3c GDP growth DM test 
e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)
# DM = 2.1366, p-value = 0.03468

  # unrate DM test 
u_var4 <- actual[, "unrate"] - fc_var4[, 1]
u_bvar <- actual[, "unrate"] - fc_bvar_03[, 1]

forecast::dm.test(u_var4, u_bvar, h = 1, power = 2)
# DM = 0.91508, p-value = 0.362

## The DM test statistic for GDP is statistically significant and tells us that the BVAR(4) outperforms and is more statistically accurate to the VAR(4) model. The DM test statistic for unemployment rate was not statistically significant between the two models which signals that the models give roughly the same forecast performance.
## The DM test statistics for GDP and Unemployment rate tell us that VAR(1) could still be the best option. More comparisons would need to be run to confidently conclude this.

# ==================================== #
# PART 5 - A FACTOR MODE WITH FRED-MD
# ==================================== #

# 5.1a Data: September 1016 vintage of FRED-MD
md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
tcode <- unlist(md_raw[1, -1])
md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))

transform_series <- function(x, code) {
  d <- function(v) c(NA, diff(v))
  switch(as.character(code),
         "1" = x,                      # level
         "2" = d(x),                   # first difference
         "3" = d(d(x)),                # second difference
         "4" = log(x),                 # log
         "5" = 100 * d(log(x)),        # growth rate
         "6" = 100 * d(d(log(x))),     # change in the growth rate
         "7" = d(x / dplyr::lag(x) - 1))
}

# 5.1b Apply the transformations 
fm <- map2_dfc(md[names(tcode)], tcode, transform_series) |>
  mutate(date = md$date, .before = 1) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

fm <- fm[, colSums(is.na(fm)) == 0]

dim(fm)

tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]

## INDPRO: code 5, growth rate 
## PAYEMS: code 5, growth rate
## CPIAUCSL: code 6, change in growth rate 
## FEDFUNDS: code 2, first difference 
## the transformation differences depend on what we are interested in observing. The first difference of FEDFUNDS removes randomness and give us the a stationary rates of change. The growth rate for INDPRO was taken because the variable could have exhibited seasonality and therefore to understand the true underlying growth of this variable, we would find the log of the variable * 100. The first difference measures absolute change, the growth rate measures the percentage change over time. 

# 5.2a Estimating the Factors 
Z <- fm |> select(-date) |> as.matrix()

pc <- prcomp(scale(Z))

summary(pc)$importance[, 1:5]

## The principal components are estimates of the dynamic factors. These are linear combinations, weighted averages, of all the data. The share of the variation in all 121 series of the first three factors explain how much these dynamic factors move together. The first three factors explain 29% of the variation in all of the series. The first factor, PC1 = 0.149840, explains the real activity factor. The second, PC2 = 0.223910, explains the inflation factor. The third, PC3 = 0.294530, explains price variables. 

# 5.2b 

F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))

tibble(date = fm$date, F1 = F1) |>
  ggplot(aes(date, F1)) +
  geom_line() +
  labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
  theme_minimal()

# 5.2c find 8 series most correlated with each of the first two factors:
sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]
sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]

## Factor 1: The 8 most correlated: Based on thee series and the plot, it sees as though the factors capture economic production and activity. 
  ## PAYEMS: All employees: Non-farm
  ## IPMANSICS: IP:   manufacturing (SIC)
  ## USGOOD:          All employees: Goods-producing industries 
  ## INDPRO:          IP index
  ## MANEMP:          All employees: Manufacturing 
  ## IPFPNSS:         IP: Final products and nonindustrial supplies 
  ## CUMFNS:          Capacity utilization: manufacturing 
  ## DMANEMP:         All employees: Durable goods

## Factor 2: The 8 most correlated: Based on these series and the plot, it seems as though the factors capture consumption variables, or the Consumer Price Index (CPI). We can see this with the various dips due to recessions. 
  ## CUSR0000SAC:     CPI: commodities 
  ## DNDGRG3M086SBEA: Person Consumption Expenditure: Non-durable goods 
  ## CUSR0000SA0L2:   CPI: all items less shelter 
  ## CPIAUCSL:        CPI: all items 
  ## CUSR0000SA0L5:   CPI: all items less medical care
  ## CPITRNSL:        CPI: transportation 
  ## PCEPI:           Personal Consumption Expenditure: chain index
  ## CPIULFSL:        CPI: all items less food

# 5.3 Replicate the Horse Race from Class
ip <- fm$INDPRO
Z_mat <- as.matrix(Z)          # Ensure Z is treated as a matrix
n <- nrow(Z_mat)

# Safely find the integer row index
match_idx <- which(fm$date == as.Date("1989-12-01"))
if (length(match_idx) == 0) stop("Date not found in fm$date!")
init <- match_idx[1]          # Take the first match if there are duplicates

e_ar <- NULL
e_fa <- NULL
for (T in init:(n - 1)) {
  F_T <- prcomp(scale(Z_mat[1:T, ]))$x[, 1:3]
  d <- data.frame(y_next = ip[2:T], y = ip[1:(T - 1)], F_T[1:(T - 1), ])
  new <- data.frame(y = ip[T], t(F_T[T, ]))
  e_ar <- c(e_ar, ip[T + 1] - predict(lm(y_next ~ y, d), new))
  e_fa <- c(e_fa, ip[T + 1] - predict(lm(y_next ~ ., d), new))
}

c(RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)))

## The RMSE_AR = 0.617 and RMSE_FA = 0.589, which matches the numbers found in class. We couldn't estimate the factors once using the full sample for the horse race because this introduces information leakage which invalidates out of sample forecasts.

# 5.4 A direct forecast of inflation 

direct_race <- function(series, h = 12, r = 3) {
  lv <- md |>
    mutate(log_x = log(.data[[series]]),
           g = 100 * (log_x - lag(log_x))) |>
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  yh <- 100 * (lead(lv$log_x, h) - lv$log_x)      # growth over the next h months
  g  <- lv$g                                      # growth over the latest month
  
  e_ar <- NULL
  e_fa <- NULL
  for (T in init:(n - h)) {
    F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:r, drop = FALSE]
    i <- 1:(T - h)
    d <- data.frame(yh = yh[i], g = g[i], F_T[i, , drop = FALSE])
    new <- data.frame(g = g[T], F_T[T, , drop = FALSE])
    e_ar <- c(e_ar, yh[T] - predict(lm(yh ~ g, d), new))
    e_fa <- c(e_fa, yh[T] - predict(lm(yh ~ ., d), new))
  }
  tibble(series, forecasts = length(e_ar),
         RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)),
         MAE_AR = mean(abs(e_ar)), MAE_FA = mean(abs(e_fa)),
         DM_pvalue = forecast::dm.test(e_ar, e_fa, h = h, power = 2,
                                       varestimator = "bartlett")$p.value)
}

direct_race("CPIAUCSL")

## Yes, the factors improve the forecast of 12-month CPI inflation. The RMSE drops by about 10.43% and the MAE drops by about 14.56%. The difference is statistically significant. 
