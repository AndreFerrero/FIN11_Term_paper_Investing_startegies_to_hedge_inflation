# Tidyverse and plyr to handle and merge dataframes
# lubridate to mutate date variables, treated by R as a different object
library(tidyverse)
library(plyr)
library(lubridate)

# IMPORTING DATA 
setwd("C:/Users/utente/OneDrive - Università degli Studi di Torino/University/NHH/Trading/Final paper")

gold <- read_csv("Gold ETF.csv")

tips <- read_csv("tips.csv")

reit <- read_csv("reit.csv")

xle <- read_csv("XLE.csv")

sp500 <- read_csv("SPY.csv")

# This is the series of the risk free rate
tbills <- read_csv("T-bills.csv")

tbills <- tbills %>%
  # dates are in reverse order
  arrange(mdy(date)) %>%
  mutate(date = mdy(date)) %>%
  # Filtering data for the period we are interested about
  filter(date > "2021-11-17", date < "2022-11-19")

# Before filtering the assets, we merged them together with an inner join
assets <- join_all(list(gold, tips, reit, xle, sp500), type = "inner", by = "date") %>%
  mutate(date = dmy(date)) %>%
  filter(date > "2021-11-17")

# Extracting benchmark data
benchmark <- assets %>% select(c("date", "sp500"))

# Converting to xts objects for later
library(xts)
portfolio <- xts(assets[ , c(2, 3, 4, 5, 6)], order.by = assets$date)

benchmark <- xts(benchmark[ , 2], order.by = benchmark$date)

# Removing data not useful anymore
rm(gold)
rm(reit)
rm(sp500)
rm(tips)
rm(xle)

# PORTFOLIO ANALYSIS

# We will use the quantmod and PerformanceAnalytics packages
library(quantmod)
library(PerformanceAnalytics)

# This function computes the daily rate of change (Returns, as defined in the methodology section)
portfolioReturns <- na.omit(ROC(portfolio))

benchmarkReturns <- na.omit(ROC(benchmark))

# Here we define the different weights to be used as input later
gold_w <- c(0.6, 0, 0, 0, 0.4)
tips_w <- c(0, 0.6, 0, 0, 0.4)
reit_w <- c(0, 0, 0.6, 0, 0.4)
xle_w <- c(0, 0, 0, 0.6, 0.4)
global_w <- c(0.175, 0.2, 0.1, 0.35, 0.175)

# One can use this function to check whether the weights sum up to 1
# sum(WEIGHTS)

# RISK FREE RATE 3 MONTHS

# We divide by 100 since it was in %
rf <- mean(tbills$rate_3) / 100

# RETURNS AND SHARPE RATIOS

# Return.portfolio computes the weighted returns for every time point, given the specified weights
portfolioReturn <- Return.portfolio(portfolioReturns, weights = global_w)

# This returns 3 different versions of the sharpe ratio
# The risk free rate is divided by 250 to annualise it, given 250 is the time points available
SR_portfolio <- SharpeRatio(portfolioReturn, rf/250)

benchmarkReturn <- Return.portfolio(benchmarkReturns)

SR_benchmark <- SharpeRatio(benchmarkReturn, rf/250)

# We extract a vector of every annual return for our time period
returns <- (assets[nrow(assets), 2:6] - assets[1, 2:6]) / assets[1, 2:6]
# and then obtain the final portfolio expected returns
final_return <- weighted.mean(returns, global_w)

benchmark_return <- returns[5]

# CREATING TABLES FOR METRICS

portfolio_metrics <- data.frame(final_return * 100, 100 * SR[1, 1])
names(portfolio_metrics) <- c("Expected Returns(%)", "Sharpe Ratio(%)")

benchmark_metrics <- data.frame(benchmark_return * 100, 100 * SR_benchmark[1, 1])
names(benchmark_metrics) <- c("Expected Returns(%)", "Sharpe Ratio(%)")

# sjPlot library
library(sjPlot)
tab_dfs(list(portfolio_metrics, benchmark_metrics),
        titles = c("PORTFOLIO", "BENCHMARK"), digits = 3)


         