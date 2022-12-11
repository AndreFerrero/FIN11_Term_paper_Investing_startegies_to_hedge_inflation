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


# We create an assets dataframe
# Before filtering the assets, we merged them together with an inner join
assets <- join_all(list(gold, tips, reit, xle, sp500), type = "inner", by = "date") %>%
  mutate(date = dmy(date)) %>%
  filter(date > "2021-11-17")


# Extracting benchmark data
benchmark <- assets %>% select(c("date", "sp500"))


# Importing inflation to compare it with REIT
cpi <- read_csv("CPI.csv")
reit$date <- dmy(reit$date)

# Inner join to extract the same values
reit_cpi <- inner_join(reit, cpi) %>%
  filter(date > "2021-05-17")




# TIPS ANALYSIS
# Extracting the relevant data points in this variable
bonds <- tips %>% 
  mutate(date = dmy(date), ) %>%
  filter(date < "2022-07-06", date > "2021-11-18")

# Bonds returns for a different time period were computed
bonds_returns <- as.numeric((bonds[nrow(bonds), 2] - bonds[1, 2]) / bonds[1, 2]) * 100

# Final graph
ggplot(data = bonds, aes(x = date, y = tips)) +
  geom_line(col = "blue", size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  # ggtitle("TIPS after FED rate hikes") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab(NULL) + ylab("TIPS index") +
  geom_vline(xintercept = as.numeric(as.Date("2022-03-17")), col = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2022-05-05")), col = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2022-06-16")), col = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-03"), col = "black"),
             linetype = 4) +
  theme_light()




# ITALIAN IIB ANALYSIS
ita <- read_csv("Italian IIB.csv")
# filtering for the period we want
ita <- ita %>% select(date, ita) %>%
  mutate(date = dmy(date)) %>%
  filter(date > "2021-11-18", date < "2022-11-18")

ggplot(data = ita, aes(x = date, y = ita)) +
  geom_line(col = "chartreuse4", size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_vline(xintercept = as.numeric(as.Date("2022-07-27")), col = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-14")), col = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2022-11-02")), col = "red") +
  theme_light() +
  ylab("Italian IIB index") + xlab(NULL)



# REIT ANALYSIS
# creating various xts objects
library(xts)

cpi_ts <- xts(reit_cpi$CPI, order.by = reit_cpi$date)
reit_ts <- xts(reit_cpi$reit, order.by = reit_cpi$date)

# This library will be used again in the portfolio analysis
library(quantmod)
# This function computes the monthly returns
month_reit <- monthlyReturn(reit_ts)

# Merge with xts objects
reit_cpi_ts <- merge.xts(cpi_ts, reit_ts)
# Change column names
colnames(reit_cpi_ts) <- c("cpi", "monthly_reit")

# Final graph
ggplot(reit_cpi_ts) +
  geom_line(aes(x = Index, y = cpi, color = "CPI"), size = 1) +
  geom_line(aes(x = Index, y = monthly_reit / 6, color = "REIT monthly returns (scaled by 1/6)"), size = 1) +
  theme_light() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Monthly returns REIT and CPI") +
  xlab(NULL)



# XLE ANALYSIS
# Graph
ggplot(assets) + 
  geom_line(aes(x = date, y = xle, color = "XLE"), size = 1) +
  geom_line(aes(x = date, y = sp500 / 4, color = "BENCHMARK (scaled by 1/4)"), size = 1) +
  theme_light() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("XLE and S&P500") +
  xlab(NULL) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), col = "black")

# Removing data not useful anymore
rm(gold)
rm(reit)
rm(sp500)
rm(tips)
rm(xle)

# PORTFOLIO ANALYSIS

# Converting to xts objects for later
portfolio <- xts(assets[ , c(2, 3, 4, 5, 6)], order.by = assets$date)

benchmark <- xts(benchmark[ , 2], order.by = benchmark$date)

# We will use the quantmod and PerformanceAnalytics packages
# library(quantmod)
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
goldReturn <- Return.portfolio(portfolioReturns, weights = gold_w)
tipsReturn <- Return.portfolio(portfolioReturns, weights = tips_w)
reitReturn <- Return.portfolio(portfolioReturns, weights = reit_w)
xleReturn <- Return.portfolio(portfolioReturns, weights = xle_w)
globalReturn <- Return.portfolio(portfolioReturns, weights = global_w)

# This returns 3 different versions of the sharpe ratio
# The risk free rate is divided by 250 to annualise it, given 250 is the time points available
gold_SR <- SharpeRatio(goldReturn, rf/250)
tips_SR <- SharpeRatio(tipsReturn, rf/250)
reit_SR <- SharpeRatio(reitReturn, rf/250)
xle_SR <- SharpeRatio(xleReturn, rf/250)
global_SR <- SharpeRatio(globalReturn, rf/250)

benchmarkReturn <- Return.portfolio(benchmarkReturns)

SR_benchmark <- SharpeRatio(benchmarkReturn, rf/250)

# We extract a vector of every annual return for our time period
returns <- (assets[nrow(assets), 2:6] - assets[1, 2:6]) / assets[1, 2:6]
# and then obtain the final portfolio expected returns
gold_final <- weighted.mean(returns, gold_w)
tips_final <- weighted.mean(returns, tips_w)
reit_final <- weighted.mean(returns, reit_w)
xle_final <- weighted.mean(returns, xle_w)
global_final <- weighted.mean(returns, global_w)


benchmark_return <- returns[5]



# CREATING TABLES FOR METRICS

gold_metrics <- data.frame(gold_final * 100, 100 * gold_SR[1, 1])
names(gold_metrics) <- c("Expected Returns(%)", "Sharpe Ratio(%)")

tips_metrics <- data.frame(tips_final * 100, 100 * tips_SR[1, 1])
names(tips_metrics) <- c("Expected Returns(%)", "Sharpe Ratio(%)")

reit_metrics <- data.frame(reit_final * 100, 100 * reit_SR[1, 1])
names(reit_metrics) <- c("Expected Returns(%)", "Sharpe Ratio(%)")

xle_metrics <- data.frame(xle_final * 100, 100 * xle_SR[1, 1])
names(xle_metrics) <- c("Expected Returns(%)", "Sharpe Ratio(%)")

global_metrics <- data.frame(global_final * 100, 100 * global_SR[1, 1])
names(global_metrics) <- c("Expected Returns(%)", "Sharpe Ratio(%)")

benchmark_metrics <- data.frame(benchmark_return * 100, 100 * SR_benchmark[1 , 1])
names(benchmark_metrics) <- c("Expected Returns(%)", "Sharpe Ratio(%)")

df <- data.frame(
  c(gold_final, tips_final, reit_final, xle_final, global_final, as.numeric(returns[5])) * 100,
  c(gold_SR[1, 1], tips_SR[1, 1], reit_SR[1, 1], xle_SR[1, 1], global_SR[1, 1], SR_benchmark[1, 1]) * 100)

rownames(df) <- c("GOLD", "TIPS", "REIT", "XLE", "GLOBAL", "BENCHMARK")
colnames(df) <- c("Expected returns(%)", "Sharpe Ratio(%)")

# sjPlot library
library(sjPlot)
tab_dfs(list(gold_metrics, benchmark_metrics),
        titles = c("GOLD PORTFOLIO", "BENCHMARK"), digits = 3)

tab_dfs(list(tips_metrics, benchmark_metrics),
        titles = c("TIPS PORTFOLIO", "BENCHMARK"), digits = 3)

tab_dfs(list(reit_metrics, benchmark_metrics),
        titles = c("REIT PORTFOLIO", "BENCHMARK"), digits = 3)

tab_dfs(list(xle_metrics, benchmark_metrics),
        titles = c("ENERGY PORTFOLIO", "BENCHMARK"), digits = 3)

tab_dfs(list(global_metrics, benchmark_metrics),
        titles = c("GLOBAL PORTFOLIO", "BENCHMARK"), digits = 3)

tab_df(df, title = "PORTFOLIOS",
        digits = 3, show.rownames = T, col.header = c("Expected returns(%)", "Sharpe Ratio(%)"))

