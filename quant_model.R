library(quantmod)
library(PerformanceAnalytics)
library(BatchGetSymbols)
library(tidyquant)
library(TTR)
library(pdfetch)
library(tidyquant)
library(tidyverse)
library(timetk)
library(tidyr)
library(dplyr)
library(xlsx)
library(ggplot2)
library(plyr)
library(RedditExtractoR)
library(sentimentr)
library(edgar)
library(tidyr)
library(lubridate)
library(dplyr)
library(bizdays)
library(finreportr)
library(Quandl)
Quandl.api_key("INSERT KEY HERE") 
start_date = "2018-01-01"

df = tq_index("SP500")
symbols_vec <- c(df$symbol[1:100])
symbols_vec <- symbols_vec[!symbols_vec %in% c("JW.A","BRK.B", "BF.B", "APH", "FRC")]
symbols_vec_obj <- gsub("\\^", "", symbols_vec)
getSymbols(symbols_vec, from = start_date, to = Sys.Date())
stock_data <- mget(symbols_vec_obj)
#saveRDS(stock_data, file = "stock_data_raw_100", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
stock_data = readRDS("stock_data_raw_100", refhook = NULL)
# limit stocks if they had less than 15 days of data
stock_data = stock_data[sapply(stock_data, function(x) dim(x)[1]) > 15]

col_titles <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

# Risk-free rate (Current 10-year treasury rate)
rfr <- 0.0068

# Return list of renamed data frames.
changeColNames <- function(Data)
{
  names(Data) <- col_titles
  return(Data)
}

# hello world
stock_data <- lapply(stock_data, changeColNames) 

# Retrieving the adjusted price column
adj_price <- lapply(stock_data, Ad)

# Calculating daily returns
daily_returns <- lapply(adj_price, dailyReturn, type = "log")

addReturns <- function(Data)
{
  for (i in 1:length(symbols_vec)){
    Data[[symbols_vec[i]]]$returns <- daily_returns[[symbols_vec[i]]]$daily.returns
  }
  return(Data)
}
stock_data <- addReturns(stock_data) 

add_MACD <- function(Data)
{
  Data[is.na(Data)] = 0
  macd <- MACD(Data$Adjusted, nFast=12, nSlow=26, nSig=9, maType=SMA)
  Data$macd <- macd$macd
  Data$macd_signal <- macd$signal
  Data$macd_factor <- macd$signal - macd$macd
  return(Data)
}
stock_data <- lapply(stock_data, add_MACD)

add_RSI <- function(Data)
{
  rsi <- RSI(Data$Adjusted, n=14)
  Data$rsi_factor <- rsi
  return(Data)
}
stock_data <- lapply(stock_data, add_RSI)

### Setting appropriate calendar to NYSE
load_rmetrics_calendars(2000:year(Sys.Date()) +1)
calendars()
bizdays.options$set(default.calendar = "Rmetrics/NYSE")
bizdays.options$get('default.calendar')

keeps<-c("company.name", "form.type", "date.filed", "lm.negative.count",
         "lm.positive.count","lm.strong.modal.count",
         "lm.moderate.modal.count", "lm.weak.modal.count", "lm.uncertainty.count",
         "hv.negative.count")

addSetinment <- function(Data, symbols_vec) {
  for (i in 1:length(Data)) {
    print(paste("Getting data for ",symbols_vec[i],sep=""))
    sentiment <- getSentiment(cik.no = c(gsub("(^|[^0-9])0+", "\\1", c(CompanyInfo(symbols_vec[i])$CIK[1]), perl = TRUE)),
                             form.type = '10-Q',
                             filing.year = c(2017, 2018, 2019, 2020))
    sentiment <- sentiment[keeps]
    names(sentiment)[3] <- "Date"
    sentiment <- sentiment %>%
      mutate(Date = as.Date(Date)) %>%
      complete(Date = bizseq("2018/1/1", str_replace_all(Sys.Date()-1, "-", "/")))

    df.xts <- xts(sentiment,order.by = sentiment$Date)
    df.xts <- na.locf(merge(df.xts, foo=zoo(NA, order.by=seq(start(df.xts), end(df.xts),
                                                             "day",drop=F)))[, ])

    df <- df.xts[paste('2018-01-01/', Sys.Date(),sep="")]

    cols_wanted <- c("lm.negative.count","lm.positive.count", "lm.strong.modal.count", "lm.moderate.modal.count","lm.weak.modal.count","lm.uncertainty.count","hv.negative.count")
    df <- df[,cols_wanted]
    Data[[symbols_vec[i]]] <- merge(Data[[symbols_vec[i]]], df)
  }
  return(Data)
}

stock_data <- addSetinment(stock_data, symbols_vec)
# saveRDS(stock_data1, file = "stock_data_senti_100", ascii = FALSE, version = NULL,
#         compress = TRUE, refhook = NULL)
# Macroeconomic data
unrate <- Quandl("FRED/UNRATE", start_date=start_date, end_date=Sys.Date())
unrate["Value"] <- unrate$Value/100
unrate <- xts(unrate, order.by = unrate$Date)
unrate <- na.locf(merge(unrate, foo=zoo(NA, order.by=seq(start(unrate), end(unrate),"day",drop=F)))[, ])

cpi = Quandl("FRED/CPIAUCSL", start_date="2018-01-01", end_date=Sys.Date())
cpi <- xts(cpi, order.by = cpi$Date)
cpi <- na.locf(merge(cpi, foo=zoo(NA, order.by=seq(start(cpi), end(cpi),"day",drop=F)))[, ])
cpi = cpi[,"Value"]

fedFunds <- Quandl("FRED/FEDFUNDS", start_date = "2018-01-01", end_date = Sys.Date())
fedFunds["Value"] <- fedFunds$Value/100
fedFunds <- xts(fedFunds, order.by = fedFunds$Date)
fedFunds <- na.locf(merge(fedFunds, foo=zoo(NA, order.by=seq(start(fedFunds), end(fedFunds),"day",drop=F)))[, ])
fedFunds <- fedFunds[,"Value"]

add_Macro = function(Data)
{
  Data$unrate <- unrate$Value
  Data$cpi <- cpi$Value
  Data$fedFunds <- fedFunds$Value
  Data <- na.locf(Data) 
  return(Data)
}
stock_data <- lapply(stock_data, add_Macro)


# There might be an error with our code above, so you can load our preprocessed data from stock_data.rds
stock_data <- readRDS("stock_data_100", refhook = NULL)
#saveRDS(stock_data, file = "stock_data_100", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)


factors <- c("rsi_factor", "macd_factor", "Volume", "macd", "lm.negative.count","lm.positive.count", "lm.strong.modal.count", "lm.moderate.modal.count", "unrate", "cpi", "fedFunds")
p_value_factors <- c("Intercept_p_value", paste(factors, "_p_value", sep=""))
coef_factors <- c("Intercept_coef", paste(factors, "_coef", sep=""))


lagFactors <- function(Data, factors) {
  for (i in 1:length(symbols_vec)) {
    Data[[symbols_vec[i]]] = na.omit(Data[[symbols_vec[i]]])
    x = length(Data[[symbols_vec[i]]][,'returns']) -1
    for (factor in factors) {
      test = c(NA,Data[[symbols_vec[i]]][,factor][0:x])
      Data[[symbols_vec[i]]][,factor] = test
    }
  }
  return(stock_data)
}
stock_data_lag = lagFactors(stock_data, factors)

df =do.call("rbind", stock_data_lag)
signif_level = 0.05
top_percent = 0.4
remove <- c("macd", "lm.moderate.modal.count")
num_times_to_sort = 3
 

reg = lm(df$returns ~ df[,factors])
factor_coef = summary(reg)[["coefficients"]][,"Estimate"]
names(factor_coef) <- c("Intercept", factors)
factor_signif = summary(reg)[["coefficients"]][, "Pr(>|t|)"]
names(factor_signif) <- c("Intercept", factors)

p_values = factor_signif[order(unlist(factor_signif),decreasing=FALSE)]
p_values = p_values[p_values < signif_level]
p_sort = names(p_values)

determineStocks <- function(Data, symbols_vec, p_factor) {
  result=list()
  for (i in 1:length(symbols_vec)) {
    abc = df[[symbols_vec[i]]][,p_factor][[length(df[[symbols_vec[i]]][,p_factor]),1]]
    print(abc)
    result[[symbols_vec[i]]] = abc
  }
  result = result[order(unlist(result),decreasing=FALSE)]
  return(result)
  
}
df = stock_data
getSortedStocks <- function(Data, factors,symbols_vec) {
  result=list()
  for (factor in factors) {
    sorted = determineStocks(Data, symbols_vec, factor)
    result[[factor]] = sorted
  }
  return(result)
}

sorted_stocks = getSortedStocks(stock_data, factors,symbols_vec)

# gets stocks you should buy for a particular factor and top percentage
buyStocks <- function(sorted_stocks, buy_order, top_p, factor, factor_coef) {
  coef = factor_coef[[factor]]
  if (coef > 0) {
    # for positive coefficient, get largest, for example, macd_factor
    buys = names(tail(buy_order[[factor]], n=top_p*length(buy_order[[factor]])))
  } else {
    # if negative coefficient, then we want the smallest factor value
    # example we would want a small rsi value
    buys = names(head(buy_order[[factor]], n=top_p*length(buy_order[[factor]])))
  }
  buys = names(head(buy_order[[factor]], n=top_p*length(buy_order[[factor]])))
  return(buys)
}

buyFactors <- function(Data, buy_order, top_p, sort_n, remove_list, factor_coef) {
  buys = names(buy_order)
  final = names(Data)
  print("Sorted from initial list of:")
  print(final)
  print("Begin sorting")
  buy_list = buys[! buys %in% remove_list]
  for (factor in buy_list[1:sort_n]) {
    buy_list_factor = buyStocks(sorted_stocks, buy_order, top_p, factor, factor_coef)
    final = final[final %in% buy_list_factor]
    print("For factor: ")
    print(factor)
    print("Reduced to legnth: ")
    print(length(final))
    print("Stocks left: ")
    print(final)
    if (length(final) < 5) {
      break
    }
  }
  return(final)
}

stocks_to_buy = buyFactors(stock_data, sorted_stocks, top_percent,num_times_to_sort, remove, factor_coef)

# returns the weights of our portfolio based on the buys
getWeights <- function(buys, symbols_vec) {
  result <- vector(mode="numeric", length=length(symbols_vec))
  frac_add = 1/length(buys)
  for (ordered_stock in buys) {
    for (stock in ordered_stock) {
      index = which(symbols_vec == stock)
      result[index] = frac_add
    }
  }
  return(result)
}
stock_weights = getWeights(stocks_to_buy, symbols_vec)


#saveRDS(stocks_to_buy, file = "Final_stocks_to_buy.rds")

# Using S&P500 as the market index (benchmark)
mkt_ret <-
  dailyReturn(Ad(getSymbols(
    "^GSPC", auto.assign = FALSE, from = "2015-01-01"
  )), type = "log")

colnames(mkt_ret) <- "GSPC"
#adj_price <- lapply(stock_data, Ad)

# Calculating daily returns
daily_returns <- lapply(adj_price, dailyReturn, type = "log")

# Calculating monthly returns
monthly_returns <- lapply(adj_price, monthlyReturn, type = "log")

# Plot return of each stock
for (i in 1:length(monthly_returns)) {
  charts.PerformanceSummary(monthly_returns[[i]], main = names(monthly_returns)[i])
}

# Annualized return, standard deviation and sharpe ratio
annual_summary <-
  as.data.frame(lapply(monthly_returns, function(x)
    table.AnnualizedReturns(x, Rf = rfr / 12, scale = 12)))

colnames(annual_summary) <- names(monthly_returns)
print("Annualized summary of each stock")
print(annual_summary)

write.csv(annual_summary, file = "Results/Annual_summary_stock.csv")

# Merging the returns of all stocks
comb_ret <- Reduce(function(x, y)
  merge(x, y, all = FALSE), monthly_returns)
colnames(comb_ret) <- names(monthly_returns)
comb_ret <- comb_ret[-1,]

# Var of each stock at 95% confidence level
stock_VaR <-
  lapply(comb_ret, function(x)
    VaR(x, method = "gaussian", p = 0.95))

# Maximum drawdown of each stock
stock_max_drawdown <- maxDrawdown(comb_ret)

# port_mon_ret <- Return.portfolio(comb_ret, weights = stock_weights)
port_mon_ret <-
  Return.rebalancing(comb_ret, weights = stock_weights)

# VaR of the portfolio (marginal) at 95% confidence level
port_VaR_marginal <-
  VaR(
    comb_ret,
    p = 0.95,
    method = "gaussian",
    weights = stock_weights,
    portfolio_method = "marginal"
  )

print("Marginal VaR of the portfolio:")
print(port_VaR_marginal)
write.csv(port_VaR_marginal, file = "Results/MVaR_portfolio.csv")

# VaR of the portfolio (component) at 95% confidence level
port_VaR_component <-
  VaR(
    comb_ret,
    p = 0.95,
    method = "gaussian",
    weights = stock_weights,
    portfolio_method = "component"
  )
print("Component VaR of the portfolio:")
print(port_VaR_component)
write.csv(port_VaR_component, file = "Results/CVaR_portfolio.csv")

# Expected shortfall of the portfolio (component) at 95% confidence level
port_es <-
  ES(
    comb_ret,
    p = 0.95,
    method = "gaussian",
    weights = stock_weights,
    portfolio_method = "component"
  )
print("Expected shortfall of the portfolio:")
print(port_es)
write.csv(port_es, file = "Results/ES_portfolio.csv")

# Maximum drawdown of the portfolio
port_max_drawdown <- maxDrawdown(port_mon_ret)
print("Maximum drawdown of the portfolio:")
print(port_max_drawdown)
write.csv(port_max_drawdown, file = "Results/Max_drawdown_portfolio.csv")

# Plot of potfolio performance
png(
  "Portfolio_performance.png",
  res = 150,
  width = 1700,
  height = 1200
)
charts.PerformanceSummary(port_mon_ret, main = "Portfolio performance", legend.loc = "left")
dev.off()

# Plot of each stock's performance
comb_ret_2 <- comb_ret[,which(colnames(comb_ret) %in% stocks_to_buy)]
png(
  "Stock_performance.png",
  res = 150,
  width = 1700,
  height = 1200
)
chart.CumReturns(comb_ret_2, legend.loc = "bottomleft", main = "Performance of each stock")
dev.off()


# Annualized return, standard deviation and sharpe ratio of the portfolio
port_annual_data <-
  table.AnnualizedReturns(
    port_mon_ret,
    scale = 12,
    Rf = rfr / 12,
  )

colnames(port_annual_data) <- paste("Portfolio")
print("Annualized summary of the portfolio")
print(port_annual_data)
write.csv(port_annual_data, file = "Results/Annual_summary_portfolio.csv")

# Kelly ratio - tells us the percentage of capital that should be invested in this portfolio
kelly_ratio_port <- KellyRatio(port_mon_ret)
write.csv(kelly_ratio_port, file = "Results/Kelly_ratio_portfolio.csv")

# Information ratio - measures the risk adjusted return of the portfolio relative to the S&P500 index
info_ratio_port <-
  InformationRatio(port_mon_ret, mkt_ret, scale = 12)
write.csv(info_ratio_port, file = "Results/Information_ratio_portfolio.csv")

# Merging market return with return of stocks
all_returns <- merge(mkt_ret, comb_ret)

# Covariance and correlation
cov_stock <- cov(all_returns, use = "pairwise.complete.obs")
write.csv(cov_stock, file = "Results/Covariance_stock.csv")
cor_stock <- cor(all_returns, use = "pairwise.complete.obs")
write.csv(cor_stock, file = "Results/Correlation_stock.csv")

# Fama French
ff_url <-
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
temp_file <- tempfile()
download.file(ff_url, temp_file)
ff_factors_raw_data <- unzip(temp_file)

# Remove first 3 rows of unwanted data
ff_factors_raw_data <- read_csv(ff_factors_raw_data, skip = 3)

# We do not need rows from 1131
ff_factors_raw_data <- ff_factors_raw_data[c(1:1130), ]

colnames(ff_factors_raw_data)[1] <- "Date"

ff_factors_raw_data$Date <-
  seq(as.Date("1926-07-01"), as.Date("2020-08-31"), by = "months")


ff_factors_raw_data[, 2:ncol(ff_factors_raw_data)] <-
  ff_factors_raw_data[, 2:ncol(ff_factors_raw_data)] / 100

rownames(ff_factors_raw_data) <- ff_factors_raw_data$Date

test <- c(ceiling_date(as.Date((as.POSIXct(port_mon_ret, origin="1970-01-01", format = '%Y-%m-%d'))), "month") - days(1))
df = as_tibble(port_mon_ret)
df$Date = test
ff_factors_raw_data$Date = (ceiling_date(ff_factors_raw_data$Date,"month") - days(1))

port_with_ff = inner_join(df , ff_factors_raw_data, by="Date", copy =FALSE)
port_with_ff$Portfolio_excess_ret <- port_with_ff$portfolio.returns - port_with_ff$RF
names(port_with_ff)[3] <- "Excess_mkt_ret"

final_reg <- as.formula(paste(names(port_with_ff)[7], "~", paste(
  c(
    names(port_with_ff)[3],
    names(port_with_ff)[4],
    names(port_with_ff)[5]
  ), collapse = " + "
)))

ff_result <- summary(eval(bquote(
  lm(.(final_reg), data = port_with_ff)
)))
print(ff_result)
saveRDS(ff_result, file = "Fama_French regression.rds")


# sentimentData = function(stocks_to_buy) {
#   result = list()
#   for (stock in stocks_to_buy) {
#     reddit_text <- tryCatch(
#       {
#         reddit_text = get_reddit(stock,subreddit="Business",sort_by="new")
#       },
#       error = function(cond) {
#         return(0)
#       }
#     )
#     if (reddit_text == 0) {
#       # cant get reddit info, so will set sentiment to 0
#       result[[stock]] = 0
#     } else {
#       post_text = iconv(reddit_text$post_tex, "WINDOWS-1252", "UTF-8")
#       reddit_post_sentiment = mean(sentiment(post_text)$sentiment)
#       result[[stock]] = reddit_post_sentiment
#     }
#     
#   }
#   
# }
# sentiment_reddit = sentimentData(stocks_to_buy)
# stock_data = addRedditSentiment(stock_data, symbols_vec)



