# options (digits=4, width=70)

# Please install first using: install.packages('package_name') command!
library("PerformanceAnalytics")
library("tseries")
library("zoo")


# TASK 1 ------------------------------------------------------------------
########################################################
### TASK 1: Looping 10 stock prices of S&P500
########################################################
tickers <- c("AAPL", "MSFT", "AMZN", "GOOGL", "BRK-B",
             "JPM", "JNJ", "NVDA", "BAC", "PFE")

stock_prices = list() # object to hold stock prices

for(i in tickers){ # retrieve returns from Jan 2010 to Jan 2020
  stock_prices[[i]] <- get.hist.quote(instrument = i, start = "2010-01-01",
                                      end = "2020-01-01", quote = "AdjClose",
                                      provider = "yahoo", origin = "2000-09-01",
                                      compression = "m", retclass = "zoo")
}
sapply(stock_prices, length) # checking lengths of downloaded RETURNs (i.e 120)

# TASK 2 ------------------------------------------------------------------
########################################################
### Computing summary statistics
########################################################
len = 120 # number of months from Jan 2010 to Jan 2021
ones <- matrix(data = rep(1, len), ncol = 1) # matrix of ones
mat.x <- sapply(stock_prices, unclass) # obtain matrix of 10 securities

# mean vector
mu_cap <- 1/len * t(ones) %*% mat.x # mean returns across 10 securities

# mean matrix
mu_mat <- matrix(data = mu_cap, ncol = 10, nrow = 120, byrow = TRUE)

# variances and covariances
var_cov <- 1/(len - 1) * t((mat.x - mu_mat)) %*% (mat.x - mu_mat)

# To get the variance elements, perform diag(diag(var_cov))
# To get the covariance elements, perform var_cov - diag(diag(var_cov))

# TASK 3 ------------------------------------------------------------------
########################################################
### Optimal Markowitz portfolio
########################################################

## Method I (Lagrangian)

## Method II (Optimization)







