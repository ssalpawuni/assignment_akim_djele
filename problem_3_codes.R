options (digits=4, width=70)

rm(list = ls())
library("PerformanceAnalytics")
library("tseries")
library("zoo")

# DATA --------------------------------------------------------------------
########################################################
### TASK 1: Retrieving 10 stock prices (Yahoo S&P500)
########################################################
tickers <- c("AAPL", "MSFT", "AMZN", "GOOGL", "BRK-B",
             "JPM", "JNJ", "NVDA", "BAC", "PFE")
lst <- list() # To hold adjusted monthly returns

retrieve_stock <- function(){
    for(i in tickers){
      lst[[i]] <- get.hist.quote(instrument = i,
                                          start = "2010-01-01",
                                          end = "2020-01-01",
                                          quote = "AdjClose",
                                          provider = "yahoo",
                                          origin = "2000-09-01",
                                          compression = "m",
                                          retclass = "zoo")
      }
    return(lst)
  }
stock_prices = retrieve_stock() # Stock prices from S&P500 (Yahoo)
sapply(stock_prices, length) # Check lengths of downloaded RETURNs (i.e 120)

# Transformation and Summary Statistics -----------------------------------
########################################################
### TASK 2: mean, variance-covariance and weights
########################################################
N = length(tickers) # number of securities (10)
ones <- matrix(data = 1, nrow = N) # matrix of ones

descriptive_stat <- function(){
  lr <- lapply(X = stock_prices, FUN = function(x) diff(log(x)))
  mat <- sapply(lr, unclass) # obtain matrix of 10 securities
  
  ln = nrow(mat) # off by one after differencing
  
  # random weights
  wts <- runif(n = N) # random weights (not scaled)
  wts <- wts/sum(wts) # scaled weights (NB: this sums to One!)
  names(wts) <- tickers
  
  mu <- 1/ln * t(mat) %*% matrix(1, nrow = ln) # mean vector
  mu <- as.vector(mu)
  names(mu) <- tickers
  
  mu_mat <- matrix(data = mu, ncol = N, nrow = ln, byrow = TRUE) # mean matrix
  
  vcv <- 1/(ln - 1) * t((mat - mu_mat)) %*% (mat - mu_mat) # var-cov
  vcv <- as.matrix(vcv)
  dimnames(vcv) <- list(tickers, tickers)
  class(vcv) <- "matrix"
  
  list(mu_cap = mu, cov_mat = vcv, weights = wts) # mean, variance, weights
}
descript <- descriptive_stat() # Get and print values from descriptive_stat()
descript$mu_cap
descript$cov_mat
descript$weights

# Get Portfolio -----------------------------------------------------------
########################################################
### Task 3: Create a portfolio object
########################################################
get_portfolio <-function(er, cov_mat, weights){
  
  ### Note the following
  # er: expected returns vector (N x 1)
  # cov_mat: variance-covariance matrix of returns (N x N)
  # weights: random weights vector, summing to 1 (N x 1) 
  
  # Assess validity of inputs
  weights <- as.vector(weights); names(weights) <- tickers
  er <- as.vector(er)					# assign names if none exist
  if(length(er) != length(weights))
    stop("Stop! Wrong dimensions: er and weights do not match")
  cov_mat <- as.matrix(cov_mat)
  if(length(er) != nrow(cov_mat))
    stop("Stop! Wrong dimensions: er and cov_mat do not match")
  if(any(diag(chol(cov_mat)) <= 0)) # check for positive definiteness
    stop("Covariance matrix not positive definite")
  
  # create portfolio
  er.port <- crossprod(er, weights)
  sd.port <- sqrt(weights %*% cov_mat %*% weights)
  # results
  list(g.er = as.vector(er.port),
       g.sd = as.vector(sd.port),
       weights = weights
       )
}
# Get values from get_portfolio()
get_port <- get_portfolio(er = descript$mu,
                          cov_mat = descript$cov_mat,
                          weights = descript$weights)
# portfolio with random weights
cat("Portfolio expected return: ", get_port$g.er)
cat("\nPortfolio standard deviation: ", get_port$g.sd)
get_port$weights


# Efficient portfolio (Resolution 1) --------------------------------------
########################################################
### Task 4: Compute minimum variance portfolio
########################################################

# NB: in this section, target return, c* = mu_cap for the ith return

efficient_portfolio <- function(er, cov_mat, target_return){
  ### Note the following
  # er: expected returns vector (N x 1)
  # cov_mat: variance-covariance matrix of returns (N x N)
  # weights: random weights vector, summing to 1 (N x 1) 
  # target.return: targeted return (scalar)
  
  #
  # output is portfolio object with the following elements
  # call				    original function call
  # er					    portfolio expected return
  # sd					    portfolio standard deviation
  # weights			    N x 1 vector of portfolio weights
  
  ## inputs
  er <- as.vector(er)					# assign names if none exist
  cov_mat <- as.matrix(cov_mat)
  if(length(er) != nrow(cov_mat))
    stop("invalid inputs")
  if(any(diag(chol(cov_mat)) <= 0))
    stop("Covariance matrix not positive definite")
  
  
  # compute efficient portfolio
  # forming system of equation
  
  ones <- rep(1, length(er))
  top <- cbind(2*cov_mat, er, ones)
  bot <- cbind(rbind(t(er), t(ones)), matrix(0,2,2)) # edited by me (no error)
  A <- rbind(top, bot)
  b.target <- as.matrix(c(rep(0, length(er)), target_return, 1))
  x <- solve(A, b.target)
  w <- x[1:length(er)]
  names(w) <- tickers
  
  #
  # compute portfolio expected returns and variance
  #
  er.port <- crossprod(er,w)
  sd.port <- sqrt(w %*% cov_mat %*% w)
  
  # output
  list(exp_ret_port = as.vector(er.port),
       sd_port = as.vector(sd.port),
       weights_ef = w)
}
efficient_portfolio(er = descript$mu_cap,
                    cov_mat = descript$cov_mat,
                    target_return = 0.0211) ### mu_cap for AAPL is 0.0211

# To compute for efficient portfolios for all tickers, run the code below 
# sapply(1:N, FUN = function(x){
#  efficient_portfolio(er = descrip$mu_cap, 
#                      cov_mat = desc_vals$cov_mat,
#                      target_return = descript$mu_cap[x])})


# Global Minimum variance (Resolution 2) ----------------------------------
########################################################
### Task 5: Global minimum variance portfolio
########################################################
global_min_portfolio <- function(er, cov_mat){
  #
  cov_mat_inv <- solve(cov_mat)
  one_vec <- rep(1,length(er))
  w_gmin <- rowSums(cov_mat_inv) / sum(cov_mat_inv)
  w_gmin <- as.vector(w_gmin)
  names(w_gmin) <- tickers
  er_gmin <- crossprod(w_gmin, er)
  sd_gmin <- sqrt(t(w_gmin) %*% cov_mat %*% w_gmin)
  
  # output
  list(er_gmin = as.vector(er_gmin),
       sd_gmin = as.vector(sd_gmin),
       w_gmin = w_gmin)
}
glmin <- global_min_portfolio(er = descript$mu_cap, cov_mat = descript$cov_mat)

# Global minimum variance
cat("Portfolio expected return (global): ", glmin$er_gmin)
cat("Portfolio standard deviation (global): ", glmin$sd_gmin)
glmin$w_gmin

# Compute efficient frontier ----------------------------------------------
########################################################
### Task 6: Compute Markowitz bullet
########################################################
efficient_frontier <- function(er, cov_mat,
                               nport = N, alpha_min = -0.5,
                               alpha_max = 1.5){
  
  # create portfolio names
  port_names <- rep("port", nport)
  ns <- seq(1, nport)
  port_names <- paste(port_names, ns)
  
  # compute global minimum variance portfolio
  #
  cov_mat_inv <- solve(cov_mat)
  one_vec <- rep(1, length(er))
  port_gmin <- global_min_portfolio(er, cov_mat)
  w_gmin <- port_gmin$w_gmin
  
  
  # compute efficient frontier as convex combinations of two efficient ports
  # 1st efficient port: global min var portfolio
  # 2nd efficient port: min var port with ER = max of ER for all assets
  er.max <- max(er)
  port.max <- efficient_portfolio(er, cov_mat, er.max)
  w.max <- port.max$weights_ef    
  a <- seq(from = alpha_min, to = alpha_max,length = nport)# convex combinations
  we.mat <- a %o% w_gmin + (1-a) %o% w.max	# rows are efficient portfolios
  er.e <- we.mat %*% er							# expected returns of efficient portfolios
  er.e <- as.vector(er.e)
  names(er.e) <- port_names
  cov.e <- we.mat %*% cov_mat %*% t(we.mat) # cov mat of efficient portfolios
  sd.e <- sqrt(diag(cov.e))					# std devs of efficient portfolios
  sd.e <- as.vector(sd.e)
  names(sd.e) <- port_names
  dimnames(we.mat) <- list(port_names, tickers)
  
  # 
  # summarize results
  list(markowitz_er = er.e,
       markowitz_sd = sd.e,
       markowitz_weights = we.mat)
}
efficient_frontier(er = descript$mu_cap,
                   cov_mat = descript$cov_mat,
                   nport = N,
                   alpha_min = -0.5,
                   alpha_max = 1.5)
########################################################
###                    THE END                       ###
########################################################


### Lagrangian method
markowitz = function(mu,cov_mat,er) {
  A = t(ones) %*% solve(cov_mat) %*% mu
  B = t(mu) %*% solve(cov_mat) %*% mu
  C = t(ones) %*% solve(cov_mat) %*% ones
  D = B*C - A^2
  lam = (C*er-A)/D
  gam = (B-A*er)/D
  wts = lam[1]*(solve(cov_mat) %*% mu) + gam[1]*(solve(cov_mat) %*% ones)
  g = (B[1]*(solve(cov_mat) %*% ones) - A[1]*(solve(cov_mat) %*% mu))/D[1]
  h = (C[1]*(solve(cov_mat) %*% mu) - A[1]*(solve(cov_mat) %*% ones))/D[1]
  wts = g + h*er
}

# Optimal portfolio weights: Assuming expected returns = c*
(markowitz(mu = descript$mu_cap, cov_mat = descript$cov_mat, er = 0.0211))

