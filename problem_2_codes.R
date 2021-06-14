options(digits = 4)

### Problem 2 ###
# 1. phi function ---------------------------------------------------------

phi <- function(x = NULL){
  if(is.null(x)) stop("Stop! Null vector not allowed!") # test for non-null vector
  
  x = scale(x) # obtain standardization of vector x
  out <- 1/sqrt(2 * pi) * exp(-x^2/2) # probability density of vector x
  return(as.vector(out))
}

# output vector of densities
phi(x = seq(100)) # arbitrarily chosen

# 2. quantile vector, integrate to obtain phi vector values ---------------

quant = seq(0, 5.5, by = 1/100) # for half-table values
f = function(x) 1/sqrt(2 * pi) * exp(-x^2/2) # integrand for Phi(x) value
probs <- sapply(quant, 
                FUN = function(x) integrate(f, lower = 0, upper = x)$value)

# check if probs and pnorm() agree to 4 decimal places
identical(round(probs, 4), round(pnorm(q = quant)-0.5, 4))

# matrix form (subset first 400 to draw half-table)
xmat <- matrix(probs[1:400], ncol = 10, byrow = TRUE)
dimnames(xmat) <- list(seq(0.0, 3.99, by = 1/10), seq(0, 0.09, by = 1/100))
xmat

# 3. Distribution curve (full table plot) ----------------------------------
x = c(-rev(quant), quant)
y = sapply(x, 
           FUN = function(x) integrate(f, lower = -5.5, upper = x)$value)

plot(x, y, type = 'l',
     xlab = "Quantile values",
     ylab = "Cummulative density",
     lwd = 2, lty = 2,
     main = expression(
       paste("Standard Gaussian distribution function:")~Phi(x))
     # xlim = c(-5, 5), ylim = c(0, 1), axes = TRUE,
     )
curve(expr = pnorm(x), add = TRUE, col = 'blue', lty = 1.5)
legend('topleft',
       inset = 0.05,
       legend = c("pnorm", "brute-force"),
       bty = "n", # Removes the legend box
       lty = c(1, 2),
       col = c('blue', 'black'),
       lwd = 2)
