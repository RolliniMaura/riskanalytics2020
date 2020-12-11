# These functions can be used to transform data to uniform scale,
# when the data are analysed separately below and above a threshold
# e.g. applying the GPD to values above a high threshold and treating
#      those below the threshold with the empirical CDF.

transform_to_uniform <- function(y,
                                 cdf,
                                 threshold,
                                 ...) {
  # ...: arguments to `cdf`
  
  
  above <- y > threshold
  # proportion of exceedances
  p <- mean(above)
  # under the threshold, use the empirical CDF
  # see ?ecdf
  ecdf_below <- ecdf(y[!above])
  empirical <- ecdf_below(y[!above])
  # above, apply cdf
  theoretical <- cdf(y[above], ...)
  
  transformed <- numeric(length = length(y))
  # "Glue together" the empirical and theoretical parts by rescaling them:
  
  #        empirical          e.g. N(0,1)
  # [-----------------------|-------------]
  # 0                      1-p            1
  
  transformed[!above] <- (1 - p) * empirical    # (1)
  transformed[above] <- 1 - p + p * theoretical # (2)
  
  return(list(
    transformed = transformed,
    ecdf = ecdf_below,
    prop = p
  ))
}

inverse_transform <- function(u, ecdf, quantile_function, p, ...) {
  # ...: arguments to quantile_function
  # Basically, undo the transformations (1-2) above
  above <- u > 1 - p
  original_scale <- numeric(length = length(u))
  original_scale[!above] <- quantile(ecdf, u[!above] / (1 - p))
  original_scale[above] <-
    quantile_function((u[above] - (1 - p)) / p, ...)
  return(original_scale)
}

# Example: Normal distribution
y <- rnorm(1000)
unif <- transform_to_uniform(y, pnorm, 1)
original_scale <- with(unif, inverse_transform(transformed,
                                               ecdf,
                                               qnorm,
                                               p = prop))
plot(y, original_scale)
