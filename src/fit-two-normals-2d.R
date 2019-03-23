# This function estimates the parameters of a bivariate two-mixture finite
# mixture model.
# Input is a 2-column matrix
fit2normals2d <- function(X) {
  x <- X[, 1]
  y <- X[, 2]
  # Initialize guesses:
  lambda <- runif(1)
  mu <- list(c(mean(x) - sd(x), mean(x) + sd(x)),
             c(mean(y) - sd(y), mean(y) + sd(y)))
  sigma <- list(diag(c(sd(x)/2, sd(y)/2)),
                diag(c(sd(x)/2, sd(y)/2)))
  LL <- 0
  dist <- 1
  iter <- 0
  maxIter <- 500
  while (dist > 1e-32) {
    iter <- iter + 1
    # E-step:
    # Calculate membership weights given theta (Bayes' rule):
    p1 <- dmvnorm(X, mu[[1]], sigma[[1]])
    p2 <- dmvnorm(X, mu[[2]], sigma[[2]])
    w <- p1*lambda/(p1*lambda + p2*(1 - lambda))

    # M-step:
    # Find new mixing parameter, means and standard deviations:
    new.lambda <- mean(w)
    new.mu <- list((1/sum(w))*colSums(w*X), (1/sum(1-w))*colSums((1-w)*X))
    new.sigma <- list(cov.wt(X, w)$cov, cov.wt(X, 1-w)$cov)
    # Find the change in log likelihoods:
    new.LL <- sum(log(new.lambda * p1 + (1 - new.lambda) * p2))
    dist <- abs(LL - new.LL)

    # Update parameters
    lambda <- new.lambda
    mu <- new.mu
    sigma <- new.sigma
    LL <- new.LL
    if (iter == 1000) {
      dist <- 0
      warning("Iteration limit (1,000) exceeded")
    }
  }
  return(list(lambda = lambda, mu = mu, sigma = sigma, posterior = w,
              iter = iter, logLikelihood = LL, convergenceDistance = dist))
}
