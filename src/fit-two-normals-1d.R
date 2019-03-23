# Fits a mixtures of two normals to 1-dimensional data.
# Input is a vector.
fit2normals1d <- function(x) {
  # Initialize guesses:
  lambda <- runif(1)
  mu <- c(mean(x) - sd(x), mean(x) + sd(x))
  sigma <- rep(sd(x)/2, 2)

  LL <- 0
  dist <- 1
  iter <- 0
  while (dist > 1e-32) {
    iter <- iter + 1
    # E-step:
    # Calculate membership weights given theta (Bayes' rule):
    p1 <- dnorm(x, mean = mu[1], sd = sigma[1])
    p2 <- dnorm(x, mean = mu[2], sd = sigma[2])
    w <- p1*lambda/(p1*lambda + p2*(1 - lambda))

    # M-step:
    # Find new mixing parameter, means and standard deviations:
    new.lambda <- mean(w)
    new.mu <- c((1/sum(w))*sum(w*x), (1/sum(1-w))*sum((1-w)*x))
    new.sigma <- c(sqrt((1/sum(w))*sum(w*(x-new.mu[1])^2)),
                   sqrt((1/sum(1-w))*sum((1-w)*(x-new.mu[2])^2)))
    # Find the change in log likelihoods:
    new.LL <- sum(log(new.lambda * p1 + (1 - new.lambda) * p2))
    dist <- abs(LL - new.LL)

    # Update parameters
    lambda <- new.lambda
    mu <- new.mu
    sigma <- new.sigma
    LL <- new.LL
  }
  return(list(lambda = lambda, mu = mu, sigma = sigma, posterior = w,
              iter = iter, logLikelihood = LL, convergenceTol = dist))
}
