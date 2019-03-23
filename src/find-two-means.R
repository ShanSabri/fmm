# This function finds the means of two clusters of two-dimensional data.
# Input is a 2-column matrix
find2means <- function(x) {
  # Initialize with guess of the means:
  means <- list(a = c(mean(x[, 1])+1, mean(x[, 2])+1),
                b = c(mean(x[, 1])-1, mean(x[, 2])-1))
  dist <- 1
  while (dist > 1e-32) {
    # E-step - Find the closest mean for each point in the data:
    dist.from.a <- apply(X, 1, function(z) dist(rbind(z, means$a)))
    dist.from.b <- apply(X, 1, function(z) dist(rbind(z, means$b)))
    assign.to.a <- ifelse(dist.from.a < dist.from.b, 1, 0)

    # M-step - Find the new means:
    new.means <- list(a = colSums(x * assign.to.a)/sum(assign.to.a),
                      b = colSums(x * (1-assign.to.a))/(N-sum(assign.to.a)))

    # Find distance between new and old means:
    dist <- norm(matrix(unlist(means) - unlist(new.means)))
    means <- new.means
  }
  return(list(means = means, cluster = assign.to.a))
}
