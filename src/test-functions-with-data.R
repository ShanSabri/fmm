if (!require(pacman)) install.packages("pacman")
pacman::p_load(Cairo, fields, ggplot2, gridExtra, MASS, mixtools, reshape2)

source("fit-two-normals-1d.R")
source("find-two-means.R")
source("fit-two-normals-2d.R")

######################################################
# Fit a mixture of two normals to 1-dimensional data #
######################################################

# Draw the data:
N <- 10000
df <- data.frame(g = rbinom(N, 1, 0.4))
df$x <- df$g*rnorm(N, mean = 0, sd = 0.5) + (1-df$g)*rnorm(N, mean = 1, sd = 1)
df$Type <- factor(ifelse(df$g == 1, "A", "B"))

# Plot the actual data:
df$type <- "Data"
f <- ggplot(df, aes(x = x, fill = type)) + geom_density(alpha = 0) +
     ggtitle("Observed density") + theme(legend.title = element_blank())
g <- ggplot(df, aes(x = x , fill = Type)) + geom_density(alpha = 0.5) +
     ggtitle("Densities of each of the unknown types")

# Fit a two-mixture finite mixture model to the data:
fmm <- fit2normals1d(df$x)

# Draw types based on the posterior propbabilities
df$Type <- factor(ifelse(rbinom(N, 1, fmm$posterior) == 1, "A", "B"))

h <- ggplot(df, aes(x = x, fill = Type)) + geom_density(alpha = 0.5) +
     ggtitle("Densities of the predicted types from finite mixture model")

draws = rbinom(N, 1, fmm$lambda)
fit <-
  data.frame(x = draws * rnorm(N, mean = fmm$mu[1], sd = fmm$sigma[1]) +
                 (1 - draws) * rnorm(N, mean = fmm$mu[2], sd = fmm$sigma[2]))
fit$Type <- "Estimated"
data <- data.frame(x = df$x, Type = "Data")
fit <- rbind(fit, data)

i <- ggplot(fit, aes(x = x, fill = Type)) + geom_density(alpha = 0.5) +
     ggtitle("Estimated finite mixture model density versus the data") +
     theme(legend.title = element_blank())
cairo_pdf("fmm1d.pdf", height = 14, width = 14)
plot(arrangeGrob(f, g, h, i, ncol = 2))
dev.off()

##############################################################################
# Find both means of clustered two-dimensional data and fit a mixture of two #
# bivariate normals                                                          #
##############################################################################

# Draw x and y from one normal with probability 0.3 and a different one
# with probability 0.7:
N <- 1000
df <- data.frame(group = rbinom(N, 1, 0.3))
Mu <- list(c(1.5, 1), c(-0.4, -1))
Sigma <-
  list(matrix(diag(c(1, 0.9)),2, 2), matrix(c(0.8, -0.5, -0.5, 1), 2, 2))
X <-  (df$group) * mvrnorm(N, Mu[[1]], Sigma[[1]]) +
  (1 - df$group) * mvrnorm(N, Mu[[2]], Sigma[[2]])

# Plot actual data:
df$x <- X[, 1]
df$y <- X[, 2]
df$Type <- factor(df$group)
df$Data <- "Data"
levels(df$Type) <- c("A", "B")
f <- ggplot(df, aes(x = x, y = y, fill = Data)) + geom_point() +
     ggtitle("Observed data") +
     theme(legend.title = element_blank())
g <- ggplot(df, aes(x = x, y = y, colour = Type)) + geom_point() +
     ggtitle("Unobserved types within actual data")

# Two means clustering:
my2means <- find2means(X)
df$Cluster <- factor(my2means$cluster)
levels(df$Cluster) <- c("A", "B")
h <- ggplot(df, aes(x = x, y = y, colour = Cluster)) + geom_point() +
     ggtitle("Two Means Clustering")
cairo_pdf("find-two-means.pdf", height = 7, width = 21)
plot(arrangeGrob(f, g, h, ncol = 3))
dev.off()

# Two-mixture finite mixture model:
fmm <- fit2normals2d(X)
df1 <- data.frame(group = rbinom(N, 1, fmm$posterior))
df1$x <- df$x
df1$y <- df$y
df1$Type <- factor(df1$group)
levels(df1$Type) <- c("A", "B")
h <- ggplot(df1, aes(x = x, y = y, colour = Type)) + geom_point() +
     ggtitle("Guess of type using posterior probabilities from
             multivariate finite mixture model")
df2 <- data.frame(group = rbinom(N, 1, fmm$lambda))
X <-  (df2$group) * mvrnorm(N, fmm$mu[[1]], fmm$sigma[[1]]) +
  (1 - df2$group) * mvrnorm(N, fmm$mu[[2]], fmm$sigma[[2]])
df2$x <- X[, 1]
df2$y <- X[, 2]
df2$Type <- factor(df2$group)
levels(df2$Type) <- c("A", "B")
i <- ggplot(df2, aes(x = x, y = y, colour = Type)) + geom_point() +
     ggtitle("Draws from estimated finite mixutre model")
cairo_pdf("fmm2d.pdf", height = 14, width = 14)
plot(arrangeGrob(f, g, h, i, ncol = 2))
dev.off()
