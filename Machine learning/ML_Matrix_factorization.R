library(tidyverse)
set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# Q1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}
my_image(y)
my_image

#Q2

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q3

s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# Compute the sum of squares of the columns of  Y  and store them in ss_y.
# Then compute the sum of squares of columns of the transformed  YV  and
# store them in ss_yv. Confirm that sum(ss_y) is equal to sum(ss_yv).

# What is the value of sum(ss_y) (and also the value of sum(ss_yv))?

ss_y <- colSums(y^2)
ss_yv <- colSums( (y%*%s$v)^2 )
max(abs(ss_y-ss_yv))
sum(ss_y)
sum(ss_yv)

#Q4
# plot ss_y against the column number and then do the same for ss_yv.
qplot(1:24,ss_y)
qplot(1:24,ss_yv)

#♦Q5
# Now notice that we didn't have to compute ss_yv because we already have the answer.
# How? Remember that  YV=UD  and because  U  is orthogonal,
# we know that the sum of squares of the columns of  UD  are the diagonal entries of 
# D  squared.
# Confirm this by plotting the square root of ss_yv versus the diagonal 
# entries of  D .
qplot(s$d, sqrt(ss_yv) )

#Q6
# So from the above we know that the sum of squares of the columns of  Y 
# (the total sum of squares) adds up to the sum of s$d^2 and that the 
# transformation  YV  gives us columns with sums of squares equal to s$d^2.
# Now compute the percent of the total variability that is explained by just
# the first three columns of  YV .
# 
# What proportion of the total variability is explained by the first three
# columns of  YV ?


#Q7
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#Q8
hh <- sweep(s$u, 2, s$d, FUN = "*")[,1]
qplot(sweep(s$u, 2, s$d, FUN = "*")[,1],rowMeans(y))

my_image(s$v)


plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))