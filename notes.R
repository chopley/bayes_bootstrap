http://sumsar.net/blog/2015/07/easy-bayesian-bootstrap-in-r/#the-bayesboot-function
http://rsnippets.blogspot.com/2012/11/simple-bayesian-bootstrap.html
http://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/
http://www.sumsar.net/blog/2015/07/easy-bayesian-bootstrap-in-r/


library(gtools)

x <- cars$dist #sample distribution - length = 50
n <- 100 #resample size
bb <- rdirichlet(n, rep(1, length(x)))
#this matrix is like this:
1,1    1,2    1,3     1,4   1,50    
2,1     .      .      .     2,50
3,1     .      .      .     3,50
.       .      .      .     .
.       .      .      .     .
99,1                        99,50
100,1 100,2   100,3         100,50


dist <- apply(bb, 1, weighted.mean, x = x)

#the above is the same as below:
weighted.mean(x,bb[1,]) + weighted.mean(x,bb[2,]) + ... + weighted.mean(x,bb[50,])


# Bayesian bootstrap
mean.bb <- function(x, n) {
  apply(rdirichlet(n, rep(1, length(x))), 1, weighted.mean, x = x)
}

# standard bootstrap
mean.fb <- function(x, n) {
  replicate(n, mean(sample(x, length(x), TRUE)))
}

set.seed(1)
reps <- 100000
x <- cars$dist
system.time(fbq <- quantile((mean.fb(x, reps)), c(0.025, 0.075)))
system.time(bbq <- quantile((mean.bb(x, reps)), c(0.025, 0.075)))
