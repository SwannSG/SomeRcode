#install.packages("distrRmetrics")
#library(distrRmetrics)

#install.packages("nortest")
#library(nortest)


set.seed(567)

N = rnorm(10000,mean=10,sd=2)
U = runif(10000)


# generate skewed population
P = rsnorm(10000,mean=10, sd=2, xi = -1.5)
hist(P)
P.mean = mean(P)
P.sd = sd(P)

P = U

# check P for Normal Distribution
qqnorm(P)
qqline(P)

# test P for normality

# Anderson-Darling normality test
# A ~ 0 indicates normality
#ad.test(P)

# Cramer-von Mises normality test
# W ~ 0 indicates normality
#cvm.test(P)

# draw simulated samples repeatedly from population P
# sample statistic
# mean ~ N(mean=sample_mean, sd=se=sample_mean/sqrt(sample.size))
# sd

sample.size = 100
no.of.samples = 10000
means = c()
sds = c()
i = 1
while (i <= no.of.samples) {
  s = sample(P, sample.size, replace=FALSE)
  means = c(means, mean(s))
  sds = c(sds, sd(s))
  i = i + 1
}

# can we generate distribution of sample statistic mean using bootstrap
# draw one sample from population
small.sample = 30
s = sample(P, small.sample, replace=FALSE)
means = c()
i = 1
means = c()
while (i <= 100) {
  s.sub_sample = sample(s, size=small.sample*10, replace=TRUE)
  means = c(means, mean(s.sub_sample))
  i = i + 1
  }  




