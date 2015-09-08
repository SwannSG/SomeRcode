# install.packages(c('fitdistrplus', 'logspline'))
#library(fitdistrplus)
#library(logspline)

# returns population mean confidence interval
# pass sample of values and confidence level
pop.mean.ci = function(sample, level) {
  s.mean = mean(sample)
  s.sd = sd(sample)
  s.size = length(sample)
  se = s.sd/sqrt(s.size)
  lower.bound = qnorm(p=(1 - level)/2, mean=s.mean, sd=se)
  upper.bound = s.mean + (s.mean - lower.bound)
  return (c(lower.bound, upper.bound))
}

# True if value on interval
# Pass value, bounds=vector(lower.bound, upper.bound)
is.on.interval = function(value, bounds) {
  if ((value < bounds[1]) | (value > bounds[2])) {
    FALSE
  } else {TRUE}
}

mf = function(population, size.of.sample, no.of.random.samples,
              confidence.interval, no.of.repeats) {
  no.of.random.samples = 1000
  size.of.sample = 100
  confidence.interval = 0.80
  
  # repeats contains pop.mean.not.on.interval% for each repeat
  repeats = vector('numeric')
  no.of.repeats = 1000
  
  j = 1
  while (j <= no.of.repeats) {
    pop.mean.not.on.interval.count = 0
    i = 1
    while (i <= no.of.random.samples) {
      s = sample(population, size=size.of.sample, replace=FALSE)
      ci = pop.mean.ci(s, confidence.interval)
      if (!is.on.interval(pop.mean, ci)) {
        # population mean is not on the confidence interval
        pop.mean.not.on.interval.count = pop.mean.not.on.interval.count + 1
      }
      i = i + 1
    }
    repeats = c(repeats, 100*pop.mean.not.on.interval.count/no.of.random.samples)
    j = j + 1
  }
  return (repeats)
}  
  
pop.size = 1000000

pop.mean = 25
pop.sd =5
# population normal distribution
P = rnorm(n=pop.size, mean=pop.mean, sd=pop.sd) 

pop.min = -10
pop.max = 10
# population uniform distribution
P.uniform = runif(pop.size, min=pop.min, max=pop.max)

result_P_100_0.8 = mf(P, 100, 1000, 0.8, 1000)  
result_P_100_0.9 = mf(P, 100, 1000, 0.8, 1000)
result_P_100_0.99 = mf(P, 100, 1000, 0.8, 1000)
result_P_500_0.8 = mf(P, 500, 1000, 0.8, 1000)  
result_P_500_0.9 = mf(P, 500, 1000, 0.8, 1000)
result_P_500_0.99 = mf(P, 500, 1000, 0.8, 1000)

result_U_100_0.8 = mf(P.uniform, 100, 1000, 0.8, 1000)  
result_U_100_0.9 = mf(P.uniform, 100, 1000, 0.8, 1000)
result_U_100_0.99 = mf(P.uniform, 100, 1000, 0.8, 1000)
result_U_500_0.8 = mf(P.uniform, 500, 1000, 0.8, 1000)  
result_U_500_0.9 = mf(P.uniform, 500, 1000, 0.8, 1000)
result_U_500_0.99 = mf(P.uniform, 500, 1000, 0.8, 1000)





#descdist(counts, discrete = FALSE)
#fit.norm <- fitdist(counts, "norm")
#plot(fit.norm)