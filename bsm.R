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

pop.mean = 25
pop.sd =5
P = rnorm(n=100000, mean=pop.mean, sd=pop.sd) 

no.of.random.samples = 1000
size.of.sample = 100
confidence.interval = 0.60


bad.samples.counts = vector('numeric')
no.of.bad.samples.counts = 1000

j = 1
while (j <= no.of.bad.samples.counts) {
  bad.samples.count = 0
  bad.samples = matrix(data=NA, nrow=500, ncol=4)
  colnames(bad.samples) = c('s.mean', 's.sd', 'lb', 'ub')
  i = 1
  while (i <= no.of.random.samples) {
    s = sample(P, size=size.of.sample, replace=FALSE)
    ci = pop.mean.ci(s, confidence.interval)
    if (!is.on.interval(pop.mean, ci)) {
        # population mean is not on the confidence interval
        bad.samples.count = bad.samples.count + 1
  #       bad.samples[bad.samples.count,'s.mean'] = mean(s)
  #       bad.samples[bad.samples.count,'s.sd'] = sd(s)
  #       bad.samples[bad.samples.count,'lb'] = ci[1]
  #       bad.samples[bad.samples.count,'ub'] = ci[2]
    }
    i = i + 1
  }
  # print (bad.samples.count)
  # print (100*bad.samples.count/no.of.random.samples)
  bad.samples.count.percent = 100*bad.samples.count/no.of.random.samples
  bad.samples.counts = c(bad.samples.counts, bad.samples.count.percent)
  j = j + 1
}

#descdist(bad.samples.counts, discrete = FALSE)
#fit.norm <- fitdist(bad.samples.counts, "norm")
#plot(fit.norm)