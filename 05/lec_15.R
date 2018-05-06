# Omar AlSughayer, 1337255
# Math 390, HWS5, HW15

# lec_15_1

# a) b) 
ntrial = 5000
xbarMax = numeric(ntrial)
xbarMin = numeric(ntrial)
for( trial in 1:ntrial ){
  x = rnorm(50, 0, 1)
  xbarMax[trial] = max(x)
  xbarMin[trial] = min(x)
}

hist(xbarMax, main="Sampling Distribution of Sampling Max")
hist(xbarMin, main="Sampling Distribution of Sampling Min")

############################################################################

# lec_15_2

ntrial = 5000
xbar = numeric(ntrial)
for( trial in 1:ntrial ){
  x = rexp(100, 2)
  xbar[trial] = mean(x)
}

qqnorm(xbar)

############################################################################

# lec_15_3

