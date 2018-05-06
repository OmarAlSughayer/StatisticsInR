# Omar AlSughayer, 1337255
# Math 390, HWS8

# lec25
# 9.16, a) 

d42 = c(2.62, 2.99, 3.39, 2.86)
d36 = c(3.47, 3.85, 3.77, 3.63)
d31 = c(4.78, 4.41, 4.91, 5.06)
k = 3
n = 4

grand_mean = 1/3*(mean(d42) + mean(d36) + mean(d31))

SSTr = length(d42)*(mean(d42) - grand_mean)^2 + length(d36)*(mean(d36) - grand_mean)^2 + length(d31)*(mean(d31) - grand_mean)^2
SSTr_df = k - 1

MSTr = SSTr/(k - 1)

MSE = (1/k) * (sd(d42)^2 + sd(d36)^2 + sd(d31)^2)
MSE_df = k*(n - 1)

SSE = MSE_df*MSE

SST = SSTr + SSE

df = n*k - 1

# the table is the same
# the p-value is 0.000 because it has few degrees of significance 

# b) 
# p-value < 0.01 = alpha -> we reject H0 in favor of Ha -> there is a diffrence between at least two means


# lect26
# lec_1

# a) sigma_epsilon = sigma of error = 1

# b)
x = runif(100, 0, 1)
Sxx = sum( (x - mean(x))^2 )

betas = numeric(5000)

for(i in c(1:5000)){
  error = rnorm(100, 0, 1)
  y = 10 + 2*x + error 
  Sxy = sum((x - mean(x))*(y - mean(y)))
  B = Sxy/Sxx
  betas[i] = B
}

hist(betas, main = 'Lec26, lec_1, b')

# c) mean is supposed to be close to 2, and it is actually close to it (2.005 in a sample run)
# d) standard deviation is supposed to be close to sigma_epsilon/sqrt(Sxx) = 1/sqrt(7.9) = 0.35 and it is. 
# e) 
qqnorm(betas, main = 'Normal Q-Q Plot, Lec26, lec_1, e')
abline(2, 1/sqrt(Sxx), col = 'red')
