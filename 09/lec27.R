# Omar AlSughayer, 1337255
# Math 390, HWS9

# lec_27_1
x = c(89, 177, 189, 354, 362, 442, 965)
y = c(.40, .60, .48, .66, .61, .69, .99)

# a) scatterplot
plot(x,y, main = 'Lec_27_1 (a)')

# b) R's estimate of b
est = lm(y ~ x)
b_hat = est$coefficients[2] # = 0.0006210758
a_hat = est$coefficients[1]

# c) s_e
n = length(x)
se = sqrt(sum(est$residuals^2)/(n - 2))
# the typical error in the estimate is se = 0.05404525

# d) C.I. for beta
sxx = sum((x - mean(x))^2)
t_star = 2.365 

l_CI = b_hat - t_star * se / sqrt(sxx) 
u_CI = b_hat + t_star * se / sqrt(sxx)
# (0.0004418288, 0.0008003229)

# e) confidence interval 
y_hat = a_hat + b_hat*250
sy_hat = se*sqrt(1/n + (250 - mean(x))^2/sxx)
y_hat - t_star * sy_hat
y_hat + t_star * sy_hat
# we are 95% confident that the true mean of y's given x = 250 is in the interval (0.5066346, 0.6121509)

# f) predction interval
y_hat - t_star * sqrt(sy_hat^2 + se^2)
y_hat + t_star * sqrt(sy_hat^2 + se^2)
# (0.4211154, 0.6976701)0.8
# about 95% of prediction intervals will cover a single y at x = 250 


##################################################################################3


# 11.62 
x = c(23.20, 23.50, 23.52, 24.30, 25.10, 26.20)
y = c(3.78, 4.12, 4.24, 5.35, 5.87, 6.02)
x = c(x, 27.40, 28.10, 29.30, 30.60, 31.50, 32.01)
y = c(y, 6.12, 6.41, 6.62, 6.43, 6.13, 5.92)
x = c(x, 32.63, 33.23, 33.62, 34.18, 35.43, 35.62)
y = c(y, 5.64, 5.45, 5.21, 4.98, 4.65, 4.50)
x = c(x, 36.16, 36.23, 36.89, 37.90, 39.10, 41.66)
y = c(y, 4.34, 4.03, 3.92, 3.65, 3.02, 2.89)

model = lm(y ~ x + I(x^2) + I(x^3))
summary(model)


# the output is not identical because the sample output is created using Minitap. not R
# however, the same values are calculated and displayed. 

