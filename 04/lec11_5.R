# Omar AlSughayer, 1337255
# Math 390, HWS4, HW11

# lec_11_5
# a) generate a 100 cases of a univorm random variable between -1 and 1
x = runif(100, -1, 1)

# b) genarate error and y
error = rnorm(100, 0, 0.5)
y = 2 + 3*x + error

# c) do regression on x and y
predictions = lm(y ~ x)
y_hat = predictions$fitted.values

# d) compute 
very_zero = sum((y_hat - mean(y))*(y - y_hat)) # = -4.125606e-15
