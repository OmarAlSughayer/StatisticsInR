# Omar AlSughayer, 1337255
# Math 390, HWS2, HW7

#lec_2
# a) take a sample of the normal distribution of size 100 with mu = -1 and sigma = 2
x = rnorm(100, -1, 2)
# compute the mean and the standard devition
mean(x)
sd(x)

# b) make the density scale histogram for x
h = hist(x, plot = F)
plot(h$mids, h$counts, type = "l",
     main = "Normal Sample with mu = -1 and sigma = 2", xlab = "mids", ylab = "counts")
hist(x, add = T)

# 2.15 a
a = c(2006.1, 2065.2, 2118.9, 1686.6, 1966.9, 1792.5)
ssd = sd(a) 
sv = ssd*ssd

# lec8_1
# ab) compute p(x) for all possible values of of x in binomial(4, 1/4)
M = matrix(nrow = 1, ncol = 5)
e = 0
for(y in c(1:5)){
  M[,y] = dbinom(y-1, 4, 0.25)  
  e = e + M[,y]*(y-1)
}
# values for P(x)
M
# expected value
e 

# c) sample mean
mean(rbinom(100, 4, 0.25))

