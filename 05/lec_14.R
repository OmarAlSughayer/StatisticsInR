# Omar AlSughayer, 1337255
# Math 390, HWS5, HW14

# lec_14_1

# data 1
# get the data
dat = read.table('hw_3_dat1.txt', header = T)

# extract the desired row
x1 = dat[,1]
x2 = dat[,2]
y = dat[,3]

# regression fit
reg = lm(y ~ x1*x2)
summary(reg)  # R2 = 0.9353
              # sd = 2.017
plot(x1, x2)
# justification 
# x1 and x2 are not colinear 
# it is also clear from the graph that we got an interaction
# and the relationship between x1 and y, and x2 and y is not linear 

# data 2
# get the data
dat = read.table('hw_3_dat2.txt', header = T)

# extract the desired row
x1 = dat[,1]
x2 = dat[,2]
y = dat[,3]

# regression fit
reg = lm(y ~  x1*x2)
summary(reg)  # R2 = 0.9622
              # sd = 2.018

# justification 
# x1 and x2 are colinear, the relationship between them is almost linear 
# it is also clear from the graph that we got an interaction
# and the relationship between x1 and y, and x2 and y is not linear 


#################################################################################


# lec_14_2

# setting seed
set.seed(123)
n = 10
x1 = runif(n,-1,1)
y = 1 + 2*x1 + rnorm(n,0,1)

# a) make a scatterplot 
plot(x1, y, main = 'lec_14_2 (a)')

# b) perform linear regresssion and 
reg = lm(y ~ x1)
summary(reg)  # R2 =  0.4181
              # sd = 0.8648 

# c) generate more x's 
x2 = runif(n,-1,1)
x3 = runif(n,-1,1)
x4 = runif(n,-1,1)
x5 = runif(n,-1,1)

# d) perform multiple linear regressions 
reg = lm(y ~ x2)
summary(reg)  # R2 = -0.1242

reg = lm(y ~ x3)
summary(reg)  # R2 = -0.06785

reg = lm(y ~ x4)
summary(reg)  # R2 =  -0.1205

reg = lm(y ~ x5)
summary(reg)  # R2 = -0.06942


