# Omar AlSughayer, 1337255
# Math 390, HWS4, HW13

# lec_13_1

# a) 
# get the data
dat = read.table('soil_data.txt', header = T)

# extract the desired row
x1 = dat[,2]
x2 = dat[,3]
y = dat[,4]

# perform regression
reg = lm(y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2))

# Coefficients:
# (Intercept)           x1           x2      I(x1^2)      I(x2^2)   I(x1 * x2)  
# -140.22976    -16.47521     12.82710      0.09555     -0.24339      0.49864 

# b) interpret the regression coeffiecients 
# the usual explanation of the coeffiecient of x is that it is the amount y changes if x changed by one
# but the existence of the term x1*x2 makes this understanding harder to grasp 
# an alternative way to look at it, is that a the absolute value of a coeffient shows how important a term is in determining y
# for example, |x1.coeff| > |x2.coeff| which means that c1 is more important, roughly, in determining y than x2 is.
# similarly, since the coefficient of x1^2 is quite small, we can say it is not that important of a term. 

# c-d) compute R2 and se
anova(reg)
summary(reg)  # R2 = 0.6037
              # se = 7.023

# e) plot residuals vs predicted
y_pred = reg$fitted.values
res_y = y - y_pred 
plot(y_pred, res_y, main = 'Residuals vs predicted y values')
# we can hardly say anything from the plot, except that out module does not seem to be biased (i.e. always overestimates or the opposite)

# f) perform regression with only x1 and x2
reg = lm(y ~ x1 + x2)

# Coefficients:
#   (Intercept)           x1           x2  
# 14.8893       0.6607      -0.0284  

# g) compute R2 and explain what it says about goodness of fit
anova(reg)  
summary(reg)  # R2 = 0.3465
# R2 suggest that only 35% of the data is explainable by our model
# so our model's goodness of fit is about that much

# h) compare ethe two R2 values
# 0.6037 is much larger than 0.3465 (almost double). Which should suggest that the previous model explains more. 
# however, R2 always increases the more you make your model. So a strict increase does not 
# always mean that introduced terms are useful. 
# moreoever, comparing the terms' coefficients shows that the newly added terms hardly even matter compared
# to x1 and x2. 

# i) compute se 
summary(reg)  # se = 0.447

# 7.023 < 0.447, which again suggests that the newly added terms just increased the deviation from the prediction. 

####################################################################################

# lec13_2 

# generate data

n = 100
x1 = runif(n, 0, 1)
x2 = runif(n, 0, 1)

# a) create y
error = rnorm(n, 0, 0.5)
y = 2 + 3*x1 + 4*x2 + error 

# fit the model
reg = lm(y ~ x1 + x2)
summary(reg)  # R2 = 0.9146
              # se = 0.4542

# b) add a term to y then refit the data
error = rnorm(n, 0, 0.5)
y = 2 + 3*x1 + 4*x2 + 50*x1*x2 + error

# fit the model 
reg = lm(y ~ x1 + x2)
summary(reg)  # R2 = 0.8702
              # se = 4.278

# c) fit the model with an extra x1*x2 term 
reg = lm(y ~ x1 + x2 + x1*x2)
summary(reg)  # R2 = 0.9984 
              # se = 0.4722


# d) the easy reason why c is better than b is because the model we are fitting in c is closer to the actual model. 
#     Also, the model in b fits a plane in 3d, while the data itself does not lay on one plan, but a curved plane, 
#     which is better captured by the curved plane fit in c. 
  
