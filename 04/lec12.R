# Omar AlSughayer, 1337255
# Math 390, HWS4, HW12

# lec_11_1

# given data
x= c(45, 58, 71, 71, 85, 98, 108)
y = c(3.20, 3.40, 3.47, 3.55, 3.60, 3.70, 3.80)

# a) compute the equation of the OLS fit
# computing beta
b = (mean(x*y) - mean(x)*mean(y))/(mean(x^2) - (mean(x))^2) # = 0.008821631
# computing alpha
a = mean(y) - b*mean(x) # = 2.855944

# compute the equation and its values
y_predicted = a + b*x 

# b) compute the total variation SST
sst = sum((y - mean(y))^2) # = 2.855944

# c) decompose into explained and unexplianed 
explained = sum((y_predicted - mean(y))^2)
unexplained = sum((y - y_predicted)^2)

# d) compute R2 and interpret it 
R2 = explained/sst # = 0.9614587
# R2 means that 96.14587% of the variation in y is explained by x

# e) compute the standard deviation of errors 
se = sqrt((unexplained)/(length((unexplained) - 2))) # = 0.09546975
# se means that the typical deviation of the y values about the fit is about 0.095 

####################################################################################

# lec_12_2

# a)
# get the data
dat0 = read.table('bias_0_data.txt', header = T)

# extract the desired row
x0 = dat0[,1]
y0 = dat0[,2]

# perform regression to predict y from x
reg0 = lm(y0 ~ x0)
y0_hat = reg0$fitted.values 

# make a scatter plot and fit a line
plot(y0, y0_hat, xlim = c(-6, 6), ylim = c(-6, 6), main = 'bias0 data')
abline(0, 1)

# b) 
# get the data
dat1 = read.table('bias_1_data.txt', header = T)

# extract the desired row
x1 = dat1[,1]
y1 = dat1[,2]

# perform regression to predict y from x
reg1 = lm(y1 ~ x1)
y1_hat = reg1$fitted.values 

# add the points 
points(y1, y1_hat, col = 'red')

# c) why is the data in red constantly shited up? 
# because bias_1_data.txt inclues one extra point (0, 100) that changes the means of x and y 
# which causes the new intercept to be greater and the new slop to be smaller

####################################################################################

# lec_12_3 

# a) 
# get the data
dat = read.table('transform_data.txt', header = T)

# extract the desired row
x = dat[,1]
y = dat[,2]

# make a scatted plot of y vs x
plot(x, y, main = 'transform data')

# transform the data 
x_t = sqrt(x)
y_t = sqrt(y)

# make a scatter plot of the transformed data
plot(x_t, y_t, main = 'square roots of transform data') 

# b) 
# perform a regression of the transformed data
reg_t = lm(y_t ~ x_t)

# fit the regerssion on the plot
abline(reg_t)

# c) 
# fit a regression line on the form a + b_1 * sqrt(x) + b_2 * x to the original
reg = lm(y ~ I(x ^ 0.5) + x)

# d) 
# compare the prediction of both models 
plot(sqrt(reg$fitted.values), reg_t$fitted.values,
     xlab = 'transformed predicted valeus', ylab = 'predicted transformed values',
     main = 'comparing the regression of original and transformed data')
# we see that the scatter plot is a straight line which shows that boths predictions are roughly the same

####################################################################################

# lec_12_4

# a) 
# get the data
dat = read.table('sin_data.txt', header = T)

# extract the desired row
x = dat[,1]
y = dat[,2]

# plot the data
plot(x, y, main = 'sin data') 

# b) 
# make a sin funciton
range = c(1:100)
sin_y = sin(2*pi*range / 24)
points(range, sin_y, col = 'red')

# c)
# take the difference and plot it 
diff_y = y - sin_y
plot(x, diff_y, ylab = 'y - sin(2pi*x/24)', main = 'sin_data.y  - sin[2pix/24] from 0 to 100')

# d) draw an OLS line through the data in part (c)
reg = lm(diff_y ~ x)
abline(reg)

# e) find R2 and se and interpret them
anova(reg)
summary(reg)  # R2 = 0.9763
              # se = 0.4573
# this means that 97.63% of the deviation in diff_y is explained by x 
# and that the typical deviation of diff_y about the regression line is 0.4573


