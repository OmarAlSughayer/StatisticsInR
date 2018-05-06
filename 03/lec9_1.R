# Omar AlSughayer, 1337255
# Math 390, HWS3, HW9

# lec_9_1 and lec9_2
# get the data
dat = read.table('dataset.txt', header = F)

# extract the desired row
income = dat[,2]
age = dat[,3]
gender = dat[,4]
race = dat[,5]

# plot qqplot of income
qqnorm(income, cex=0.5, main = "Normal Q-Q Plot of Income")

# plot the qqplot of age
qqnorm(age, cex=0.5, main = "Normal Q-Q Plot of age")

# plot the scatterplot 
plot(age, income, main="Scatter Plot of Income vs Age")

# 2.48

x = c(.77, 1.20, 3.00, 1.62, 2.81, 2.48, 1.74, .47, 3.09, 1.31, 1.87, .96, .81, 1.43, 1.51, .32, 1.18, 1.89,
      1.20, 3.37, 2.10, .59, 1.35, .90, 1.95, 2.20, .52, .81, 4.75, 2.05)

# a) plot normal qqplot
qqnorm(x, main = "Normal Q-Q Plot 2.48.a")

# b) plot the normal qqplot of the squared values
x2 = x^2
qqnorm(x2, main = "Normal Q-Q Plot 2.48.b")

# b) plot the normal qqplot of the cubed values
x3 = x^3
qqnorm(x3, main = "Normal Q-Q Plot 2.48.c")

