# Omar AlSughayer, 1337255
# Math 390, HWS3, HW10

# lec_9_4
# data
moe = c(29.8, 33.2, 33.7, 35.3, 35.5, 36.1, 36.2, 36.3, 37.5, 37.7, 38.7, 38.8, 39.6, 41.0, 42.8, 42.8,
        43.5, 45.6, 46.0, 46.9, 48.0, 49.3, 51.7, 62.6, 69.8, 79.5, 80.0)

str = c(5.9, 7.2, 7.3, 6.3, 8.1, 6.8, 7.0, 7.6, 6.8, 6.5, 7.0, 6.3, 7.9, 9.0, 8.2, 8.7, 7.8, 9.7, 7.4, 
        7.7, 9.7, 7.8, 7.7, 11.6, 11.3, 11.8, 10.7)

# a)  Plot a scatterplot of Strength vs. MOE. By computer
plot(moe, str, main = "MoE vs Strength (lec10_4_a)")

# b) Make a boxplot of MOE, and of Strength. By computer.
boxplot(moe, ylab = "MoE", main = "Box plot of MoE")
boxplot(str, ylab = "Strength", main = "Box plot of Strength")

# c) Make a qqplot of MOE, and of Strength. By computer.
qqnorm(moe, main = "Normal Q-Q Plot of Moe")
qqnorm(str, main = "Normal Q-Q Plot of Strength")

# d) Compute the correlation coefficient between MOE and Strength. By hand. You may use the computer to 
# compute sample means of necessary quantities,but you must use one of the formulas for r.
n = length(moe) # = length(str)
mm = mean(moe)
ms = mean(str)

num = sum((moe - mm)*(str - ms))
dom = sqrt(sum((moe - mm)^2)*sum((str - ms)^2))
r_hand = num/dom # = 0.8592721

# e) Compare it with the correlation you get from cor() in R.
r_com = cor(moe, str) # = 0.8592721
# after running the code we see that r_hand == r_com = 0.8592721

# f) Compute the equation of the OLS fit (i.e., the intercept and slope). 
# By hand.You may use the computer to compute sample means
# of necessary quantities,but you must use the formulas for OLS intercept and slope)

# mean of squared moe 
mms = mean(moe^2)
# mean of multipication 
mxy = mean(moe*str)

# beta
b = (mxy - mm*ms)/(mms - mm^2) # = 0.1074821
# alpha 
a = ms - b*mm # = 3.2925

# g) Interpret the slope.

# a change of 1 moe is associated with a change of b = 0.107 in strength unit

# h) Predict Strength when MoE is 39.0 . By hand.
prediction = a + b*39.0 # = 7.484304

# i) Compute the sum squared error (SSE, or SSResid). You may use the computer 
# to compute sample means of necessary quantities.

# the predictions of strength using moe
pre_str = a + b*moe

# the sum of squared errors
sse = sum((str - pre_str)^2) # = 18.7356


