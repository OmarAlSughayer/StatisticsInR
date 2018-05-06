# Omar AlSughayer, 1337255
# Math 390, HWS7


# lec_20_1
# get the data
dat = read.table('dataset.txt', header = F)

# extract the desired row
income = dat[,2]
age = dat[,3]
gender = dat[,4]
race = dat[,5]

# a) compute the t-based, 2-sided, 95% confidence interval
t.test(income ~ gender, conf.level = .95)
# t = 1.0757, df = 9.1787, p-value = 0.3095
# 95 percent confidence interval:
# (-34.03318  96.09568)

# b) is there evidence that the true means are different? 
# since the confidence inerval contains 0, we do not have evidence to suggest the tre means are different 


######################################################################


# lec_20_2 

x1 = c(-0.27, -0.14, 1.61, 0.09, 0.00, 2.07, 0.56, -1.67, -0.51, -0.54)
x2 = c(-0.32, 0.20, 1.93, 0.54, 0.75, 1.77, 0.84, -0.29, -0.33, 0.17)

# a) paired data confidence interval 
xd = x1 - x2
md = mean(xd)
sd = sd(xd) 

lc = md - 2.262*sd/sqrt(10) # = -0.7386533
uc = md + 2.262*sd/sqrt(10) # = -0.07334668

# b) non-paired dara confidence interval
x1m = mean(x1)
x2m = mean(x2)
s1 = sd(x1)
s2 = sd(x2)

lcn = (x1m - x2m) - 1.96*sqrt((s1^2 +s2^2)/ 10) # = -1.245294
ucn = (x1m - x2m) + 1.96*sqrt((s1^2 +s2^2)/ 10) # = 0.4332941

# we are 95% confident the true mean of x1 - x2 is in the given interval

# c) which is narrower? 
# as expected, paired is narrower (a)

######################################################################

# 7.51

x1 = c(418, 421, 421, 422, 425, 427,
      431, 434, 437, 439, 446, 447,
      448, 453, 454, 463, 465)
x2 = c(429, 430, 430, 431, 436, 437,
       440, 441, 445, 446, 447)

# a) draw comparative boxplots
boxplot(x1, x2, main = '7.51.a Comparative Boxplot', ylab = 'Degree of Polymerization', xlab = c('x1, x2'))
# it seems that x2 is fully contained within x1s quadrants, and has the almost the same mean

# b) calculate the 95% CL
x1m = mean(x1)
x2m = mean(x2)
s1 = sd(x1)
s2 = sd(x2)

lcn = (x1m - x2m) - 1.96*sqrt((s1^2 +s2^2)/ 10) # = -9.457767
ucn = (x1m - x2m) + 1.96*sqrt((s1^2 +s2^2)/ 10) # = 11.13691
# we cann't say that the two true means are different since the CL includes 0


######################################################################

# 7.55

before = c(15, 26, 66, 115, 62, 64)
after =  c(16, 24, 42, 80, 78, 73)
diff = after - before

md = mean(diff)
sd = sd(diff) 

lc = md - 2.571*sd/sqrt(6) # = -26.50196
uc = md + 2.571*sd/sqrt(6) # = 14.8353

# we are 95% confident the true mean of the difference in accidents before and after in in the given inerval

####################################################################### 

# 8.31

h = c(1.2, .9, .7, 1.0, 1.7, 1.7, 1.1, .9, 1.7,
      1.9, 1.3, 2.1, 1.6, 1.8, 1.4, 1.3, 1.9, 1.6,
      .8, 2.0, 1.7, 1.6, 2.3, 2.0)

p = c(1.6, 1.5, 1.1, 2.1, 1.5, 1.3, 1.0, 2.6)

# a) draw a qqplot 
qqnorm(h, main = '8.31.a for H')
qqnorm(p, main = '8.31.a for P')

# for both graphs, it is implausable that they coem from a standard normal

# b) draw a comparative boxplot
boxplot(h, p, main = '8.31.b', ylab = 'extensibility (%) at 100 gm/cm', xlab = c('H', 'P'))
# there is too much overlap to suggest that the true means are different 

####################################################################### 

# 8.41

con = c(.0011, .0014, .0018, .0022, .0010, .0016, .0028, .0020, .0015, .0014, .0023, .0017, .0020)
per = c(.0011, .0010, .0019, .0013, .0011, .0017, .0024, .0020, .0013, .0013, .0017, .0015, .0013)

diff = con - per

mean(diff)
sd(diff)
length(diff)

