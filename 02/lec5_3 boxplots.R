# Omar AlSughayer, 1337255
# Math 390, HWS2

# get the data
dat = read.table('dataset.txt', header = F)

# extract the desired row
income = dat[,2]
age = dat[,3]
gender = dat[,4]
race = dat[,5]


# Question lect5-3
# plot income 
boxplot(income, ylab = "in thousand of dollars", xlab = "income")
# plot age
boxplot(age, ylab= "in years", xlab = "age")

# Question lect6_1
# plot gender vs income
boxplot(income[gender == 'M'], income[gender == 'F'],
        main = "Males vs Females Income", xlab = "Gender", ylab = "in thousand of dollars",
        names = c("Male", "Female"))



