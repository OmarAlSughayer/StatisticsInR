# Omar AlSughayer, 1337255
# Math 390, HWS1 - HW2 - 1.12

# get the data, assuming that it exists in a file called y.txt
dat <- read.table('y.txt', header = F)

# extract the desired row
y <- dat[, 1]

# another way of getting the data is to construct a table as such 
# y     | 0  | 1  | 2 | 3 | 4 | 5 |
# freq  | 17 | 22 | 6 | 1 | 0 | 1 |


# plot the histogram
hist(y, breaks = seq(-1, 5, by = 1))
hist(y, breaks = 1000)

# a)  the propotion with no culs-de-sac is 17/47 = 0.36
#     the propotion with at least one is 1 - 17/47 = 0.64


# get the data, assuming that it exists in a file called z.txt
dat <- read.table('z.txt', header = F)

# extract the desired row
z <- dat[, 1]

# another way of getting the data is to construct a table as such 
# z     | 0  | 1  | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
# freq  | 13 | 11 | 3 | 7 | 5 | 3 | 3 | 0 | 2 |


# plot the histogram
hist(z, breaks = seq(-1, 8, by = 1))

# b)  the propotion with at most five culs-de-sac is (13 + 11 + 3 + 7 + 5 + 3)/47 = 0.89
#     the propotion with fewer than five intersections (13 + 11 + 3 + 7 + 5)/47  = 0.83

###############################################################################

# Omar AlSughayer, 1337255
# Math 390, HWS1 - HW2 - 1.16

# get the data, assuming that it exists in a file called idt.txt
dat <- read.table('idt.txt', header = F)

# extract the desired row
idt <- dat[, 1]

#plot 
hist(idt, breaks = seq(10, 80, by=10))

# take the log
log_idt <- log10(idt)

# plot
hist(log_idt, breaks = seq(1.1, 2.0, by=0.1))

# what does taking the log do to the data? It makes it less skewed

