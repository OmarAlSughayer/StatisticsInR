
#  The following code is the code whose results I showed in class today.  It does 
# bootstrap to "find" the correct number if hidden nodes (here 6)? Run it first and 
# make sure it generates the figs shown in the lecture. There may be small differences
# in the labels and stuff, but that's not important.

# Now, you'll notice that the data that's read in has many predictors than just x1.
# The fact is that x2 = x1^2, x3 = x1^4, etc. In other words, all the predictor
# columns are just powers of the x1 column. And, of course, y is the response.
# Your hw is to revise this code so that instead of the model being a neural network 
# with the number of hidden nodes set to 0, 6, and 20, the model is lm() with
# the order of the polynomial set to 1 (i.e., simple linear regression), 5, and 9.
# Turn in the code AND the figs.

# Hint1: you need to revise/replace only the two lines that have mod.1 = ...
# and pred.1 = ... .
# Hint2: the following is how you would do quadratic regression: 
#     mod.1 = lm(y ~ ., dat[samp,1:3])      # 3, because y, x1, x1^2
#     pred.1 = predict(mod.1,newdata=dat[-samp,]) 

rm(list=ls(all=TRUE))
library(nnet)

n.boot = 100              # Number of bootstrap trials.
nhd = c(2,6,10)
skip = c("T","F","F")    # nnet needs to have skip=T when nhd=0.
wtdecay = 0.00           # Change to 0.01 to see how it "prevents" overfitting.
range = 30               # Range of the initial wts. Change to see what happens.  

dat = read.table("nn_hw_dat.txt",header=F, col.names=c("y","x1","x2","x3","x4","x5","x6","x7","x8","x9")) 
attach(dat)

n = nrow(dat) 
trn = matrix(nrow=length(nhd),ncol=n.boot)
vld = matrix(nrow=length(nhd),ncol=n.boot) 

for(model in 1:length(nhd) ){ 
  for(trial in 1:n.boot){        
    set.seed(model*trial)             # take a different sample for each model and trial.
    samp = c(sample(1:n,n,replace=T))   
    
    mod.1 = lm(dat$y ~ ., dat[samp,1:nhd[model]])
    pred.1 = predict(mod.1,newdata=dat[-samp,])
    
    trn[model,trial] = log10( mean( (mod.1$fitted-y[samp])^2 ))
    vld[model,trial] = log10( mean( (pred.1-y[-samp])^2 ) ) 
  }} # end of loop over trial and model 

png("dat.png")
plot(x1,y)
dev.off()

png("hist.png")
par(mfrow=c(3,1),mar=c(4,4,1,1))
for(model in 1:length(nhd)){
  hist(trn[model,],breaks=50, xlim = c(-6,6),main="" )
  hist(vld[model,],breaks=50,add=T, border=2)
}
dev.off()

X = c(rep(1,n.boot), rep(2,n.boot), rep(3,n.boot))
Y_trn = c(trn[1,], trn[2,], trn[3,])
Y_vld = c(vld[1,], vld[2,], vld[3,])
aov.trn = aov(Y_trn ~ as.factor(X)) 
aov.vld = aov(Y_vld ~ as.factor(X)) 
temp = summary(aov.trn)[[1]]
F_trn = temp[1,4]; pv_trn = temp[1,5]
temp = summary(aov.vld)[[1]]
F_vld = temp[1,4]; pv_vld = temp[1,5]

png("boxplot.png")
par(mfrow=c(2,1),mar=c(4,4,1,1))
boxplot(trn[1,],trn[2,],trn[3,],xlab="Model",ylab="MSE_trn",names=c(1:3) ,range=0) 
boxplot(vld[1,],vld[2,],vld[3,],xlab="Model",ylab="MSE_vld",names=c(1:3) ,range=0)
mtext(paste("F,p-value = ",round(F_trn,2),", ", round(pv_trn,2),sep=""),col=2, line=+7, at=1)
mtext(paste("F,p-value = ",round(F_vld,2),", ", round(pv_vld,2),sep=""),col=2, line=-2, at=1)
dev.off()

library(stats)
png("tukey.png")
tuk.1 = TukeyHSD(aov.vld, conf.level=0.99); 
plot(tuk.1) 
abline(v=0)
dev.off()

