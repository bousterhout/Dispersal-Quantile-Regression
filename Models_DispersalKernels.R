##########################
# Analysis
# Dispersal kernels
# BHO
# 1.5.18
##########################

#Load data
source('DataSteps_BodyCondPCA.R')

#Load functions
source('Functions.R')

#Load libraries
packages <- c('fitdistrplus', 'actuar', 'statmod', 'boot', 'agricolae', 'AICcmodavg')
package.check(packages)


move$lWk1NetDist.Rate<-log(1+move$Wk1NetDist.Rate)

library(fitdistrplus)
library(actuar)
library(statmod)
library(boot)
library(agricolae)
library(AICcmodavg)

#References: van Houtan et al 2007, CH 15 in Dispersal Ecology and Evolution

#Can define own functions in this package 
#https://tinyurl.com/yamqck5w
######################
# Kurtosis and CI
######################

kurtosis.f <- function(d, i) {
                d2<-d[i,]
                return(kurtosis(d2$Wk1NetDist.Rate))
}


kurtosis.boot<-boot(move, kurtosis.f, R =1000)

boot.ci(kurtosis.boot) # Use percentile estimates

##############################
# Dispersal kernel AIC table
##############################

k.table <- data.frame(Data=character(),
                 Guassian=numeric(), 
                 Weibull=numeric(),
                 LogNormal = numeric(),
                 Gamma = numeric(),
                 Exponential = numeric(),
                 Cauchy = numeric(),
                 Logistic = numeric(),
                 LogLogistic = numeric(),
                 InvGaussian = numeric(),
                 Burr = numeric(),
                 stringsAsFactors=FALSE) 
################
# Full data set
################

descdist(move$Wk1MovtRate, discrete=F)
norm<-fitdist(move$Wk1MovtRate, 'norm')
weibull<-fitdist(move$Wk1MovtRate, 'weibull')
lnorm<-fitdist(move$Wk1MovtRate, 'lnorm')
gamma<-fitdist(move$Wk1MovtRate, 'gamma')
exp <-fitdist(move$Wk1MovtRate, 'exp')
cauchy <-fitdist(move$Wk1MovtRate, 'cauchy')
logis <-fitdist(move$Wk1MovtRate, 'logis')
llogis <-fitdist(move$Wk1MovtRate, 'llogis')
invgaus<-fitdist(move$Wk1MovtRate, 'invgauss', start=list(mean=1, shape=1, dispersion=1))
burr <- fitdist(move$Wk1MovtRate, 'burr', start=list(shape1 = 0.3, shape2 = 1, rate=1))

k.table[1,1]<-"All"
k.table[1,2]<-norm$aic
k.table[1,3]<-weibull$aic
k.table[1,4]<-lnorm$aic
k.table[1,5]<-gamma$aic 
k.table[1,6]<-exp$aic
k.table[1,7]<-cauchy$aic
k.table[1,8]<-logis$aic
k.table[1,9]<-llogis$aic
k.table[1,10]<-invgaus$aic
k.table[1,11]<-burr$aic #Best fit by quite a bit for whole data set

plot(burr)
plot(llogis)
plot(gamma)
plot(weibull)

llogis.b <-bootdist(llogis, niter=1001)
summary(llogis.b)

#Figures
cdfcomp(list(norm, weibull, exp, llogis), legendtext=c("Gaussian", "Weibull", "Exp", 'Log-logistic'))
denscomp(list(norm, weibull, exp, llogis), legendtext=c("Gaussian", "Weibull", "Exp", 'Log-logistic'))

################
# By habitat data
################
grass<-subset(move, Habitat=="Field")
forest<-subset(move, Habitat=="Forest")

descdist(grass$Wk1MovtRate, discrete=F)
norm<-fitdist(grass$Wk1MovtRate, 'norm')
weibull<-fitdist(grass$Wk1MovtRate, 'weibull')
lnorm<-fitdist(grass$Wk1MovtRate, 'lnorm')
gamma<-fitdist(grass$Wk1MovtRate, 'gamma')
exp <-fitdist(grass$Wk1MovtRate, 'exp')
cauchy <-fitdist(grass$Wk1MovtRate, 'cauchy')
logis <-fitdist(grass$Wk1MovtRate, 'logis')
llogis <-fitdist(grass$Wk1MovtRate, 'llogis')
invgaus<-fitdist(grass$Wk1MovtRate, 'invgauss', start=list(mean=1, shape=1, dispersion=1))
burr <- fitdist(grass$Wk1MovtRate, 'burr', start=list(shape1 = 0.3, shape2 = 1, rate=1))

k.table[2,1]<-"Field"
k.table[2,2]<-norm$aic
k.table[2,3]<-weibull$aic
k.table[2,4]<-lnorm$aic
k.table[2,5]<-gamma$aic 
k.table[2,6]<-exp$aic
k.table[2,7]<-cauchy$aic
k.table[2,8]<-logis$aic
k.table[2,9]<-llogis$aic
k.table[2,10]<-invgaus$aic
k.table[2,11]<-burr$aic #Best fit by quite a bit for whole data set

descdist(forest$Wk1MovtRate, discrete=F)
norm<-fitdist(forest$Wk1MovtRate, 'norm')
weibull<-fitdist(forest$Wk1MovtRate, 'weibull')
lnorm<-fitdist(forest$Wk1MovtRate, 'lnorm')
gamma<-fitdist(forest$Wk1MovtRate, 'gamma')
exp <-fitdist(forest$Wk1MovtRate, 'exp')
cauchy <-fitdist(forest$Wk1MovtRate, 'cauchy')
logis <-fitdist(forest$Wk1MovtRate, 'logis')
llogis <-fitdist(forest$Wk1MovtRate, 'llogis')
invgaus<-fitdist(forest$Wk1MovtRate, 'invgauss', start=list(mean=1, shape=1, dispersion=1))
burr <- fitdist(forest$Wk1MovtRate, 'burr', start=list(shape1 = 0.3, shape2 = 1, rate=1))

k.table[3,1]<-"Forest"
k.table[3,2]<-norm$aic
k.table[3,3]<-weibull$aic
k.table[3,4]<-lnorm$aic
k.table[3,5]<-gamma$aic 
k.table[3,6]<-exp$aic
k.table[3,7]<-cauchy$aic
k.table[3,8]<-logis$aic
k.table[3,9]<-llogis$aic
k.table[3,10]<-invgaus$aic
k.table[3,11]<-burr$aic #Best fit by quite a bit for whole data set

################
# By Natal Density
################
low<-subset(move, Density==6)
medium<-subset(move, Density==18)
high<-subset(move, Density==36)

descdist(low$Wk1MovtRate, discrete=F)
norm<-fitdist(low$Wk1MovtRate, 'norm')
weibull<-fitdist(low$Wk1MovtRate, 'weibull')
lnorm<-fitdist(low$Wk1MovtRate, 'lnorm')
gamma<-fitdist(low$Wk1MovtRate, 'gamma')
exp <-fitdist(low$Wk1MovtRate, 'exp')
cauchy <-fitdist(low$Wk1MovtRate, 'cauchy')
logis <-fitdist(low$Wk1MovtRate, 'logis')
llogis <-fitdist(low$Wk1MovtRate, 'llogis')
invgaus<-fitdist(low$Wk1MovtRate, 'invgauss', start=list(mean=1, shape=1, dispersion=1))
burr <- fitdist(low$Wk1MovtRate, 'burr', start=list(shape1 = 0.3, shape2 = 1, rate=1))

k.table[4,1]<-"Low"
k.table[4,2]<-norm$aic
k.table[4,3]<-weibull$aic
k.table[4,4]<-lnorm$aic
k.table[4,5]<-gamma$aic 
k.table[4,6]<-exp$aic
k.table[4,7]<-cauchy$aic
k.table[4,8]<-logis$aic
k.table[4,9]<-llogis$aic
k.table[4,10]<-invgaus$aic
k.table[4,11]<-burr$aic #Best fit by quite a bit for whole data set

descdist(medium$Wk1MovtRate, discrete=F)
norm<-fitdist(medium$Wk1MovtRate, 'norm')
weibull<-fitdist(medium$Wk1MovtRate, 'weibull')
lnorm<-fitdist(medium$Wk1MovtRate, 'lnorm')
gamma<-fitdist(medium$Wk1MovtRate, 'gamma')
exp <-fitdist(medium$Wk1MovtRate, 'exp')
cauchy <-fitdist(medium$Wk1MovtRate, 'cauchy')
logis <-fitdist(medium$Wk1MovtRate, 'logis')
llogis <-fitdist(medium$Wk1MovtRate, 'llogis')
invgaus<-fitdist(medium$Wk1MovtRate, 'invgauss', start=list(mean=1, shape=1, dispersion=1))
burr <- fitdist(medium$Wk1MovtRate, 'burr', start=list(shape1 = 0.3, shape2 = 1, rate=1))

k.table[5,1]<-"Medium"
k.table[5,2]<-norm$aic
k.table[5,3]<-weibull$aic
k.table[5,4]<-lnorm$aic
k.table[5,5]<-gamma$aic 
k.table[5,6]<-exp$aic
k.table[5,7]<-cauchy$aic
k.table[5,8]<-logis$aic
k.table[5,9]<-llogis$aic
k.table[5,10]<-invgaus$aic
k.table[5,11]<-burr$aic #Best fit by quite a bit for whole data set


descdist(high$Wk1MovtRate, discrete=F)
norm<-fitdist(high$Wk1MovtRate, 'norm')
weibull<-fitdist(high$Wk1MovtRate, 'weibull')
lnorm<-fitdist(high$Wk1MovtRate, 'lnorm')
gamma<-fitdist(high$Wk1MovtRate, 'gamma')
exp <-fitdist(high$Wk1MovtRate, 'exp')
cauchy <-fitdist(high$Wk1MovtRate, 'cauchy')
logis <-fitdist(high$Wk1MovtRate, 'logis')
llogis <-fitdist(high$Wk1MovtRate, 'llogis')
invgaus<-fitdist(high$Wk1MovtRate, 'invgauss', start=list(mean=1, shape=1, dispersion=1))
burr <- fitdist(high$Wk1MovtRate, 'burr', start=list(shape1 = 0.3, shape2 = 1, rate=1))

k.table[6,1]<-"High"
k.table[6,2]<-norm$aic
k.table[6,3]<-weibull$aic
k.table[6,4]<-lnorm$aic
k.table[6,5]<-gamma$aic 
k.table[6,6]<-exp$aic
k.table[6,7]<-cauchy$aic
k.table[6,8]<-logis$aic
k.table[6,9]<-llogis$aic
k.table[6,10]<-invgaus$aic
k.table[6,11]<-burr$aic #Best fit by quite a bit for whole data set

#write.csv(k.table, 'Figures/KernalAIC.csv', row.names=F)
