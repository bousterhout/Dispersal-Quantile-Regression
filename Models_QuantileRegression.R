##########################
# Analysis
# Quantile Regression
# BHO
# 1.5.18
##########################

#Load data
source('DataSteps_BodyCondPCA.R')

#Load functions
source('Functions.R')

#Load libraries
packages <- c('quantreg', 'MuMIn', 'car')
package.check(packages)


move$lWk1NetDist.Rate<-log(1+move$Wk1NetDist.Rate)

#Plots
plot(move$Wk1NetDist.Rate ~ move$dist.cm)
plot(move$Wk1NetDist.Rate ~ move$SMI)
plot(move$Wk1NetDist.Rate ~ move$PC2)
plot(move$Wk1NetDist.Rate ~ move$PC2)
plot(move$Wk1NetDist.Rate ~ move$Treatment)

#############
# Reduced bootstraps to 1000, but for MS used 100000
#############

#Residents
full.1 <-rq(Wk1NetDist.Rate ~ dist.cm  * Treatment + SMI * Treatment + PC2 * Treatment + 
               dist.cm*Habitat + SMI*Habitat + PC2 * Habitat + Cohort, data=move, tau=c(.1))
summary(full.1)


red1.1 <-rq(Wk1NetDist.Rate ~ dist.cm  * Treatment  + 
                + SMI + Habitat + PC2  + Cohort, data=move, tau=.1)
summary(red1.1, se='boot', R = 1000, covariance = T)

#write.csv(x = res.sum$coefficients, file = 'Figures/NetDist_Resident_Summary_quantreg.csv')

##Median
full.5 <-rq(Wk1NetDist.Rate ~ dist.cm  * Treatment + SMI * Treatment + PC2 * Treatment + 
               dist.cm*Habitat + SMI*Habitat + PC2 * Habitat + Cohort, data=move, tau=.5)
summary(full.5, se='boot', R = 1000)

red1.5 <-rq(Wk1NetDist.Rate ~ Treatment + dist.cm*Habitat + SMI*Habitat + PC2 * Habitat + Cohort, data=move, tau=.5)
summary(red1.5, se='boot', R = 1000)

red2.5 <-rq(Wk1NetDist.Rate ~ Treatment + dist.cm + SMI + PC2 + Habitat + Cohort, data=move, tau=.5)
med.sum<-summary(red2.5, se='boot', R = 1000)

#write.csv(x = med.sum$coefficients, file = 'Figures/NetDist_Median_Summary_quantreg.csv')

##Dispersers
full.9 <-rq(Wk1NetDist.Rate ~ dist.cm  * Treatment + SMI * Treatment + PC2 * Treatment + 
               dist.cm*Habitat + SMI*Habitat + PC2 * Habitat + Cohort, data=move, tau=.9)
summary(full.9, se='boot',R = 1000)

red1.9 <-rq(Wk1NetDist.Rate ~ Treatment*dist.cm   + Treatment*SMI  + Treatment*PC2  + dist.cm*Habitat +  Cohort, data=move, tau=.9)
summary(red1.9, se='boot', R = 1000)

red2.9 <-rq(Wk1NetDist.Rate ~ Treatment*dist.cm   + SMI  + PC2  + dist.cm*Habitat +  Cohort, data=move, tau=.9)
summary(red2.9, se='boot', R = 1000)

red2.9 <-rq(Wk1NetDist.Rate ~ Habitat + Treatment + dist.cm + SMI  + PC2  + Cohort, data=move, tau=.9)
disp.sum<-summary(red2.9, se='boot', R = 1000)
#write.csv(x = disp.sum$coefficients, file = 'Figures/NetDist_Disperser_Summary_quantreg.csv')

red.r <-rq(Wk1NetDist.Rate ~ Habitat*SVL + dist.cm +  PC2  + Cohort, data=move, tau=.9)

#Slope differs between models
red2.9.qr <-rq(Wk1NetDist.Rate ~ Habitat + dist.cm  + Treatment + SMI  + PC2  + Cohort, data=move, tau=c(.1,.5, .9))
anova(red2.9.qr)
