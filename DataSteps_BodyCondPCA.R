################################################
# Conditional Expression of Dispersal Phenotype
# Tossed all body measurements into single PCA
# BHO
# 12.09.16
################################################

# Build data frame
source('DataSteps_Master.R')

#Load functions
source('Functions.R')

#Load libraries
packages <- c('agricolae', 'MASS')
package.check(packages)


#Calculate mean limb length
dfo$Arm.Upper<-ifelse(is.na(dfo$Arm.RightUpper) & is.na(dfo$Arm.LeftUpper), NA,
                      ifelse(is.na(dfo$Arm.RightUpper), dfo$Arm.LeftUpper, 
                             ifelse(is.na(dfo$Arm.LeftUpper), dfo$Arm.RightUpper,  (dfo$Arm.RightUpper+dfo$Arm.LeftUpper)/2)))
dfo$Arm.Lower<-ifelse(is.na(dfo$Arm.RightLower) & is.na(dfo$Arm.LeftLower), NA,
                      ifelse(is.na(dfo$Arm.RightLower), dfo$Arm.LeftLower, 
                             ifelse(is.na(dfo$Arm.LeftLower), dfo$Arm.RightLower,  (dfo$Arm.RightLower+dfo$Arm.LeftLower)/2)))


dfo$Leg.Upper<-ifelse(is.na(dfo$Leg.RightUpper) & is.na(dfo$Leg.LeftUpper), NA,
                      ifelse(is.na(dfo$Leg.RightUpper), dfo$Leg.LeftUpper, 
                             ifelse(is.na(dfo$Leg.LeftUpper), dfo$Leg.RightUpper,  (dfo$Leg.RightUpper+dfo$Leg.LeftUpper)/2)))
dfo$Leg.Lower<-ifelse(is.na(dfo$Leg.RightLower) & is.na(dfo$Leg.LeftLower), NA,
                      ifelse(is.na(dfo$Leg.RightLower), dfo$Leg.LeftLower, 
                             ifelse(is.na(dfo$Leg.LeftLower), dfo$Leg.RightLower,  (dfo$Leg.RightLower+dfo$Leg.LeftLower)/2)))

dfo$PicSVL<-dfo$Trunk.L + dfo$Head.L
dfo$Fore<-dfo$Arm.Upper + dfo$Arm.Lower
dfo$Hind<-dfo$Leg.Upper + dfo$Leg.Lower
dfo<-subset(dfo, !is.na(Arm.Upper))

exp$PicSVL<-exp$Trunk.L + exp$Head.L
exp$Fore<-exp$Arm.Upper + exp$Arm.Lower
exp$Hind<-exp$Leg.Upper + exp$Leg.Lower

an.s<-subset(dfo, Species=="AMAN")
ma.s<-subset(dfo, Species=="AMMA")
an.e<-subset(exp, Species=="AMAN")

#########################
# Body condition checks
# From Green 2001
#########################

plot(an.e$Mass ~ an.e$SVL) # Plot to make sure linear
an.e$Mass.s<-scale(an.e$Mass, scale=T, center =F)
dif<-lm(log(Mass.s) ~ log(SVL), data=an.e)
an.e<-subset(an.e, !is.na(SVL))
an.e$Cond<-resid(dif) 

cor.test(an.e$SVL, an.e$Cond) # Make sure not correlated (Green 2001)

#########################
# Body condition  - SMI
# From Peig & Green 2009
#########################

L0<-mean(an.e$SVL, na.rm=T)
dif<-lm(log(Mass) ~ log(SVL), data=an.e)
bSMA <- dif$coefficient[2]
an.e$SMI <- an.e$Mass * (L0/an.e$SVL)^bSMA

cor.test(an.e$SVL, an.e$SMI) # Not correlated, so that's good.

############################################
############################################
# MORPHOLOGY
############################################
############################################

# For biometrics (Lowe and McPeak 2012)
# 
# 1) Create size adjusted shape varibles to removed affects of body size
# Make a covariance matrix for log transformed SVL and each of: 
#   a) head (max length and width)
#   b) trunk (max length and width)
#   c) tail (max length and width)
#   d) legs (humerus and femur length)
# 
# 2) PC1 = generalized size becuase SVL pos cor with all measurements
# 3) PC2 and 3 = size adjusted morphological characters
# 

#################################################
#################################################
# Trunk and Head length perfectly related once get residuals becuase used
# to generate SVL (PicSVL = Head.L + Trunk.L)
#
# So I dropped Head L
#################################################
#################################################
# After talking with Siepielski, collapsed everything into 1 PCA
#################################################

an.es<-subset(an.e, !is.na(Tail.L) & !is.na(Leg.Upper))
log.an <- log(an.es[, c(38:41,43:45)])

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
morph.pca <- prcomp(log.an,
                    center = TRUE,
                    scale. = TRUE) 
#write.csv(morph.pca$rotation, file='Figures/PCA.csv')
plot(morph.pca, type="l")
summary(morph.pca)

loadings <- morph.pca$rotation
morph.pca <- predict(morph.pca, newdata = log.an) #PC1 = body size, PC2 = limb length
run.pca<-as.data.frame(morph.pca)
run.pca<-subset(run.pca, select=c(2))

an.es$Habitat<-ifelse(an.es$Runway<4, "Forest",
                      ifelse(an.es$Runway>3 & an.es$Runway<7, "Field",
                             ifelse(an.es$Runway>6 & an.es$Runway<10, "Forest","Field")))

run.covar<-an.es[,c(2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 16:19, 21:23, 25, 26, 29, 30, 
                    42, 43, 47)]
an.e.pca<-cbind(run.covar,run.pca)

an.e.pca$Habitat<-as.factor(an.e.pca$Habitat)
an.e.pca$Cohort<-as.factor(an.e.pca$Cohort)
an.e.pca$Runway<-as.factor(an.e.pca$Runway)

an.e.pca$NotMove<-an.e.pca$Wk1Recaps-an.e.pca$Wk1Moves
an.e.pca$Wk1NetDist.Rate<-an.e.pca$Wk1NetDist/an.e.pca$Wk1Recaps

move<-an.e.pca

move$Group <- as.factor(paste(move$Cohort, move$Runway, sep='_'))

rm(list= ls()[!(ls() %in% c('move'))])
