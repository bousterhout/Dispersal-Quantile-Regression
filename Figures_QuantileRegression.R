##########################
# Figures
# Conditional Dispersal
# BHO
# 1.5.18
##########################

#Load data
source('Models_QuantileRegression.R')

#Load functions
source('Functions.R')

#Load libraries
packages <- c('ggplot2', 'cowplot', 'visreg', 'quantreg', 'RCurl')
package.check(packages)

###############
# Net dist rate
###############


#############
# Dispersers
#############

#Effect of natal density for dispersers
quantile(move$Wk1NetDist.Rate, probs = .9)

# move$color.code<-ifelse(move$Wk1NetDist.Rate>17.95 & move$Treatment==6, 1,
#                    ifelse(move$Wk1NetDist.Rate>17.95 & move$Treatment==18, 2,
#                           ifelse(move$Wk1NetDist.Rate>17.95 & move$Treatment==36, 3, 4)))

move$color.code<-ifelse(move$Wk1NetDist.Rate>17.95, 1, 0)

move$color.code<-as.factor(move$color.code)

new.dat<-expand.grid(Habitat = 'Field',
                     dist.cm = mean(move$dist.cm),
                     Treatment = c(6, 18, 36),
                     SMI = mean(move$SMI),
                     PC2 = mean(move$PC2),
                     Cohort = '1',
                     color.code='0')
                     
conf_interval <- predict(red2.9, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)
new.dat<-cbind(new.dat,conf_interval)


disp.trt<-
ggplot(data=move, aes(x=Treatment, y=Wk1NetDist.Rate, shape=color.code, color=color.code)) + 
  geom_jitter(width=3)+
  geom_line(data=new.dat, aes(x=Treatment, y=lower), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=Treatment, y=higher), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=Treatment, y=fit), color='black')+
  scale_shape_manual(values = c(3,19))+
  scale_color_manual(values = c('grey50','#225ea8'))+
  scale_x_continuous(name='Natal density', breaks=c(6,18,36))+
  scale_y_continuous(name='Net distance (m/detection)')+
  guides(shape=F, color=F)


#Effect of habitat for dispersers
move$color.code<-ifelse(move$Wk1NetDist.Rate>17.95 & move$Habitat =='Field', 1, 
                        ifelse(move$Wk1NetDist.Rate>17.95 & move$Habitat=='Forest', 2, 3))

move$color.code<-as.factor(move$color.code)

new.dat<-expand.grid(Habitat = c('Field','Forest'),
                     dist.cm = mean(move$dist.cm),
                     Treatment = mean(move$Treatment),
                     SMI = mean(move$SMI),
                     PC2 = mean(move$PC2),
                     Cohort = '1',
                     color.code='5')

conf_interval <- predict(red2.9, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)
new.dat<-cbind(new.dat,conf_interval)

disp.hab<-
  ggplot() + 
  geom_jitter(data=move, aes(x=Habitat, y=Wk1NetDist.Rate, shape=color.code, colour=color.code), width=.2, size=1.7)+
  geom_point(data=new.dat, aes(x=Habitat, y=fit), color='black', size=2.5)+
  geom_errorbar(data=new.dat, aes(x=Habitat, ymin=lower, ymax=higher), color='black', width=0, lwd=1)+
  scale_shape_manual(values = c(19,19,3))+
  scale_color_manual(values = c('#31a354','#31a354', 'grey50'))+
  scale_x_discrete(name='Habitat', labels=c('Grassland', 'Forest'))+
  scale_y_continuous(name='Net distance (m/detection)', limits = c(0,50))+
  guides(shape=F, color=F)


#Effect of SMI for dispersers

move$color.code<-ifelse(move$Wk1NetDist.Rate>17.95, 1, 2) 
                        
move$color.code<-as.factor(move$color.code)

new.dat<-expand.grid(Habitat = 'Field',
                     dist.cm = mean(move$dist.cm),
                     Treatment = mean(move$Treatment),
                     SMI = seq(min(move$SMI), max(move$SMI), by=.1),
                     PC2 = mean(move$PC2),
                     Cohort = '1',
                     color.code='5')

conf_interval <- predict(red2.9, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)
new.dat<-cbind(new.dat,conf_interval)

#Nothing going on with SMI
disp.smi<-  
ggplot() + 
  geom_point(data=move, aes(x=SMI, y=Wk1NetDist.Rate, shape=color.code, colour=color.code), size=1.7)+
  geom_line(data=new.dat, aes(x=SMI, y=fit), color='black')+
  geom_line(data=new.dat, aes(x=SMI, y=lower), color='black', lty='dashed')+
  geom_line(data=new.dat, aes(x=SMI, y=higher), color='black', lty='dashed')+
  scale_shape_manual(values = c(19,3))+
  scale_color_manual(values = c('black', 'grey50'))+
  scale_x_continuous(name='Body condition index')+
  scale_y_continuous(name='Net distance (m/detection)', limits = c(0,50))+
  guides(color=F, shape=F)

#To print figures
# pdf("Figures/disp_panel.pdf", width = 3, height = 9)
# plot_grid(disp.hab, disp.trt, disp.smi, ncol=1, labels=c('a)', 'b)', 'c)'), label_x=0.23)
# dev.off()
  

#####################
# Residents
####################

quantile(move$Wk1NetDist.Rate, probs = .1)
  
move$color.code<-ifelse(move$Wk1NetDist.Rate<2.73 & move$Treatment==6, 1,
                          ifelse(move$Wk1NetDist.Rate<2.73 & move$Treatment==18, 2,
                                 ifelse(move$Wk1NetDist.Rate<2.73 & move$Treatment==36, 3, 4)))
  
move$color.code<-as.factor(move$color.code)


  
  new.dat<-expand.grid(Habitat = 'Field',
                       dist.cm = seq(min(move$dist.cm), max(move$dist.cm), by=20),
                       Treatment = c(6, 18, 36),
                       SMI = mean(move$SMI),
                       PC2 = mean(move$PC2),
                       Cohort = '1',
                       color.code='5')
  
  conf_interval <- predict(red1.1, newdata=new.dat, interval="confidence", level = 0.95, se.fit=T)
  new.dat<-cbind(new.dat,conf_interval)
  new.dat$Treatment <- as.factor(as.character(new.dat$Treatment))
  
  res.trt.expl<-
    ggplot() + 
    geom_ribbon(data=new.dat, aes(x=dist.cm, ymin=lower, ymax=higher, fill=Treatment), alpha=0.3)+
    geom_jitter(data=move, aes(x=dist.cm, y=Wk1NetDist.Rate, shape=color.code, color=color.code))+
    geom_line(data=new.dat, aes(x=dist.cm, y=fit, group=Treatment))+
    scale_shape_manual(values = c(17,19,3))+
    scale_color_manual(values = c('black','black', 'grey80'))+
    scale_fill_manual(values = c('#41b6c4','#225ea8','#a1dab4'))+
    scale_x_continuous(name='Exploration (cm)')+
    scale_y_continuous(name='Net distance (m/detection)')
  
  
  # pdf("Figures/qr_panel.tiff", width = 4, height = 12, units = 'in', res=200)
  # plot_grid(disp.hab, disp.trt, res.trt.expl, ncol=1)
  # dev.off()
  
  #########################################
  #########################################
  # Plots with visreg
  
  densitycol<-c('#225ea8','#41b6c4', '#a1dab4')
  
  densitycol1<-add.alpha(densitycol, alpha=0.4)
  
  move$color.code<-ifelse(move$Wk1NetDist.Rate<2.73 & move$Treatment==6, '#225ea8',
                          ifelse(move$Wk1NetDist.Rate<2.73 & move$Treatment==18, '#41b6c4',
                                 ifelse(move$Wk1NetDist.Rate<2.73 & move$Treatment==36, '#a1dab4', 'grey60')))
  
  move$shape.code<-ifelse(move$Wk1NetDist.Rate & move$Treatment==6, 15,
                          ifelse(move$Wk1NetDist.Rate & move$Treatment==18, 17, 19))


a<-subset(move,Wk1NetDist.Rate>2.73)
a$shape.code<-ifelse(a$Treatment==6, 0,
                     ifelse(a$Treatment == 18, 2, 1))
b<-subset(move,Wk1NetDist.Rate<2.73)

pdf("Figures/resident_legend.pdf", width = 6, height = 4)
visreg(red1.1, 'dist.cm', by='Treatment', overlay=T,
         ylab='Net distance (m/detection)',
         xlab = "Exploration (cm)",
         ylim=c(0,50),
         xaxt='n',
         fill = list(col=densitycol1),
         line = list(col=densitycol),
         partial=FALSE, rug=FALSE)
axis(1, at=seq(0,1200,300))
points(b$dist.cm, b$Wk1NetDist.Rate, pch=b$shape.code,col=b$color.code)
points(a$dist.cm, a$Wk1NetDist.Rate, pch=a$shape.code,col='grey60', cex=.7)
dev.off()