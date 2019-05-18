######################
# Adding in Field Data
# 10.30.15
# BHO
######################

meas<-read.delim("Data/2013Runway_BodyMeasurements.txt")
move<-read.csv("Data/2013Runway_MovementData.csv")
net.dist<-read.delim("Data/2013Runway_NetMovementData.txt")

#Measurements

meas<-subset(meas, select=c(2:14))

names(meas)[4]<-"Arm.Upper"
names(meas)[5]<-"Arm.Lower"
names(meas)[6]<-"Leg.Upper"
names(meas)[7]<-"Leg.Lower"
names(meas)[8]<-"Head.Length"

meas$JPhotoDate <- as.numeric(format(as.Date(as.character(meas$PhotoDate),"%m/%d/%Y"),"%j"))
meas$PhotoDate <- as.Date(meas$PhotoDate, format="%m/%d/%Y")
meas$PhotoTime<-as.numeric(meas$PhotoTime)

#Deleted photos with duplicate times that could not be assigned with
# confidence

#Runway Data
move$JPhotoDate <- as.numeric(format(as.Date(as.character(move$PhotoDate),"%m/%d/%Y"),"%j"))
move$PhotoDate <- as.Date(move$PhotoDate, format="%m/%d/%Y")
move$JMetaDate <- as.numeric(format(as.Date(as.character(move$MetaDate),"%m/%d/%Y"),"%j"))
move$MetaDate <- as.Date(move$MetaDate, format="%m/%d/%Y")

move$Cohort<-as.integer(substring(move$Cohort, 3))
move$Init_Mass<-as.numeric(move$Init_Mass)
move$Density<-as.factor(move$Density)
move<-subset(move, Year==2013)
move$Wk1MovtRate<-move$Wk1Dist/move$Wk1Recaps
move$Wk1PropMoves<-move$Wk1Moves/move$Wk1Recaps

move$Unique<-paste(move$Box, move$MetaDate)
count<-count(move$Unique)
names(count)[1]<-"Unique"
move$Unique<-as.factor(move$Unique)
move<-join(move,count, by='Unique')
move<-subset(move, freq==1)

net.dist$MetaDate <- as.Date(net.dist$MetaDate, format="%m/%d/%Y")
net.dist$Unique<-paste(net.dist$Box, net.dist$MetaDate)

count<-count(net.dist$Unique)
names(count)[1]<-"Unique"
net.dist$Unique<-as.factor(net.dist$Unique)
net.dist<-join(net.dist,count, by='Unique')
net.dist<-subset(net.dist, freq==1)

net.dist<-net.dist[,c(1,4,20,22)]
colnames(net.dist)[3]<-'Wk1NetDist'

#Join
move1<-join(move, net.dist, by=c('Box', 'MetaDate'), type='inner')

run<-join(move1,meas, by=c("JPhotoDate","PhotoTime","Cohort"), type='inner')
run<-subset(run, !is.na(Wk1PropMoves) & !is.na(Wk1MovtRate))

rm(meas,move,move1)
