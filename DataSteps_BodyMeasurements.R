##########################
# Data Steps
# Biometric Measurements from 2013 Activity Assays
# BHO
# 7.14.15
##########################

## Adjusted time in Biometric Id so doesn't match with field notebook to account
## for cameras not having DST

#Set folder where data files are and where to store changed files
FOLDER<- "Data/BodyMeasurements"

#Read in files
files<-list.files(path=FOLDER,pattern = "\\.csv$",full.names=TRUE) # Make list of all files with '.csv' extension


#Compile files
measure <- do.call("rbind",lapply(files, FUN=function(files)
{read.csv(files, na.string="NA", stringsAsFactors=FALSE)}))

names(measure)<-c("ID","Date","Pic.Date","Pic.Time", "Arm.LeftUpper","Arm.LeftLower",
             "Arm.RightUpper","Arm.RightLower", "Leg.LeftUpper", "Leg.LeftLower",
             "Leg.RightUpper", "Leg.RightLower","Head.L","Head.W","Trunk.L",
             "Trunk.W","Tail.L","Tail.W","Pic.Num")

options(digits=15)

measure$Head.L<-as.numeric(measure$Head.L)
measure$Date <- as.Date(measure$Date, format="%m/%d/%Y")
measure$Pic.Date <- as.Date(measure$Pic.Date, format="%m/%d/%Y")
measure$Julian<-as.numeric(format(as.Date(as.character(measure$Date),"%Y-%m-%d"),"%j"))

#Add in ID and Meta Date to match to other records
id<-read.csv("Data/Biometrics_IDs.csv")
id<-subset(id, select=c(1:8))

id$Date <- as.Date(id$Date, format="%m/%d/%Y")
id$Pic.Date <- as.Date(id$Pic.Date, format="%m/%d/%Y")
id$Julian<-as.numeric(format(as.Date(as.character(id$Date),"%Y-%m-%d"),"%j"))

measure$mcheck<-rep("m",nrow(measure))
id$icheck<-rep("i",nrow(id))


measure2<-join(measure,id, type="left", by =c("Pic.Date","Pic.Time","Pic.Num"))

#fit<-subset(measure2, is.na(icheck))

###If fit is greater than 1, make sure all picture times have a preceding 0 (09:13 NOT 9:13)

##To look at accuracy, compare measurements for 9/30/13 10:02-10:03 (DSCN4016.JPG)
#tag.check<-subset(measure2, Pic.Date=="2013-09-27"|Pic.Date=="2013-09-30")

#write.table(fit,"fit.csv", row.names=F, sep=",")

#measure3<-subset(measure2, !is.na(icheck))
#a<-count(measure3, vars=c("ID","Date"))

#Get average measures for each salamander

measure<-subset(measure2, select=c(3:19,22:27))

measure.agg<-aggregate(cbind(Arm.LeftUpper, Arm.LeftLower,
             Arm.RightUpper, Arm.RightLower, Leg.LeftUpper, Leg.LeftLower,
             Leg.RightUpper, Leg.RightLower, Head.L, Head.W, Trunk.L,
             Trunk.W, Tail.L, Tail.W)~ID+Julian + Date + Species,FUN=mean,data=measure)

colnames(measure.agg)[2]<-"Meta_Julian"; colnames(measure.agg)[3]<-"Meta_Date" 

# Make sure it worked
# cols<-c("ID","Date")
# measure.agg$UID <- apply( measure.agg[ , cols ] , 1 , paste , collapse = "-" )
# measure.agg$dup<-duplicated(measure.agg$UID)
