##########################
# Data Steps
# Processes Activity Videos
# BHO
# 6.24.15
##########################

### VideoL = whole video length, including acclimation
### TrackL = original track length, uncorrected
### Censured animals that went missing from camera view
### Responses ending in .adj have been corrected for trials which were not the full length

#Load common functions
source('Functions.R')

#Load Libraries
packages <- c('Rcpp', 'plyr', 'reshape2')
package.check(packages)

#Save all files as *.txt **Did manually. Also works if save as ASIS

#Set folder where data files are and where to store changed files
FOLDER<- "Data/VideoData/"

#Read in files
files<-list.files(path=FOLDER,pattern = "\\.txt$",full.names=TRUE) # Make list of all files with '.csv' extension


#Compile files
df <- do.call("rbind",lapply(files, FUN=function(files)
  {read.delim(files, na.string="-")}))

names(df)<-c("Trial","Trial.duration","Arena.settings","Recording.duration", "Date",
             "TimeMissing","Dist.Moved.m", "Time.Moving.s", "Latency.Move.s", 
             "Velocity.m.s")

#Fix time and adj to real world time
df$VideoL<-sapply(strsplit(substr(df$Trial.duration,5,11),":"),
                  function(x) {
                    x <- as.numeric(x)
                    x[1]*60+x[2]
                   }
                 )
df$TrackL<-sapply(strsplit(substr(df$Recording.duration,5,11),":"),
               function(x) {
                 x <- as.numeric(x)
                 x[1]*60+x[2]
                   }
                )

df$JDate <- as.numeric(format(as.Date(as.character(df$Date),"%m/%d/%Y"),"%j"))
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
df$RealTimeL<-rep(40*60,nrow(df))
df$TrialTimeL<-rep(30*60,nrow(df))

#Replace NA's with 0 when animals didn't move
df$Dist.Moved.m<-ifelse(is.na(df$Dist.Moved.m),0, df$Dist.Moved.m )
df$Time.Moving.s<-ifelse(df$Dist.Moved.m== 0, 0, df$Time.Moving.s )
df$Latency.Move.s<-ifelse(df$Dist.Moved.m== 0, df$TrackL, df$Latency.Move.s )
df$Velocity.m.s<-ifelse(df$Dist.Moved.m== 0, 0, df$Velocity.m.s )

#############################
#Adjust to real world time
#############################

df$time.move.adj<-round((df$RealTimeL/df$VideoL)*(df$Time.Moving.s))
df$latency.adj<-round((df$RealTimeL/df$VideoL)*(df$Latency.Move.s))
df$velocity.adj<-((df$RealTimeL/df$VideoL)*(df$Velocity.m.s)) #Check this adjustment
df$dist.cm<-round(100*df$Dist.Moved.m)

#Trial
df$Trial<-ifelse(df$JDate==164 & with(df,grepl('T1', Arena.settings))=="TRUE" ,1,
                    ifelse(df$JDate!=164 & with(df,grepl('T2', Arena.settings))=="FALSE", 1,2))
#Camera

df$Cam.label<-grepl('Cam', df$Arena.settings) #If Cam, true, if C false
df$Cam.label2<-grepl('cam', df$Arena.settings) #If Cam, true, if C false
df$Cam.label3<-grepl('CAM', df$Arena.settings) #If Cam, true, if C false
df$Cam.label4<-grepl('CAm', df$Arena.settings) #If Cam, true, if C false

df$Cam.loc<-gregexpr(pattern ='[C,c]',df$Arena.settings) #Get "C" in string
df$CamNum.loc<-ifelse(df$Cam.label=="TRUE"|df$Cam.label2=="TRUE"|df$Cam.label3=="TRUE"|
                      df$Cam.label4=="TRUE", as.numeric(df$Cam.loc)+3, #Start place of cam number
                      as.numeric(df$Cam.loc)+1)

df$Camera<-substring(df$Arena.settings, df$CamNum.loc, df$CamNum.loc)

df <- subset(df, select = -c(Cam.label,Cam.label2,Cam.label3,Cam.label4,
                             Cam.loc, CamNum.loc) )

#Arena
df$Arena<-ifelse(with(df,grepl('BL', Arena.settings))=="TRUE" ,"BL",
                  ifelse(with(df,grepl('BR', Arena.settings))=="TRUE" ,"BR",
                         ifelse(with(df,grepl('TR', Arena.settings))=="TRUE" ,"TR","TL")))
                                
  
####################################################################


#Merge individual info with trial info
match <- read.delim("Data/2013_ID_Camera_Location.txt")
match <- match[,c(1,3:6,8,9)]
match$Trial<-as.factor(match$Trial)
match$Arena<-as.character(match$Arena)
match$Date<-as.Date(match$Date, format="%m/%d/%Y")
match$Camera<-as.factor(match$Camera)
match$Arena<-as.factor(match$Arena)
match$Meta_Julian<-as.numeric(format(as.Date(as.character(match$Meta_Date),"%m/%d/%Y"),"%j"))
match$Meta_Date <- as.Date(match$Meta_Date, format="%m/%d/%Y")


df$Camera<-as.factor(df$Camera)

df2<-join(df, match, by=c("Date","Trial","Camera","Arena"), type = "left")
df2<-subset(df2, !is.na(Tank))

## Checking for errors in merge
cols<-c("Box","Meta_Date")
df2$UID <- apply( df2[ , cols ] , 1 , paste , collapse = "-" )
 a<-subset(df2, is.na(Tank))
# b <- df2[df2$UID %in% a$UID, ]
# c<-b[order(a$Date),]

cols<-c("Box","Meta_Date", "Date")
df2$UID2 <- apply( df2[ , cols ] , 1 , paste , collapse = "-" )
df2$dup<-duplicated(df2$UID2)

a<-subset(df2, dup=="TRUE"); a$Videodup<-rep("Yes", nrow(a))
a<-subset(a, select=c("UID2","Videodup"))

df2<-join(df2, a, type="left", by="UID2")
df2<-subset(df2, is.na(Videodup))

#b<-a[order(a$UID2), ]
#videodups <- df2[df2$UID2 %in% a$UID2, ]

###############
# Boxes got mislabeled/duplicates, but none were for repeatability.
# Censured all duplicate box ids
###############
AA.tank.info<-read.delim("Data/AMAN_TankTreatment2013.txt")
AM.tank.info<-read.delim("Data/AMMA_TankTreatment2013.txt")
AM.tank.info$Tank<-as.factor(AM.tank.info$Tank)
trt<-tank<-rbind(AA.tank.info, AM.tank.info)
df2.1<-join(df2, trt, by="Tank", type="left" )

AMAN<- read.delim("Data/AMAN_Metamorphs.txt")
AMAN<-AMAN[,c(3:6,8:9)]
AMMA<-read.delim("Data/AMMA_Metamorphs.txt")
colnames(AMMA)[6]<-"Meta_Date"; colnames(AMMA)[10]<-"Treatment";
AMMA<-AMMA[,c(2:5,7:8)]
names(AMMA)<-c("Species","Box","Tank", "Meta_Date", "Mass","SVL")
tank<-rbind(AMMA,AMAN)

tank$Tank<-as.factor(tank$Tank)
tank$Meta_Julian<-as.numeric(format(as.Date(as.character(tank$Meta_Date),"%m/%d/%Y"),"%j"))
tank$Meta_Date <- as.Date(tank$Meta_Date, format="%m/%d/%Y")


cols<-c("Box","Meta_Date")
tank$UID <- apply( tank[ , cols ] , 1 , paste , collapse = "-" )
tank$dup<-duplicated(tank$UID)

# a<-subset(tank, dup=="TRUE")
# b<-aa[order(a$UID), ]
# tankdups <- tank[tank$UID %in% a$UID, ] #Big differences between animals in 22, 39 and 42
 
#  aa<-subset(df3, dup=="TRUE")
#  bb<-aa[order(aa$UID), ]
#  count(bb$UID)

tank<-subset(tank, dup=="FALSE")


df3<-join(df2.1, tank, by=c("Meta_Date","Tank","Box"),type="left")

# a<-subset(df3, is.na(SVL))
# bb<-a[order(a$Meta_Date), ]
# write.csv(bb,"DataSteps_Checks/TankCheck.csv", row.names=F)

df3$Mass.s<-scale(df3$Mass, scale=T, center =F)
dif<-lm(log(Mass.s) ~ log(SVL), data=df3)

df3.na<-subset(df3, !is.na(SVL))
df3.na$Cond<-resid(dif)

df3.na<-subset(df3.na, select=c(22,23,24,38))

df3<-join(df3,df3.na, by=c('Meta_Date','Tank','Box'),type='left', match='first')

#Remove animals that went missing
df3<-subset(df3, TimeMissing==0) 
df3.1<-df3[,c("Date","Dist.Moved.m","Time.Moving.s", "Latency.Move.s", "Velocity.m.s",
           "JDate", "time.move.adj", "latency.adj", "velocity.adj", "dist.cm",
           "Meta_Date", "Meta_Julian", "Box", "Tank", "Species", "Mass", "SVL", "Treatment",
           "Cond")]

colnames(df3.1)[13]<-("ID")
#write.csv(df,"DataSteps_Checks/FinalCheck.csv", row.names=F)


##########################
# AIR PRESSURE
##########################

#Source: Wunderground, downloaded 7.13.15

weather<- read.csv("Data/Weather2013.csv")
weather$Julian<-as.numeric(format(as.Date(as.character(weather$CDT),"%Y-%m-%d"),"%j"))
weather$Date <- as.Date(weather$CDT, format="%Y-%m-%d")

#weather$diff<-weather[,11]-weather[,13] #Pressure doesn't appear to change much over a day. Use mean

atm<-subset(weather, select=c(12,25))
names(atm)<-c("ATM", "Date")
df4<-join(df3.1, atm, type="left", by = "Date")

##########################
# BODY MEASUREMENTS
##########################

source('DataSteps_BodyMeasurements.R')

df5<-join(df4, measure.agg, type="left", by=c("ID","Meta_Date"))

cols<-c("ID","Meta_Date", "Date")
df5$UID2 <- apply( df5[ , cols ] , 1 , paste , collapse = "-" )
df5$dup<-duplicated(df5$UID2)

df<-df5

rm(AA.tank.info, AM.tank.info, AMAN, AMMA, atm, df2, df2.1, df3, df3.1, df4, df5, id, match, measure, measure.agg, measure2,
   tank, trt, a, weather)

#######################################
#  REMOVE ACTIVITY RECORDS AFTER FIRST
#######################################

dfo<-df[order(df$Date),]
cols<-c("ID","Meta_Date")
dfo$UID <- apply( dfo[ , cols ] , 1 , paste , collapse = "-" )
dfo$dup<-duplicated(dfo$UID)

dfo<-subset(dfo, dup=="FALSE")

an<-subset(dfo, Species=="AMAN")
ma<-subset(dfo, Species=="AMMA")

#############
# FIELD DATA
#############

source('DataSteps_Runways.R')
names(run)[1]<-"ID"
names(run)[3]<-"Meta_Date"
names(run)[15]<-"Meta_Julian"
names(run)[27:32]<-c("Head.L","Head.W","Trunk.L","Trunk.W","Tail.L","Tail.W")

df.r<-subset(an, select=c(1,10,11,12,13,14,15,16,17,18,19))

exp<-join(df.r, run, type="inner", by=c("ID","Meta_Julian"))

#exp$U<-paste(exp$ID, exp$Meta_Date)
#count<-count(exp$U)


exp$Habitat<-ifelse(exp$Runway<4,"Forest",
                    ifelse(exp$Runway>3 & exp$Runway<7, 'Field',
                           ifelse(exp$Runway>6 & exp$Runway<9, 'Forest','Field')))

##################
# Clean up
##################

rm(list=ls()[! ls() %in% c("df","dfo", "exp")])
