

###############################################################
## Import and Clean
###############################################################

library(Hmisc); library("dplyr")

a <- read.table("Revealed Preferences-2016-04-07 13-05-18.962662.csv",header=TRUE,sep=",")

# rp.confirm messages
b<-a[a$Key=="rp.confirm",]
temp0<-strsplit(gsub("\\{|\\}|\\\"|:|\"|,","",x=as.character(b$Value))
," ")
temp<-do.call(rbind,temp0)

b$y<-as.numeric(temp[,2])
b$x<-as.numeric(temp[,4])
b$Round<-as.numeric(temp[,6])

b<-b[,c("Period","Sender","x","y","Round")]

# rp.round_started messages
c<-a[a$Key=="rp.round_started",]

temp0<-strsplit(gsub("\\{|\\}|\\\"|:|\"|,","",x=as.character(c$Value))
," ")
temp<-do.call(rbind,temp0)

c$Price<-temp[,2]
c$Ex<-temp[,7]
c$Ey<-temp[,5]
c$Round<-temp[,9]

c<-c[,c("Period","Round","Sender","Price","Ex","Ey")]


# merge into one dataset

d<-merge(b,c,by=c("Period","Sender","Round"))


#Save to csv
write.table(d,file="Revealed Preferences-16-04-07 parsed_data.csv",sep=",",row.names=FALSE)



	