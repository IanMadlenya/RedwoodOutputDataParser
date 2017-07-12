

###############################################################
## Import and Clean
###############################################################

library(Hmisc); library("dplyr")

a <- read.table("examples/example_ttm/20160428-ttm-test-a/Revealed Preferences-2016-04-28 01-31-07.337304.csv",header=TRUE,sep=",")
a <- read.table("examples/example_ttm/20160501-ttm-test-1/Revealed Preferences-2016-05-02 16-38-06.942216.csv",header=TRUE,sep=",")


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
temp<-do.call(bind_rows,lapply(temp0, function(x){
  as.data.frame(t(data.frame(x)),
                stringsAsFactors = F)
}))

c$Price<-temp$V2
c$Ex<-temp$V7
c$Ey<-temp$V5
c$Round<-temp$V9
c$Market<-temp$V11
c$inTTM<-temp$V13


c<-c[,c("Period","Round","Sender","Price","Ex","Ey","Market","inTTM")]


# merge into one dataset

d<-merge(b,c,by=c("Period","Sender","Round"))


#Save to csv
write.table(d,file="TTM-2016-04-27 parsed_data.csv",sep=",",row.names=FALSE)



	