require(Hmisc)
require("dplyr")

# Enter the following fields manually ------------------------------------------
# Load raw redwood data
ttm_rawrwdata <- read.csv(
  "examples/example_ttm/20160428-ttm-test-a/Revealed Preferences-2016-04-28 12-47-02.614648.csv",
  stringsAsFactors = F)
ttm_rawrwdata <- read.csv(
  "examples/example_ttm/20160501-ttm-test-1/Revealed Preferences-2016-05-02 16-38-06.942216.csv",
  stringsAsFactors = F)
pr <- 5 #Number of practice periods, must be entered manually


# Clean Raw Data ---------------------------------------------------------------

# Creates ttm_Cleaned_Step1
#' all this from `FORM Clean Data.R` file. 
{
  a <- ttm_rawrwdata
  
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
  
  if (ncol(temp) == 13){
    c$Price<-temp$V2
    c$Ex<-temp$V7
    c$Ey<-temp$V5
    c$Round<-temp$V9
    c$Market<-temp$V11
    c$inTTM<-temp$V13
    
    c<-c[,c("Period","Round","Sender","Price","Ex","Ey","Market","inTTM")]
    
    d<-merge(b,c,by=c("Period","Sender","Round"))
    
    ttm_Cleaned_Step1 <- d %>%
      mutate(Sender = (as.character(Sender)),
             Price = as.numeric(as.character(Price)),
             Ex = as.numeric(as.character(Ex)),
             Ey = as.numeric(as.character(Ey)),
             Market = as.numeric(as.character(Market)),
             inTTM2 = ifelse(inTTM == "true", T, 
                             ifelse(inTTM == 'false', F, NA))
      ) %>%
      arrange(Period, Sender)
  } else {
    c$Price<-temp$V2
    c$Ex<-temp$V7
    c$Ey<-temp$V5
    c$Round<-temp$V9
    
    c<-c[,c("Period","Round","Sender","Price","Ex","Ey")]
    
    d<-merge(b,c,by=c("Period","Sender","Round"))
    
    ttm_Cleaned_Step1 <- d %>%
      mutate(Sender = (as.numeric(as.character(Sender))),
             Price = as.numeric(as.character(Price)),
             Ex = as.numeric(as.character(Ex)),
             Ey = as.numeric(as.character(Ey))
             ) %>%
      arrange(Period, Sender)
  }


  
  
  ttm_Cleaned_Step1 <- ttm_Cleaned_Step1 %>%
    mutate(Session = "...") %>%
    select(Session, Sender, Period, x, y, Price, Ex, Ey, everything())
  
  # merge into one dataset
  

  
  rm(a,b,c,d)
  
}

# Check Sorting is Done Right ==================================================

# According to Redwood: 

# Subjects in TTM Group 1 ######################################################
group1 <- sort(
  as.numeric(
    unique(
      (ttm_rawrwdata %>% filter(
        Period %in% c(56) & Key == "rp.group1Finished"))$Sender
    )
  )
)

# Subjects in TTM Group 2
group2 <- sort(
  as.numeric(
    unique(
      (ttm_rawrwdata %>% filter(
        Period %in% c(56,57) & Key == "rp.group2Finished"))$Sender
    )
  )
)

# these don't appear to be logging correctly.....
# a number of subjects get messages from both groups. 

# add a variable to indicate subject/Sender TTM group membership. 
ttm_Cleaned_Step2 <- ttm_Cleaned_Step1 %>%
  mutate(
    rw.TTM_Group_1 = ifelse(
      (Sender %in% group1),
      TRUE,
      FALSE
    ),
    rw.TTM_Group_2 = ifelse(
      (Sender %in% group2),
      TRUE,
      FALSE)
  )

# Noise Traders VS those in the market
inTTM <- ttm_rawrwdata %>%
  filter(Key == "rp.inTTM") %>%
  select(Sender, rw.inTTM = Value) %>%
  distinct(Sender) %>%
  mutate(Sender = (Sender),
         rw.inTTM = ifelse(rw.inTTM == "True", T, F)) %>%
  arrange(Sender)

ttm_Cleaned_Step2 <- left_join(
  ttm_Cleaned_Step2,
  inTTM)


rw.ttm <- ttm_Cleaned_Step2 %>%
  filter(Period %in% c(56,57)) %>%
  distinct(Period, Sender) %>%
  arrange(Period, as.numeric(Sender))

# Sean's Manual Sorting Function --------------
#' redwood's output file indicating which subject is sorted into which market
#' should match the output from the following process. 
SeanR <- function(dataEndow = ttm_Cleaned_Step1, pr = 5){
  
  {
    require(foreach) # To use foreach loops
    
    dataEndow = dataEndow %>%
      filter(Period < 56) %>%
      mutate(
        Session = Sys.Date()
      ) %>%
      select(Session, everything())
    
    # The following is from Sean's "SortByExcessDemand5.R" 
    
    pa <- max(dataEndow$Period)-pr #Number of paid periods in choice experiment
    np <- pa/2 #Length of price grid
    s <- length(dataEndow$Session)/(pr+pa) #Number of subjects
    
    dataEndow = dataEndow[order(dataEndow$Period),] #Sort data frame by Period low to high
    dataEndow = dataEndow[(pr*s+1):length(dataEndow$Session),] #Delete practice periods from data frame.
    dataEndow = dataEndow[order(dataEndow$Sender,dataEndow$Period),] #Re-order data frame first by Sender, then Period
    
    #################################################################################################################
    
    z <- length(dataEndow$Session) #Total number of choices
    dataEndow$Subject <- rep(0,pa*s)
    for (i in 1:s) {
      dataEndow$Subject[((i-1)*pa+1):(i*pa)] <- rep(i,pa)
    }
    
    dataEndow <- dataEndow[order(-dataEndow$Ex,dataEndow$Sender,dataEndow$Price,dataEndow$Period),]
    x1 <- dataEndow$x[c(TRUE,FALSE)] #First choice of x, sorted by subject-endowment-price
    x2 <- dataEndow$x[c(FALSE,TRUE)] #Second choice of x, sorted by subject-endowment-price
    sub <- dataEndow$Subject[c(TRUE,FALSE)] #List of subject numbers for one set of choices
    price <- dataEndow$Price[c(TRUE,FALSE)] #List of prices that correspond to x1, x2
    y1 <- dataEndow$y[c(TRUE,FALSE)] #First choice of y, sorted by subject-endowment-price
    y2 <- dataEndow$y[c(FALSE,TRUE)] #Second choice of y, sorted by subject-endowment-price
    x2dif <- rep(0,s*(np-1)) #For given subject, difference during 2nd x choice from one price to the next
    y2dif <- x2dif #For given subject, difference during 2nd y choice from one price to the next
    dist2 <- x2dif #Distance between 2nd choices from one price to the next
    for (i in 1:s) {
      for (j in 1:(np-1)) {
        x2dif[(i-1)*(np-1)+j] <- x2[(i-1)*np+j+1]-x2[(i-1)*np+j]
        y2dif[(i-1)*(np-1)+j] <- y2[(i-1)*np+j+1]-y2[(i-1)*np+j]
        dist2[(i-1)*(np-1)+j] <- sqrt(((x2[(i-1)*np+j+1]-x2[(i-1)*np+j])^2)+((y2[(i-1)*np+j+1]-y2[(i-1)*np+j])^2))
      }
    }
    noise <- rep(0,s) #Add up distances for 2nd choices that changed sign for either x or y
    for (i in 1:s) {
      for (j in 2:(np-1)) {
        if ((sign(x2dif[((i-1)*(np-1)+j)])!=sign(x2dif[((i-1)*(np-1)+j-1)])) || (sign(y2dif[((i-1)*(np-1)+j)])!=sign(y2dif[((i-1)*(np-1)+j-1)]))) {
          noise[i] <- noise[i] + dist2[(i-1)*(np-1)+j]
        }
      }
    }
    
    noise_all <- noise
    sub_all <- sub
    price_all <- price
    
    ###############################################################################################
    # Drop two subjects from each side of market with highest noise score
    ###############################################################################################
    
    d <- length(noise)
    # Drop first supplier
    drop <- which.max(noise[1:(d/2)])
    if (drop==1) {
      noise <- noise[2:d]
      sub <- sub[(np+1):(d*np)]
      x2 <- x2[(np+1):(d*np)]
      y2 <- y2[(np+1):(d*np)]
      price <- price[(np+1):(d*np)]
    } else {
      noise <- c(noise[1:(drop-1)],noise[(drop+1):d])
      sub <- c(sub[1:(np*(drop-1))],sub[(np*drop+1):(d*np)])
      x2 <- c(x2[1:(np*(drop-1))],x2[(np*drop+1):(d*np)])
      y2 <- c(y2[1:(np*(drop-1))],y2[(np*drop+1):(d*np)])
      price <- c(price[1:(np*(drop-1))],price[(np*drop+1):(d*np)])
    }
    # Drop first buyer
    drop <- (d/2)-1+which.max(noise[(d/2):(d-1)])
    if (drop==(d-1)) {
      noise <- noise[1:(d-2)]
      sub <- sub[1:((d-2)*np)]
      x2 <- x2[1:((d-2)*np)]
      y2 <- y2[1:((d-2)*np)]
      price <- price[1:((d-2)*np)]
    } else {
      noise <- c(noise[1:(drop-1)],noise[(drop+1):(d-1)])
      sub <- c(sub[1:(np*(drop-1))],sub[(np*drop+1):((d-1)*np)])
      x2 <- c(x2[1:(np*(drop-1))],x2[(np*drop+1):((d-1)*np)])
      y2 <- c(y2[1:(np*(drop-1))],y2[(np*drop+1):((d-1)*np)])
      price <- c(price[1:(np*(drop-1))],price[(np*drop+1):((d-1)*np)])
    }
    # Drop second supplier
    d <- length(noise)
    drop <- which.max(noise[1:(d/2)])
    if (drop==1) {
      noise <- noise[2:d]
      sub <- sub[(np+1):(d*np)]
      x2 <- x2[(np+1):(d*np)]
      y2 <- y2[(np+1):(d*np)]
      price <- price[(np+1):(d*np)]
    } else {
      noise <- c(noise[1:(drop-1)],noise[(drop+1):d])
      sub <- c(sub[1:(np*(drop-1))],sub[(np*drop+1):(d*np)])
      x2 <- c(x2[1:(np*(drop-1))],x2[(np*drop+1):(d*np)])
      y2 <- c(y2[1:(np*(drop-1))],y2[(np*drop+1):(d*np)])
      price <- c(price[1:(np*(drop-1))],price[(np*drop+1):(d*np)])
    }
    # Drop second buyer
    drop <- (d/2)-1+which.max(noise[(d/2):(d-1)])
    if (drop==(d-1)) {
      noise <- noise[1:(d-2)]
      sub <- sub[1:((d-2)*np)]
      x2 <- x2[1:((d-2)*np)]
      y2 <- y2[1:((d-2)*np)]
      price <- price[1:((d-2)*np)]
    } else {
      noise <- c(noise[1:(drop-1)],noise[(drop+1):(d-1)])
      sub <- c(sub[1:(np*(drop-1))],sub[(np*drop+1):((d-1)*np)])
      x2 <- c(x2[1:(np*(drop-1))],x2[(np*drop+1):((d-1)*np)])
      y2 <- c(y2[1:(np*(drop-1))],y2[(np*drop+1):((d-1)*np)])
      price <- c(price[1:(np*(drop-1))],price[(np*drop+1):((d-1)*np)])
    }
    fin <- data.frame(sub,price,x2,y2)
    n <- ((s/2)-2) #Number of total buyers, number of total sellers
    finsup <-fin[1:(np*n),]
    findem <-fin[(np*n+1):(np*(s-4)),]
    p <- price[1:np]
    
    ########################################################################################
    # Of remaining s-4 subjects, split into two separate markets for period 1 tatonnement
    ########################################################################################
    
    star <- max(dataEndow$Ey)/max(dataEndow$Ex) #Price at which both corner endowments are on same budget line
    dif <- (p-star)^2
    r <- which.min(dif) #r is the index of the price at which both corner endowments are on the same budget line
    
    finsup <- finsup[order(finsup$price,finsup$sub),]
    findem <- findem[order(findem$price,findem$sub),]
    totsup <- finsup$x2[((r-2)*n+1):((r-1)*n)]+finsup$x2[((r-1)*n+1):(r*n)]+finsup$x2[(r*n+1):((r+1)*n)]
    totdem <- findem$x2[((r-2)*n+1):((r-1)*n)]+findem$x2[((r-1)*n+1):(r*n)]+findem$x2[(r*n+1):((r+1)*n)]
    supsub <- finsup$sub[1:n] #Non-dropped subjects on the supply side
    demsub <- findem$sub[1:n] #Non-dropped subjects on the demand side
    findsupdf <- data.frame(supsub,totsup)
    finddemdf <- data.frame(demsub,totdem)
    findsupdf <- findsupdf[order(findsupdf$totsup),]
    finddemdf <- finddemdf[order(-finddemdf$totdem),]
    
    mkt1sup <- findsupdf$supsub[1:(n/2)] #Put biggest suppliers near "star" price  in market 1
    mkt1dem <- finddemdf$demsub[((n/2)+1):n] #Put smallest demanders near "star" price  in market 1
    mkt2sup <- findsupdf$supsub[((n/2)+1):n] #Put smallest suppliers near "star" price  in market 2
    mkt2dem <- finddemdf$demsub[1:(n/2)] #Put biggest demanders near "star" price  in market 2
    
    dataEndow2 <- dataEndow[c(FALSE,TRUE),]
    dataEndow2 <- dataEndow2[order(dataEndow2$Subject,dataEndow2$Price),]
    sup1 <- rep(0,np)
    dem1 <- sup1
    sup2 <- sup1
    dem2 <- sup1
    for (i in 1:np) {
      for (j in 1:(n/2)) {
        sup1[i] <- sup1[i]+dataEndow2$x[((mkt1sup[j]-1)*np+i)]-100
        sup2[i] <- sup2[i]+dataEndow2$x[((mkt2sup[j]-1)*np+i)]-100
        dem1[i] <- dem1[i]+dataEndow2$x[((mkt1dem[j]-1)*np+i)]
        dem2[i] <- dem2[i]+dataEndow2$x[((mkt2dem[j]-1)*np+i)]
      }
    }
    ZZ1 <- sup1+dem1
    ZZ2 <- sup2+dem2
    mkt1df = data.frame(p,ZZ1,sup1,dem1)
    mkt2df = data.frame(p,ZZ2,sup2,dem2)
    
    ########################################################################################
    # Swap buyers between markets 1 and 2 for period 2 tatonnement.
    ########################################################################################
    
    mkt3sup <- mkt1sup
    mkt3dem <- mkt2dem
    mkt4sup <- mkt2sup
    mkt4dem <- mkt1dem
    sup3 <- rep(0,np)
    dem3 <- sup3
    sup4 <- sup3
    dem4 <- sup3
    for (i in 1:np) {
      for (j in 1:(n/2)) {
        sup3[i] <- sup3[i]+dataEndow2$x[((mkt3sup[j]-1)*np+i)]-100
        sup4[i] <- sup4[i]+dataEndow2$x[((mkt4sup[j]-1)*np+i)]-100
        dem3[i] <- dem3[i]+dataEndow2$x[((mkt3dem[j]-1)*np+i)]
        dem4[i] <- dem4[i]+dataEndow2$x[((mkt4dem[j]-1)*np+i)]
      }
    }
    ZZ3 <- sup3+dem3
    ZZ4 <- sup4+dem4
    mkt3df = data.frame(p,ZZ3,sup3,dem3)
    mkt4df = data.frame(p,ZZ4,sup4,dem4)
  }
  
  SeanR_ttm = data.frame(
    Sender = 1:max(as.numeric(as.character(dataEndow$Sender)))
  ) %>%
    mutate(
      SeanR.Mkt1 = ifelse(Sender %in% c(mkt1sup,mkt1dem), T, F),
      SeanR.Mkt2 = ifelse(Sender %in% c(mkt2sup,mkt2dem), T, F),
      SeanR.Mkt3 = ifelse(Sender %in% c(mkt3sup,mkt3dem), T, F),
      SeanR.Mkt4 = ifelse(Sender %in% c(mkt4sup,mkt4dem), T, F),
      SeanR.inTTM  = ifelse((Sender %in% c(mkt1sup,mkt1dem, mkt2sup,mkt3dem)), T, F)
    )
  
  return(SeanR_ttm)
  
}

SeanR_ttm = SeanR(ttm_Cleaned_Step1)
  
SubjectGroups <- left_join(
  (rw.ttm %>%
     filter(Period == 56) %>%
     mutate(Sender = as.numeric(Sender)) %>%
     select(Sender, rw.TTM_Group_1, rw.TTM_Group_2, rw.inTTM, Ex, Ey)),
  SeanR_ttm
)

SeanR_ttm_mkt_1 <- ttm_Cleaned_Step1 %>%
  filter(Period == 56 & 
           Sender %in% (SubjectGroups %>% filter())$Sender)



