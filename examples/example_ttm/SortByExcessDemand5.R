# setwd("/Users/seancrockett/Dropbox/Edgy/Edgeworks 2013/ChoiceAndTatonnement")
# Running this command in R console will change working directory to appropriate directory

# source("SortByExcessDemand5.r")
# Running this command in R console will run the script file "analyze.r" saved in working directory

# rm(list=ls())
# Running this command in R console will clear the workspace, but maintain the current working directory

# Add necessary packages
library(foreach) # To use foreach loops

dataEndow = ttm_Cleaned_Step1 %>% filter(Period < 56)
# dataEndow = read.csv("160415RoboChoice.csv")
#Columns are Session (date), Sender, Period, x, y, price, Ex, Ey

#################################################################################################################

pr <- 5 #Number of practice periods, must be entered manually

#################################################################################################################

pa <- max(dataEndow$Period)-pr #Number of paid periods in choice experiment
np <- pa/2 #Length of price grid
s <- length(dataEndow$Session)/(pr+pa) #Number of subjects

# dataEndow = dataEndow[order(dataEndow$Period),] #Sort data frame by Period low to high
# dataEndow = dataEndow[(pr*s+1):length(dataEndow$Session),] #Delete practice periods from data frame.
# dataEndow = dataEndow[order(dataEndow$Sender,dataEndow$Period),] #Re-order data frame first by Sender, then Period
dataEndow <- dataEndow %>%
  arrange(Sender, Period) %>%
  filter(Period >= 6 & Period <= 56)
  


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



