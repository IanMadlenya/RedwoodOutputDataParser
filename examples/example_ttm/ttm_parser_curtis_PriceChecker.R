
# Update "ttm_parser_curtis_GroupAssignmentChecker.R"
# - insert correct redwood output file
# - insert correct number of practice periods. 

source("examples/example_ttm/ttm_parser_curtis_GroupAssignmentChecker.R")
# Creates "SubjectGroups"
# - THese groups should match. 

#Creates ttm_Cleaned_Step1 used below. 





# Sean's Trusted R Parser ------------------------------------------------------

# Load only non-TTM and non-practice periods. 
ttm_dataTat   = ttm_Cleaned_Step1 %>%
  mutate(Sender = as.numeric(Sender)) %>%
  filter(Period >= 56 | Period >= 57)
#Columns are Period, Sender, Round, x, y, Price, Ex, Ey


dataTat_allTTM <- split(ttm_dataTat, ttm_dataTat$Period)

# dataTat <- dataTat_allTTM[[1]]

PriceSeqLog <- data.frame()
for (dataTat in dataTat_allTTM){
  t <- length(dataTat$Period) #Total number of choices
  s <- length(unique(dataTat$Sender))
  
  pds <- dataTat$Period[t]-dataTat$Period[1]+1 #Number of periods
  rds <- t/s #Total number of rounds
  rd <- rep(0,pds) #Will be number of rounds per period
  j <- 1
  for (i in 2:rds) {
    if (dataTat$Round[s*i]>dataTat$Round[s*(i-1)]) {
      rd[j] <- dataTat$Round[s*i]
    } else {
      j <- j+1
      rd[j] <- 1
    }
  }
  
  pr <- sort(c(.2,.28,.36,.43,.5,.57,.64,.7,.76,.83,.89,.94,1,1.06,1.13,1.21,1.31,1.43,1.57,1.75,2,2.33,2.81,3.57,5))#Price grid from choice experiment
  p <- rep(0,rds) #Vector of prices in tatonnement, each element is a round
  Z <- p #Vector of per capita excess demand, each element is a round
  for (i in 1:rds) {
    for (j in 1:s) {
      if (dataTat$Ex[(i-1)*s+j]==100) {
        Z[i] <- Z[i] - 100 + dataTat$x[(i-1)*s+j]
      } else {
        Z[i] <- Z[i] + dataTat$x[(i-1)*s+j]
      }
    }
    p[i] <- dataTat$Price[(i-1)*s+1]
  }
  Z <- Z/s
  
  q <- .1745 #Seeding for K vector (controls size of price changes)
  K <- c(q,q/2,q/4,q/8,q/16) #K vector
  ez <- 13.5 #Predicted round 1 per capita excess demand
  eps <- 3 #Convergence tolerance
  A <- rep(0,rds) #A through D are intermediate steps along the way to determining price
  B <- A
  C <- A
  D <- A
  k <- A #This will be the value used for k=(1/ez)*(Z/M) in each round
  psnap <- A #Vector of "next period prices" in tatonnement, snapped to grid where required
  pnosnap <- A #Vector of "next period prices," not snapped to grid
  
  snap <- 1 #While this variable =1, we snap price to the grid used in the choice experiment
  for (i in 1:rds) {
    if (dataTat$Round[(i-1)*s+1]==1) {
      r <- 1.57 #Initial price in round 1
      n <- 1 #n is the index of where we're at in the K vector
      snap <- 1
    } else {
      r <- psnap[i-1] #r is price asked by tatonnement in current period
      if (sign(Z[i])!=sign(Z[i-1])) { #There's a sign change in excess demand
        n <- n+1
      }
    }
    if (n <= 5) { #If we haven't moved past the end of the vector
      k[i] <- (1/ez)*K[n]
      A[i] <- k[i]*Z[i]
      B[i] <- .26175*sign(Z[i]) # _maxAngularDiff * excessDemandSign;
      C[i] <- sign(Z[i])*min(abs(A[i]),abs(B[i]))
      if (C[i]<0) {
        D[i] <- max(atan(p[i])+C[i],.01)
      } else {
        D[i] <- min(atan(p[i])+C[i],1.5608)
      }
      psnap[i] <- tan(D[i]) #This is the price that will be offered to subjects in the following round, prior to being snapped to the grid
    } else { #End of vector has been reached
      psnap[i] <- (psnap[i-1]+(0.01*sign(Z[i])))
    }
    pnosnap[i] <- psnap[i]
    if (snap==1) {
      
      # Get price from price-grid closest to current ttm price
      prindex <- which.min((pr-psnap[i])^2) #index of correct grid point
      pgrid <- pr[prindex] #This is the price snapped to the grid
      
      if (pgrid==r) { #We won't snap this price or future rounds to the grid
        snap <- 0
      } else {
        psnap[i] <- pgrid #Snap price to the grid
      }
    }
    if (dataTat$Round[(i-1)*s+1]==1) { #Resets to grid snapping in the first round of each period
      snap <- 1
    }
  }
  
  dataTat %>%
    arrange(Round) %>%
    distinct(Round, Price) %>%
    rename(SeanR.Price.Snap = Price) %>%
    select(Period:y,Ex,Ey)
  
  ttm_Cleaned_Step1 %>% filter(Period == 57) %>% distinct(Round) %>% arrange(Round)
  
  
  
  # ttm_testdata %>% filter(Period == unique(dataTat$Period)) %>%
  #   arrange(Round) %>%
  #   distinct(Round) %>%
  #   print()
  
  
}

