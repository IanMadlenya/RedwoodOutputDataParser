# loading and parsing pilot with tricky discrete logs. 

# Load dependencies and redwood parcer helper functions
source("RedwoodDataLoader.R")
library(tidyr)

Bub.testData <- read.csv("examples/example_bubbles/Bubbles-2016-04-26 pt1.csv")
Pt1Period = max(Bub.testData$Period)-1

Bub.testData.pt2 <-   read.csv("examples/example_bubbles/Bubbles-2016-04-26 pt2.csv") %>%
  mutate(
    Period = Period + Pt1Period #get period counts correct, max period in Pt1 is Min+1 in Pt2
  )

Bub.testData <- bind_rows(
  Bub.testData,
  Bub.testData.pt2
)


# Config File ------------------------------------------------------------------
Config = read.csv("examples/example_bubbles/2016-04-26 parameters input-ptA.csv")
Config = bind_rows(
  Config, 
  read.csv("examples/example_bubbles/2016-04-26 parameters input-ptB.csv")
)
names(Config) <- paste0("config.",names(Config))

# Bub.testData <- left_join(
#   Bub.testData, 
#   Config
# )

# Discrete time periods --------------------------------------------------------
DS_periods = (Config %>% filter(config.num_sub_periods > 0))$config.period
Bub.DS <- Bub.testData %>%
  filter(Period %in% DS_periods)

# Bub.testData <- read.csv("C:/Users/OKComputer/Downloads/Bubbles-2016-04-14 08-25-19.468240.csv") #from a local copy
Bub.DS <- redwoodParser(data = Bub.DS,
                        keys = c("__set_points__","updateAction", "endofsubperiod")
) #only want two messages

Bub.DS2 <- Bub.DS %>%
  arrange(Period, Sender, Time) %>%
  as.data.frame()

g = paste(Bub.DS2$Period,Bub.DS2$Sender)
Bub.DS2_tmp <- split(Bub.DS2, g)
for (i_df in 1:length(Bub.DS2_tmp)){
  
  Bub.DS2_subdf = Bub.DS2_tmp[[i_df]]
  
  #for first subperiod action, carry backward first log
  for (i in 2:(nrow(Bub.DS2_subdf))){
    if (is.na(Bub.DS2_subdf$updateAction.action[i])){
      Bub.DS2_subdf$updateAction.action[i] = Bub.DS2_subdf$updateAction.action[i-1]
    }
  }
  
  #carry forward to NA previous actions
  for (i in ((nrow(Bub.DS2_subdf)-1):1)){
    if (is.na(Bub.DS2_subdf$updateAction.action[i])){
      Bub.DS2_subdf$updateAction.action[i] = Bub.DS2_subdf$updateAction.action[i+1]
    }
  }
  
  Bub.DS2_tmp[[i_df]] = Bub.DS2_subdf
  
}

Bub.DS2 <- unsplit(Bub.DS2_tmp, g)



Bub.DS2 <- Bub.DS2 %>%
  arrange(Period, Sender, Time) %>%
  group_by(Period, Sender) %>%
  filter(grepl("endofsubperiod",Key)) %>%
  distinct(Period, Sender, datetime)

Bub.DS2 <- Bub.DS2 %>%
  mutate(
    Subperiod = rank(Time)
  )

Bub.DS <- Bub.DS2 %>%
  select(-starts_with("endofsubperiod"), -starts_with("__set_points__.period"))

names(Bub.DS)[which(names(Bub.DS)== "__set_points__.points")] = "state.payoff"

Bub.DS <- Bub.DS %>%
  mutate(
    state.action = updateAction.action,
    state.subjectid = Sender
    
  )


# continuous time periods ------------------------------------------------------
DS_periods <- (Config %>% filter(config.num_sub_periods > 0))$config.period
Bub.CS <- Bub.testData %>%
  filter(!(Period %in% DS_periods))

Bub.CS <- redwoodParser(data = Bub.CS,
                        keys = c("state", "updateAction", "endofsubperiod")
) 

# Merging discrete DS and continuous time rounds CS ----------------------------

Bub.testData <- bind_rows(
  Bub.CS,Bub.DS 
) %>%
  arrange(Period, Sender, Time)

# ---------------------------------------



Bub.testData.2 <- Bub.testData %>%
  ungroup()%>%
  group_by(Period) %>%
  mutate(
    Time.Period = Time - min(Time),
    Time.Period = Time.Period / 10,
    state.subjectid = as.factor(state.subjectid)
  )

# Merge over config fields
Bub.testData.2 <- left_join(
  Bub.testData.2, 
  Config, 
  by = c("Period" = "config.period")
)


library(ggplot2)
# add within period time



# Group 1 ==============
Bub.testData.g1 <- Bub.testData.2 %>%
  filter(Group == 1) %>%

ggplot(Bub.testData.g1,
       aes(x = Time.Period, y = state.action, colour = state.subjectid)) +
  geom_point(size = 0.9) +
  facet_wrap(~Period)



# Group 2 ==============
Bub.testData.g2 <- Bub.testData.2 %>%
  as.data.frame()  %>%
  filter(Group == 2) 

ggplot(Bub.testData.g2,
       aes(x = Time.Period, y = state.action, colour = state.subjectid)) +
  geom_point(size = 0.9) +
  facet_wrap(~Period)

# Save times ===================================================================
write.csv(Bub.testData.g1,
          file = "examples/example_bubbles/Bub_20160428_g1.csv")
write.csv(Bub.testData.g2,
          file = "examples/example_bubbles/Bub_20160428_g2.csv")


# payoff check: =================================================================
s_pi <- function(r, p1, n = 6, q1 = 2/3, q2 = 1/3, mu = 100){

  if ((q1 > 0.66 && q1 < 0.67) && (q2 > 0.33 && q2 < 0.34)){
    100 * p1 * (
      (2/3) 
      + ((2/3) * ((r-1)) / (n-1))
      )
  } else if ((q1 == 0.8 && q2 == 0.1)){
    100 * p1 * ((0.8)  
              + 0.2 * ((r-1) / (n - 1))
              + (0.3 * max(0, (((r - 1) / (n - 1)) * ((r - 2) / (n - 2)))))
    )
  }
}

#   s_pi(1.0, 1.75, 6, 0.6666, 0.33333, 100)
#   s_pi(2.0, 0.75, 6, 0.6666, 0.33333, 100)

Bub.testData.g2 <- Bub.testData.g2 %>%
  ungroup() %>%
  filter(grepl("state", Key)) 

Bub.testData.g2 <- Bub.testData.g2 %>%
  mutate(
    s_pi = s_pi(r = state.rank, p1 = state.action, n = 6, q1 = config.q1, q2 = config.q2, mu = config.mu)
  ) %>%
  select(
    Period, Group, datetime, state.subjectid, state.rank, state.action, state.payoff, s_pi,
    config.q1,config.q2,config.mu, config.groups, everything()
  )



