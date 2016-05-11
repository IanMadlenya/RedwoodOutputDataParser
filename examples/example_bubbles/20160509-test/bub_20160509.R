# loading and parsing pilot with tricky discrete logs. 

# Load dependencies and redwood parcer helper functions
source("RedwoodDataLoader.R")
library(tidyr)

Bub.testData <- read.csv("examples/example_bubbles/20160509-test/5-7-out.csv")


# Config File ------------------------------------------------------------------
Config = read.csv("examples/example_bubbles/20160509-test/Session-config-3-9.csv")
names(Config) <- paste0("config.",names(Config))


# Discrete time periods --------------------------------------------------------
DS_periods = (Config %>% filter(config.num_sub_periods > 0))$config.period
Bub.DS <- Bub.testData %>%
  filter(Period %in% DS_periods)

# Bub.testData <- read.csv("C:/Users/OKComputer/Downloads/Bubbles-2016-04-14 08-25-19.468240.csv") #from a local copy
Bub.DS <- redwoodParser(data = Bub.DS,
                        keys = c("state")
) #only want two messages


# continuous time periods ------------------------------------------------------
DS_periods <- (Config %>% filter(config.num_sub_periods > 0))$config.period
Bub.CS <- Bub.testData %>%
  filter(!(Period %in% DS_periods))

Bub.CS <- redwoodParser(data = Bub.CS,
                        keys = c("state")
) 

# Merging discrete DS and continuous time rounds CS ----------------------------

Bub.testData <- bind_rows(
  Bub.CS,Bub.DS 
) %>%
  arrange(Period, Sender, Time)


# Merging Config file
Bub.testData <- left_join(
  Bub.testData, 
  Config,
  by = c("Period" = "config.period")
) %>%
  arrange(Period, Sender, Time)



# ---------------------------------------
# Prep for images

Bub.testData <- Bub.testData %>%
  ungroup()%>%
  group_by(Period, Group) %>%
  mutate(
    Time.Period = Time - min(Time),
    Time.Period = Time.Period / 10, #time within periods, 0 is start of period
    state.subjectid = as.factor(state.subjectid)
  )


library(ggplot2)
# add within period time

# Group 2 ==============
Bub.testData.g2 <- Bub.testData %>%
  as.data.frame()  %>%
  filter(Group == 2) 

ggplot(Bub.testData.g2,
       aes(x = Time.Period, y = state.action, colour = state.subjectid)) +
  geom_point(size = 0.9) +
  facet_wrap(~Period)


# Group 1 ==============
Bub.testData.g1 <- Bub.testData.2 %>%
  filter(Group == 1) %>%

ggplot(Bub.testData.g1,
       aes(x = Time.Period, y = state.action, colour = state.subjectid)) +
  geom_point(size = 0.9) +
  facet_wrap(~Period)


# Save times ===================================================================
write.csv(Bub.testData.g1,
          file = "examples/example_bubbles/Bub_20160428_g1.csv")
write.csv(Bub.testData.g2,
          file = "examples/example_bubbles/Bub_20160428_g2.csv")






