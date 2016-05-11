# loading and parsing pilot with tricky discrete logs. 

# Load dependencies and redwood parcer helper functions
source("RedwoodDataLoader.R")
library(tidyr)

Bub.testData <- read.csv("examples/example_bubbles/20160510-test/Bubbles-2016-05-10 13-59-31.794369.csv")


# Config File ------------------------------------------------------------------
Config = read.csv("examples/example_bubbles/20160510-test/Session-config-20160509.csv")
names(Config) <- paste0("config.",names(Config))


# Discrete time periods --------------------------------------------------------
DS_periods = (Config %>% filter(config.num_sub_periods > 0))$config.period
Bub.DS <- Bub.testData %>%
  filter(Period %in% DS_periods)

# Bub.testData <- read.csv("C:/Users/OKComputer/Downloads/Bubbles-2016-04-14 08-25-19.468240.csv") #from a local copy
Bub.DS <- redwoodParser(data = Bub.DS,
                        keys = c("state")
)


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

ggplot(Bub.testData.g2,
       aes(x = Time.Period, y = state.payoff, colour = state.subjectid)) +
  geom_point(size = 0.9) +
  facet_wrap(~Period)



# Group 1 ==============
Bub.testData.g1 <- Bub.testData %>%
  filter(Group == 1)

ggplot(Bub.testData.g1,
       aes(x = Time.Period, y = state.action, colour = state.subjectid)) +
  geom_point(size = 0.9) +
  facet_wrap(~Period)

ggplot(Bub.testData.g1,
       aes(x = Time.Period, y = state.payoff, colour = state.subjectid)) +
  geom_point(size = 0.9) +
  facet_wrap(~Period)


# Save times ===================================================================
write.csv(Bub.testData.g1,
          file = "examples/example_bubbles/20160510-test/bjp_testdata_g1.csv")
write.csv(Bub.testData.g2,
          file = "examples/example_bubbles/20160510-test/bjp_testdata_g2.csv")

# Confirm payoff calcs =========================================================

#' Payoff function
#' see doc ""eq 4, 5, and 7

s_pi <- function(r, p1, n = 4, q1 = 2/3, q2 = 1/3, mu = 100){
  mu * p1 * ((q1) 
             + (q2) * ((r-1)) / (n-1)
             + 3 * (1 - q2 - q1) * max(0, (((r - 1) / (n - 1)) * ((r - 2) / (n - 2))))
  )
}

Bub.testData <- Bub.testData %>%
  mutate(
    config.q1 = 2/3,
    config.q2 = 1/3
  ) %>%
  mutate(
    s_pi = s_pi(state.rank, state.action, n = 4, q1 = config.q1, q2 = config.q2, mu = config.mu)
  ) %>%
  select(
    Period, Group, datetime, state.subjectid, state.rank, state.action, state.payoff, s_pi,
    config.q1,config.q2,config.mu, config.groups, everything()
  )



