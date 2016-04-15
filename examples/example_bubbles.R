# Bubble example

# Load dependencies and redwood parcer helper functions
source("RedwoodDataLoader.R")

# Bubbles -----

Bub.testData <- read.csv("examples/data/Bubbles.csv")
# Bub.testData <- read.csv("C:/Users/OKComputer/Downloads/Bubbles-2016-04-14 08-25-19.468240.csv") #from a local copy
Bub.testData <- redwoodParser(data = Bub.testData,
                              keys = c("LOG_CONFIG","state")) #only want two messages

# -----
#' apply config fields to all rows ###################
Bub.testData.Config <- Bub.testData %>%
  filter(Key =="LOG_CONFIG") %>%
  select(1:5, starts_with("LOG_CONFIG.")) %>%
  mutate(
    LOG_CONFIG.groups = as.character(LOG_CONFIG.groups),  #group col is an odd one, and requires a little extra work
    LOG_CONFIG.groups = ifelse(LOG_CONFIG.groups == "NULL", NA, LOG_CONFIG.groups)
  )

# subject of data, only start variable
Bub.testData.state <- Bub.testData %>%
  filter(Key != "LOG_CONFIG") %>%
  select(1:5, starts_with("state"))

# merge back
Bub.testData <- left_join(
  Bub.testData.state,
  select(Bub.testData.Config, 
         Period, starts_with("LOG_CONFIG."))
)
# mostly done

# -----
# keeps only obs once a second #############
# state is a periodically generated variable (once every 110-120 milliseconds)
# that's too much data!
# this keeps only one obs a second
#' I know that there are about 8-10 obs a sec, this finds the ob most recently after a second-hand tick, and keeps only it. 

Bub.testData.2 <- Bub.testData %>%
  mutate(
    temp.timeDist = Time %% 1000000000 / 1000000000,
    temp.time     = as.integer(Time / 1000000000)
  ) %>%
  ungroup() %>%
  group_by(
    temp.time
  ) %>%
  mutate(
    temp.time.rank = rank(temp.timeDist),
    temp.time.keep = ifelse(temp.time.rank == min(temp.time.rank), 1,0)
  ) %>%
  ungroup() %>%
  dplyr::filter(
    temp.time.keep == 1
  ) %>%
  select(
    -temp.timeDist,-temp.time.rank, -temp.time.keep, temp.time
  )
# write.csv(Bub.testData.2, file = "examples/data/Bub.testData.csv", row.names = F)

# ------
# Plot Actions over time, within periods #################

library(ggplot2)
# add within period time
Bub.testData.2 <- Bub.testData.2 %>%
  ungroup()%>%
  group_by(Period) %>%
  mutate(
    Time.Period = Time - min(Time),
    Time.Period = Time.Period / 1000000000
  )

ggplot(Bub.testData.2,
       aes(x = Time.Period, y = state.action, colour = state.subjectid)) +
  geom_line() +
  facet_wrap(~Period)





