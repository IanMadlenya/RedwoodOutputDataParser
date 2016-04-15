# Bubble example


source("RedwoodDataLoader.R")

# Bubbles -----

Bub.testData <- read.csv("examples/data/Bubbles.csv")
# Bub.testData <- read.csv("C:/Users/OKComputer/Downloads/Bubbles-2016-04-14 08-25-19.468240.csv")
Bub.testData <- redwoodParser(data = Bub.testData,
                              keys = c("LOG_CONFIG","state"))


#' apply config fields to all rows


Bub.testData.Config <- Bub.testData %>%
  filter(Key =="LOG_CONFIG") %>%
  select(1:5, starts_with("LOG_CONFIG.")) %>%
  mutate(
    LOG_CONFIG.groups = as.character(LOG_CONFIG.groups),
    LOG_CONFIG.groups = ifelse(LOG_CONFIG.groups == "NULL", NA, LOG_CONFIG.groups)
  )

Bub.testData.state <- Bub.testData %>%
  filter(Key != "LOG_CONFIG") %>%
  select(1:5, starts_with("state"))


Bub.testData <- left_join(
  Bub.testData.state,
  select(Bub.testData.Config, 
         Period, starts_with("LOG_CONFIG."))
)


# -----
#' #' state is a periodically generated variable (once every 110-120 milliseconds)
#' #' this keeps only one obs a second
#' #

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
    -temp.timeDist,-temp.time.rank, -temp.time.keep
  )
# write.csv(Bub.testData.2, file = "examples/data/Bub.testData.csv", row.names = F)
