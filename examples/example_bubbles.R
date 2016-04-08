# Bubble example


# Bubbles -----

Bub.testData <- read.csv("examples/data/Bubbles.csv")
Bub.testData <- redwoodParser(data = Bub.testData,
                              keys = c("LOG_CONFIG","state"))


#' apply config fields to all rows 
Bub.testData.2 <- Bub.testData %>%
  select(starts_with("LOG_CONFIG.")) %>%
  mutate(
    LOG_CONFIG.groups = as.character(LOG_CONFIG.groups),
    LOG_CONFIG.groups = ifelse(LOG_CONFIG.groups == "NULL", NA, LOG_CONFIG.groups)
  )
na.locf <- function(x){
  x = unlist(x)
  #x is a vector, replace any NA with the value that preceeds it. 
  for (i in 2:length(x)){
    if(is.na(x[i])){x[i] = x[i-1]}
  }
  as.vector(x)
}

for (Col in names(Bub.testData.2)){
  Bub.testData.2[,Col] <- na.locf(Bub.testData.2[,Col])
}

Bub.testData <- Bub.testData %>%
  select(-starts_with("LOG_CONFIG.")) %>%
  bind_cols(Bub.testData.2)

# -----
#' #' state is a periodically generated variable (once every 110-120 milliseconds)
#' #' this keeps only one obs a second
#' #

Bub.testData.2 <- Bub.testData %>%
  
  dplyr::filter(
    Key == "state"
  ) %>%
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
    -temp.timeDist:-temp.time.keep
  )
# write.csv(Bub.testData.2, file = "examples/data/Bub.testData.csv", row.names = F)
