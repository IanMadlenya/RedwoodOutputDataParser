
source("RedwoodDataLoader.R")

#  -----
#setwd('/Users/stephendoconnell/Dropbox/Matching/')
matching.testData <- read.csv("examples/example_Matching/MockRWoutputNoCost12per24subj.csv")
matching.testData <- redwoodParser(data = matching.testData,
                                   keys = c("action","action_round", "action_owntype",
                                            "action_partnertype","action_partnerid", "exit", "strategy",
                                            "repeataction_owntype","repeataction_partnertype","repeataction_partnerid"))
# write.csv(matching.testData, file = "4work/TestNoCost12per24subj.csv", row.names = F)

matching.testData <- read.csv("examples/example_Matching/MockRWoutputNoCost12per48subj.csv")
matching.testData <- redwoodParser(data = matching.testData,
                                   keys = c("action","action_round", "action_owntype",zp,
                                            "action_partnertype","action_partnerid", "exit", "strategy",
                                            "repeataction_owntype","repeataction_partnertype","repeataction_partnerid"))
# write.csv(matching.testData, file = "4work/TestNoCost12per48subj.csv", row.names = F)



matching.testData <- read.csv("examples/example_Matching/MockRWoutputNoCost12per72subj.csv")
matching.testData <- matching.testData %>%
  mutate(Value = as.character(Value))

tmp <- matching.testData %>%
  filter(Key == "action_partnerid")
# note there are 19 empty char values ("")
table(as.character(tmp$Value))
tmp <- matching.testData %>% filter(Value =="")
unique(tmp$Key)
# [1] __set_config__     __page_loaded__    start_session      _next_period       next_round        
# [6] action_partnertype action_partnerid  


# Option 2 -----------

matching.testData <- read.csv("examples/example_Matching/MockRWoutputNoCost12per72subj.csv")
matching.testData <- matching.testData %>%
  mutate(
    Value = as.character(Value),
    Value = ifelse(Key == "action_partnerid", "0", Value),
    Value = ifelse(Key == "action_partnertype", "0", Value)
  )
matching.testData <- redwoodParser(data = matching.testData,
                                   keys = c("action","action_round", "action_owntype",
                                            "action_partnertype","action_partnerid", "exit", "strategy",
                                            "repeataction_owntype","repeataction_partnertype","repeataction_partnerid"))
# write.csv(matching.testData, file = "4work/TestNoCost12per72subj.csv", row.names = F)


