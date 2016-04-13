# Examples -----

source("RedwoodDataLoader.R")

## Continuous Markets -----

CMtestData <- read.csv("examples/data/ContinuousMarketsExperimentData.csv")
CMtestData <- redwoodParser(data = CMtestData,
               keys = c("trade", "cancel","offer_text"))


# BJ Pricing -----

BJP.testData <- read.csv("examples/data/BJPricingExperimentData.csv")
BJP.testData <- redwoodParser(data = BJP.testData,
                          keys = c("state", "actions", "targets"))


#  -----

matching.testData <- read.csv("examples/data/Matching_CUNYbaruch.CSV.csv")
matching.testData <- redwoodParser(data = matching.testData,
                                   keys = c("action", "action_owntype",
                                            "action_partnertype", "exit",
                                            "repeataction_owntype","repeataction_partnertype"))
# write.csv(matching.testData, file = "examples/data/matching_testData.csv", row.names = F)