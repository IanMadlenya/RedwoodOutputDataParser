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


# Stephen O'Connell ------------

matching.testData <- read.csv("examples/data/Matching-Steve Active Dev -strategy method.csv")
matching.testData <- redwoodParser(data = matching.testData,
                                   keys = c("action","action_round", "action_owntype",
                                            "action_partnertype","action_partnerid", 
                                            "exit", "repeataction_owntype",
                                            "repeataction_partnertype","repeataction_partnerid",
                                            "strategy"))

