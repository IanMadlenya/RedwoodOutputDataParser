# Examples -----

source("RedwoodDataLoader.R")

## Continuous Markets -----

CMtestData <- read.csv("examples/data/ContinuousMarketsExperimentData.csv")
CMtestData <- rwp(data = CMtestData,
               keys = c("trade", "cancel","offer_text"))


# BJ Pricing -----

BJP.testData <- read.csv("examples/data/BJPricingExperimentData.csv")
BJP.testData <- rwp(data = BJP.testData,
                          keys = c("state", "actions", "targets"))


# Stephen O'Connell ------------

matching.testData <- read.csv("examples/data/Matching-Steve Active Dev -strategy method.csv")
matching.testData <- rwp(data = matching.testData,
                                   keys = c("action","action_round", "action_owntype",
                                            "action_partnertype","action_partnerid", 
                                            "exit", "repeataction_owntype",
                                            "repeataction_partnertype","repeataction_partnerid",
                                            "strategy"))

# use rwp_rowmerger helper function. 
temp <- rwp_rowmerger(matching.testData,
                      keys =  c("action","action_round", "action_owntype",
                                "action_partnertype","action_partnerid"))


write.csv(matching.testData, 
          file = "examples/data/matching_testData_output.csv")
