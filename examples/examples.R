# Examples

source("RedwoodDataLoader.R")

# Continuous Markets -----

CMtestData <- read.csv("examples/ContinuousMarketsExperimentData.csv")
CMtestData <- redwoodParser(data = CMtestData,
               keys = c("trade", "cancel","offer_text"))



# BJ Pricing -----

BJP.testData <- read.csv("examples/BJPricingExperimentData.csv")
BJP.testData <- redwoodParser(data = BJP.testData,
                          keys = c("state", "actions", "targets"))
