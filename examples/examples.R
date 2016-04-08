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
