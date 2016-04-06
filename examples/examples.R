# Examples -----

source("RedwoodDataLoader.R")

## Continuous Markets -----

CM.testData <- read.csv("examples/ContinuousMarketsExperimentData.csv")
CM.testData <- redwoodParser(data = CMtestData,
               keys = c("trade", "cancel","offer_text"))



## BJ Pricing -----

BJP.testData <- read.csv("examples/BJPricingExperimentData.csv")
BJP.testData <- redwoodParser(data = BJP.testData,
                          keys = c("state", "actions", "targets"))
