# Examples -----

source("RedwoodDataLoader.R")

## Continuous Markets -----

<<<<<<< HEAD
CMtestData <- read.csv("examples/data/ContinuousMarketsExperimentData.csv")
CMtestData <- redwoodParser(data = CMtestData,
               keys = c("trade", "cancel","offer_text"))

# BJ Pricing -----
=======
CM.testData <- read.csv("examples/ContinuousMarketsExperimentData.csv")
CM.testData <- redwoodParser(data = CM.testData,
               keys = c("trade", "cancel","offer_text"))



## BJ Pricing -----
>>>>>>> 4c315ccb4a2414855fc4ad62698bfedc2f9176f8

BJP.testData <- read.csv("examples/data/BJPricingExperimentData.csv")
BJP.testData <- redwoodParser(data = BJP.testData,
                          keys = c("state", "actions", "targets"))
