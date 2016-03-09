require(RJSONIO)    
require(dplyr)


redwoodParser <- function(data, keys){
  data <- data %>% 
    dplyr::filter(Key %in% keys)
  
  KeyData <- data %>% select(Value) %>% mutate(Value = as.character(Value)) %>% tbl_df
  KeyData[1,] <- paste(c("[",KeyData[1,]), collapse = "")
  KeyData[nrow(KeyData),] <- paste(c(KeyData[nrow(KeyData),],"]"), collapse = "")
  KeyData <- paste(unlist(KeyData), collapse=",")
  KeyData <- fromJSON(KeyData)
  
  KeyData <- lapply(KeyData, function(x) {
    x[sapply(x, is.null)] <- NA
    data.frame(x)
  })
  KeyData <- bind_rows(KeyData)
  
  data <- data %>% select(-Value)
  data <- bind_cols(data, KeyData)
}

# Example ==========
# 
# testData <- read.csv("ExperimentData.csv")
# testData <- redwoodParser(data = testData,
#               keys = c("trade", "cancel","offer_text"))

