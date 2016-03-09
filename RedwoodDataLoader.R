require(RJSONIO)    
require(dplyr)


redwoodParser <- function(data, keys){
  
  # keep only desired keys
  data <- data %>% 
    dplyr::filter(Key %in% keys)
  
  # convert json formatted column into "wide table" format
  #  first 5 rows just do subtle string manipulation....
  KeyData <- data %>% select(Value) %>% mutate(Value = as.character(Value)) %>% tbl_df
  KeyData <- paste(unlist(KeyData), collapse=",")
  KeyData <- paste(c("[", KeyData, "]"), collapse="")
  KeyData <- fromJSON(KeyData) #requires RJSONIO

  KeyData <- lapply(KeyData, function(x) {
    x[sapply(x, is.null)] <- NA
    data.frame(x,stringsAsFactors=F)
  })
  KeyData <- bind_rows(KeyData) # requires dplyr
  
  # recombine
  data <- data %>% select(-Value)
  data <- bind_cols(data, KeyData)
}

# Example ==========
# 
# testData <- read.csv("ExperimentData.csv")
# testData <- redwoodParser(data = testData,
#               keys = c("trade", "cancel","offer_text"))

