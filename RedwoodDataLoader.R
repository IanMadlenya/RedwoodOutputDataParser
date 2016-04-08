require(jsonlite)    
require(dplyr)
source("r/helperfunctions.R")

#   debugging
#   keys = c("state", "actions", "targets")
#   data=BJP.testData <- read.csv("examples/BJPricingExperimentData.csv")
#   subdata=data[[1]]

#   keys = c("trade", "cancel","offer_text")
#   data = CMtestData
#   subdata=data[[1]]

#   keys = c("LOG_CONFIG","state", "actions", "targets")
#   data = Bub.testData
#   subdata=data[[1]]


redwoodParser <- function(data, keys){
  
  # keep only desired keys
  data <- data %>% 
    mutate(Key = as.character(Key)) %>%
    dplyr::filter(Key %in% keys)
  
  output <- data.frame()
  data <- split(data, data$Key)
  for (subdata in data){
    KeyData <- subdata %>% select(Value) %>% mutate(Value = as.character(Value)) %>% tbl_df
    KeyData <- paste(unlist(KeyData), sep = "",  collapse=",")
    KeyData <- paste(c("[", KeyData, "]"), collapse="")
    KeyData <- fromJSON(KeyData)
    
      if (typeof(KeyData[[1]][[1]]) == "list"){
      
      #used for multiline entries
      if (is.data.frame(KeyData[[1]][[1]])){
        data_repeatTimes <- sapply(KeyData, function(x) {sapply(x, nrow)}) 
      } else {
        data_repeatTimes <- sapply(KeyData, function(x) {sapply(x, length)})
      }
      
      # KeyData <- lapply(KeyData, function(x){
      #   lapply(x[[1]], function(y){
      #     bind_rows(data.frame(as.list(y)))
      #   })
      # })
      
      KeyData <- lapply(KeyData, bind_rows)
      KeyData <- bind_rows(KeyData)
      
      subdata <- subdata %>% select(-Value)
      subdata <- subdata[rep(seq_len(nrow(subdata)), times = data_repeatTimes),]
      subdata <- bind_cols(subdata, KeyData)
    } else if (class(KeyData) == "data.frame" & is.vector(KeyData[1,])){
      nameMessage <- unique(subdata$Key)
      
      # KeyData <- lapply(KeyData, function(x){
      #   (unlist(x))
      # })
      KeyData <- lapply(KeyData, function(x){
        as.data.frame(t(data.frame(x)),
                      stringsAsFactors = F)
      })
      KeyData <- bind_rows(KeyData)
      names(KeyData) <- paste(nameMessage,substr(names(KeyData),2,2), sep=".")
      
      subdata <- subdata %>% select(-Value)
      subdata <- bind_cols(subdata, KeyData) 
      
    } else  {
      KeyData <- bind_rows(KeyData)
      
      #rename col names, avoid mixmatch of identically named vars
      nameMessage <- unique(subdata$Key)
      names(KeyData) <- paste(nameMessage,(names(KeyData)), sep=".")
      
      subdata <- subdata %>% select(-Value)
      subdata <- bind_cols(subdata, KeyData)      
    }
    #recombine
    output <- bind_rows(output, subdata)
  }

  # sort by time
  output <- output %>% 
    mutate(datetime = (myformat.POSIXct(Time/1000000000, digits = 3))) %>% #see helper functions
    select(Period, Group, Sender, datetime, everything()) %>%
    arrange(Time)
  
}

