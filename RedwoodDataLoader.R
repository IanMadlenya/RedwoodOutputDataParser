require(jsonlite)    
require(dplyr)
source("r/helperfunctions.R")

#   debugging ---------
#   keys = c("state", "actions", "targets")
#   data=BJP.testData <- read.csv("examples/data/BJPricingExperimentData.csv")
#   subdata=data[[1]]

#   keys = c("trade", "cancel","offer_text")
#   data = CMtestData
#   subdata=data[[1]]

#   keys = c("LOG_CONFIG","state", "actions", "targets")
#   data = Bub.testData
#   subdata=data[[1]]


#   data = matching.testData
# keys = c("action", "action_owntype",
#           "action_partnertype", "exit",
#           "repeataction_owntype","repeataction_partnertype")


# ttm_data  = read.csv("examples/example_ttm/TTM-Baruch-2016-04-15 13_54_53.101035 (1).csv")
# ttm_data_round <- ttm_data %>% dplyr::filter(Period == 56)
# data = ttm_data_round
# keys = "rp.round_started"


# Functions ------------------

redwoodParser <- function(data, keys){
  
  # keep only desired keys
  data <- data %>% 
    mutate(Key = as.character(Key)) %>%
    dplyr::filter(Key %in% keys)

  
  # -----------------------------
  # Key-Level Parser WorkHorse
  # Given a subset of data for just one Key, apply this.
  # the complicated and tricky part is dealing with each type of "Value" redwood produces
  # these may be single values, or large data.frames, or lists, or somethese else. 
  rw_parsr <- function(subdata){
    KeyData <- subdata %>% select(Value) %>% mutate(Value = as.character(Value)) %>% tbl_df
    KeyData <- paste(unlist(KeyData), sep = "",  collapse=",")
    KeyData <- paste(c("[", KeyData, "]"), collapse="")
    KeyData <- fromJSON(KeyData)
    
    if (typeof(KeyData[[1]][[1]]) == "list"){
      # if the value is a complex json object
      
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
      
      #rename vars names, add Key-name to values
      nameMessage <- unique(subdata$Key)
      names(subdata)[7:ncol(subdata)] = paste(nameMessage, names(subdata)[7:ncol(subdata)], sep = ".")
      
    } else if (is.vector(KeyData)){
      #in the simple case, if value is just a single value (of any type)
      nameMessage <- unique(subdata$Key)
      names(subdata)[which(names(subdata) == "Value")] = nameMessage
      
    } else if (class(KeyData) == "data.frame" & is.vector(KeyData[1,])){
      #in the case that the value can be exprssed as a muli-row dataframe
      
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
      # in the case....
      
      KeyData <- as.data.frame(lapply(KeyData, cbind))
      
      KeyData <- bind_rows(KeyData)
      
      #rename col names, avoid mixmatch of identically named vars
      nameMessage <- unique(subdata$Key)
      names(KeyData) <- paste(nameMessage,(names(KeyData)), sep=".")
      
      subdata <- subdata %>% select(-Value)
      subdata <- bind_cols(subdata, KeyData)
    }
    
    return(subdata)
    
  }
  # -----------------------------
  
  #apply rw parswer workhorse 
  
  if (length(keys) == 1){
    output <- rw_parsr(data)
  } else {
    data <- split(data, data$Key)
    output <- data.frame()
    for (subdata in data){
      subdata <- rw_parsr(subdata)
      
      #recombine
      output <- bind_rows(output, subdata)
    }
  }
  
  

  # sort by time
  output <- output %>% 
    mutate(datetime = (myformat.POSIXct(Time/1000000000, digits = 3))) %>% #see helper functions
    select(Period, Group, Sender, datetime, everything()) %>%
    arrange(Time)
  
}

rwp <- redwoodParser

