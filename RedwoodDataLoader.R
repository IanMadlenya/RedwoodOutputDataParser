require(jsonlite)    
require(dplyr)
require(tidyr)
source("r/helperfunctions.R")

# debugging --------------------------------------------------------------------

#   keys = c("state", "actions", "targets")
#   data=BJP.testData <- read.csv("examples/data/BJPricingExperimentData.csv")
#   subdata=data[[1]]

#   keys = c("trade", "cancel","offer_text")
#   data = CMtestData
#   subdata=data[[1]]

#   keys = c("LOG_CONFIG","state", "actions", "targets")
#   data = Bub.testData
#   keys = c("state", "updateAction", "endofsubperiod")
#   subdata=data[[1]]

# Debugging CUNY Matching =======
#   data = matching.testData <- read.csv("examples/data/Matching-Steve Active Dev -strategy method.csv")
#   keys = c("strategy","action","action_round", "action_owntype",
#          "action_partnertype","action_partnerid",
#          "exit", "repeataction_owntype",
#          "repeataction_partnertype","repeataction_partnerid")

# Debugging TTM ========= 
# ttm_data  = read.csv("examples/example_ttm/TTM-Baruch-2016-04-15 13_54_53.101035 (1).csv")
# ttm_data_round <- ttm_data %>% dplyr::filter(Period == 56)
# data = ttm_data_round
# keys = "rp.round_started"


# Functions ------------------
redwoodParser <- function(data, keys){
  
  # keep only desired keys
  data <- data %>% 
    mutate(Key = as.character(Key)) %>%
    mutate(Time = Time %/% 100000000) %>%
    select(-ClientTime) %>%
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
  
    
    # "fromJSON" converts the json objects in each row of the "Value" col
    # into R objects. these may be lists, vectors, data.frames, NULL, single values, etc. 
    # the follow if-else statements hand these different types
    # if you get an error, it may be that there does not yet exist a case that handles
    # that particular data structure. 
    
    if (length(KeyData) == 0){
      # if obj is empty (oft used to indicate the event indicated by "key")
      # e.g. "subperiod_end", just needs time and Key, not Value contents. 
      subdata <- subdata %>%
        mutate(NewCol = TRUE)
      names(subdata)[length(names(subdata))] = unique(subdata$Key)
      subdata <- subdata %>% select(-Value)
      
    } else if (typeof(KeyData[[1]][[1]]) == "list"){
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
      # when the value is a data.frame
      
      if (is.data.frame(KeyData) 
          & ncol(KeyData) == 1
          & length(KeyData[[1]][[1]]) != 1){
        
        nameMessage <- unique(subdata$Key) #setup name for new column
        KeyData = as.data.frame(t(as.data.frame(KeyData[[1]])))
        rownames(KeyData) = NULL
        names(KeyData) <- paste(nameMessage,(1:ncol(KeyData)), sep=".")
        subdata <- bind_cols(subdata,KeyData) %>%
          select(-Value)
        
      } else if (is.data.frame(KeyData) & ncol(KeyData) == 1){
        # when the value is a ONE-COLUMN dataframe
        nameMessage <- unique(subdata$Key) #setup name for new column
        subdata$Value = unlist(KeyData[1]) # put in parsed-json-value
        names(subdata)[which(names(subdata) == "Value")] = paste(nameMessage,names(KeyData), sep = ".") #rename
        
      } else {
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
      }
    } else if (class(KeyData) == "matrix"){
      nameMessage <- unique(subdata$Key)
      KeyData <- as.data.frame(KeyData)
      names(KeyData) <- paste(nameMessage,(1:ncol(KeyData)), sep=".")
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
  
  if (length(keys) == 1){ # simple case; only want one key
    output <- rw_parsr(data)
  } else { # dealing with multiple keys
    output <- data[0,] %>% select(Period, Sender, Group, Time, Key)
    data <- split(data, data$Key)
    for (subdata in data){
      subdata <- rw_parsr(subdata)
      
      #recombine
      output <- full_join(output, subdata,
                          by = c("Period","Group","Sender","Time")) 
      
      # merge keys into one column
      if ("Key.x" %in% names(output)){
        output <- output%>%
          mutate(Key.x = as.character(ifelse(is.na(Key.x), "", Key.x)),
                 Key.y = as.character(ifelse(is.na(Key.y), "", Key.y))
          ) %>%
          unite(Key, starts_with("Key"), sep = " ")        
      }
      

    }
  }
  

  # sort by time
  output <- output %>%                           
    mutate(datetime = (rwp_myformat.POSIXct(Time/10, digits = 3))) %>% #see helper functions
    select(Period, Group, Sender, datetime, everything()) %>%
    arrange(Time)
  
}

rwp <- redwoodParser

# New Parser -------------------------------------------------------------------

rwp2 <- function(data, keys){
  
  # keep only desired keys
  data <- data %>% 
    mutate(
      Period = as.integer(Period),
      Group = as.integer(Group),
      Sender = as.character(Sender),
      Key = as.character(Key),
      Value = as.character(Value)) %>%
    mutate(Time = Time %/% 100000000 / 10) %>%
    select(-ClientTime) %>%
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
    
    names(KeyData)
    
    
    
    
    
    KeyData = as.Node(KeyData)
    
    
    KeyData %>% ToDataFrameTable
    
    # New JSON handling Section
    ListObNameVal <- function(Val,Nam){
      if (is.list(...)){
        ...
      } else {
        
      }
    }
    
    length(KeyData)
    length(KeyData[1])
    length(KeyData[[1]][[1]])
    class(length(KeyData[[1]][[1]]))
    typeof(length(KeyData[[1]]))
    
    
      KeyData <- lapply(KeyData, function(x) {
        x[sapply(x, is.null)] <- NA
        unlist(x)
      })
    
      KeyData <- do.call("rbind", KeyData) %>%
        t() %>%
      
      nameMessage <- unique(subdata$Key)
      names(KeyData) = paste(nameMessage,names(KeyData), sep=".")
      
      subdata <- bind_cols(
        (subdata %>%
           select(-Value)),
        KeyData
      )
      
    
    
    
    # "fromJSON" converts the json objects in each row of the "Value" col
    # into R objects. these may be lists, vectors, data.frames, NULL, single values, etc. 
    # the follow if-else statements hand these different types
    # if you get an error, it may be that there does not yet exist a case that handles
    # that particular data structure. 
    if (length(KeyData) == 0){
      subdata <- subdata %>%
        mutate(NewCol = TRUE)
      names(subdata)[length(names(subdata))] = unique(subdata$Key)
      subdata <- subdata %>% select(-Value)
      
    } else if (typeof(KeyData[[1]][[1]]) == "list"){
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
      # when the value is a data.frame
      
      if (is.data.frame(KeyData) & ncol(KeyData) == 1){
        # when the value is a ONE-COLUMN dataframe
        nameMessage <- unique(subdata$Key) #setup name for new column
        subdata$Value = unlist(KeyData[1]) # put in parsed-json-value
        names(subdata)[which(names(subdata) == "Value")] = paste(nameMessage,names(KeyData), sep = ".") #rename
        
      } else {
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
      }
    } else if (class(KeyData) == "matrix"){
      nameMessage <- unique(subdata$Key)
      KeyData <- as.data.frame(KeyData)
      names(KeyData) <- paste(nameMessage,(1:ncol(KeyData)), sep=".")
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
  
  if (length(keys) == 1){ # simple case; only want one key
    output <- rw_parsr(data)
  } else { # dealing with multiple keys
    output <- data[0,] %>% select(Period, Sender, Group, Time, Key)
    data <- split(data, data$Key)
    for (subdata in data){
      subdata <- rw_parsr(subdata)
      
      #recombine
      output <- full_join(output, subdata,
                          by = c("Period","Group","Sender","Time")) 
      
      # merge keys into one column
      if ("Key.x" %in% names(output)){
        output <- output%>%
          mutate(Key.x = as.character(ifelse(is.na(Key.x), "", Key.x)),
                 Key.y = as.character(ifelse(is.na(Key.y), "", Key.y))
          ) %>%
          unite(Key, starts_with("Key"), sep = " ")        
      }
      
      
    }
  }
  
  
  # sort by time
  output <- output %>%                           
    mutate(datetime = (rwp_myformat.POSIXct(Time/10, digits = 3))) %>% #see helper functions
    select(Period, Group, Sender, datetime, everything()) %>%
    arrange(Time)
  
}





