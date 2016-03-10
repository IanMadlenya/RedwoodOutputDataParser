require(RJSONIO)    
require(dplyr)


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
    
    if (typeof(KeyData[[1]][[1]])=="list"){
      data_repeatTimes <- sapply(KeyData, function(x) {sapply(x, length)}) #used for multiline entries

      data.frame(as.list(KeyData[[1]][[1]][[1]]))
      
      
      KeyData = lapply(KeyData, function(x){
        lapply(x[[1]], function(y){
          bind_rows(data.frame(as.list(y)))
        })
      })
      
      KeyData <- lapply(KeyData, bind_rows)
      KeyData <- bind_rows(KeyData)
      
      subdata <- subdata %>% select(-Value)
      subdata <- subdata[rep(seq_len(nrow(subdata)), times = data_repeatTimes),]
      subdata <- bind_cols(subdata, KeyData)
    } else if (is.vector(KeyData)){
      KeyData <- lapply(KeyData, function(x){
        (unlist(x))
      })
      KeyData <- lapply(KeyData, function(x){
        as.data.frame(t(data.frame(x)),
                      stringsAsFactors = F)
      })
      KeyData <- bind_rows(KeyData)
      subdata <- subdata %>% select(-Value)
      subdata <- bind_cols(subdata, KeyData) 
      
    } else{
      KeyData <- bind_rows(KeyData)
      KeyData <- lapply(KeyData, as.data.frame)
      subdata <- subdata %>% select(-Value)
      subdata <- bind_cols(subdata, KeyData)      
    }
    #recombine
    output <- bind_rows(output, subdata)
  }

  # sort by time
  output <- output %>% 
    arrange(Time)
  
}








