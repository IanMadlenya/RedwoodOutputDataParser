require(dplyr)

# Helper functions

# POSIXct, with formatting for fractional seconds ==============================
rwp_myformat.POSIXct <- function(x, digits=0) {
  x2 <- round(unclass(x), digits)
  attributes(x2) <- attributes(x)
  x <- as.POSIXlt(x2, origin = "1970-01-01")
  x$sec <- round(x$sec, digits)
  format.POSIXlt(x, paste("%Y-%m-%d %H:%M:%OS",digits,sep=""))
}
# debugging
# myformat.POSIXct(BJP.testData$ClientTime[1:5], 3)
# returns: "2016-03-09 08:33:19.069"

# Merge rows by sender and time ================================================
#' that is, if a number of different Key/Messages are send by the same subject
#' at the same moment (approx same time), let's merges those multiple rows into 
#' one row. 
# 
# rwp_rowmerger <- function(data, keys = "all"){
#   # data: post parsed data
#   # keys are the keys you'd like to merge
#   if (keys != "all" | is.null(keys)){
#     data_sub1 <- data %>%  #working subdata
#       filter(Key %in% keys)
#     
#     data_sub2 <- data %>% #the rest, to be merged back later
#       filter(!(Key %in% keys))
#   }
#   
#   # be less precise about time: 
#   data_sub1 <- data_sub1 %>% 
#     mutate(
#       datetime2 = substr(datetime, 1, 22)
#     )
#   
#   data.splt <- split(data_sub1, 
#                      paste(data_sub1$Sender, data_sub1$datetime2))
#     
#   rmNAs <- function(vect){
#     # a function that checks vectors with >= NA and exactly-one non-NA
#     # and then replaces all NAs with the single non-NA
#     
#     if (length(vect) == 1){ # make sure you're dealing with an actual vector,
#       #othersise, just return the single value
#       return(vect)
#       
#     }
#     
#     if (sum(!is.na(vect)) == 1){ 
#       #ideal case: you ahve a single value, and all else NAs
#       # replace NAs with that single value
#       VAL = vect[!is.na(vect)]
#       vect[is.na(vect)] = VAL
#       
#     } else if (sum(!is.na(vect)) > 1 & sum(is.na(vect)) >= 1){
#       warning("Multiple messages sent by Sender in same Key at same Moment.
#               Row Merge is tricky. 
#               Consider limiting the Keys you merge on")
#     }
#     vect
#   } 
#   
#   # rmNAs(data.splt[[1]]$action)
#   # apply(data.splt[[1]], 2, rmNAs)
#   
#   rmNAs_lap <- function(data.1){
#     # a function to aply rmNAs over a sub data frame
#     # and then remove dupblicates
# 
#     data.1 = as.data.frame(data.1)
#     
#     if (nrow(data.1) == 1){
#       return(data.1)
#     }
# 
#     data.1 <- apply(data.1, 2, rmNAs)
#     data.1 <- dplyr::distinct(tbl_df(data.1), Period, Group, Sender, datetime2)
# 
#     return(class(data.1))
#     
#   }
#   temp = lapply(
#       data.splt, rmNAs_lap
#     )
# 
#   
#     
#   
#   
# }
# 
# # debugging
# 
# rwp_rowmerger()