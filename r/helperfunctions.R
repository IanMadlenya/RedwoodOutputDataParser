# Helper function

# POSIXct, with formatting for fractional seconds
myformat.POSIXct <- function(x, digits=0) {
  x2 <- round(unclass(x), digits)
  attributes(x2) <- attributes(x)
  x <- as.POSIXlt(x2, origin = "1970-01-01")
  x$sec <- round(x$sec, digits)
  format.POSIXlt(x, paste("%Y-%m-%d %H:%M:%OS",digits,sep=""))
}
# debugging
# myformat.POSIXct(BJP.testData$ClientTime[1:5], 3)
# returns: "2016-03-09 08:33:19.069"
