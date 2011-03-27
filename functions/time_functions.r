####################
## PSCF program
## ~time_functions.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

format_chron <- function(x,fmt="%Y-%m-%d %H:%M:%S")
  ## chr is a chron object
  ## example usage: format(chronobject,"%m/%d")
  format(as.POSIXct(paste(as.Date(dates(x)),times(x)%%1)),fmt)
chron2POSIXct <- function(x,tz="")
  ## this character converts chron object to POSIXct
  as.POSIXct(formatchron(x),tz=tz)
num2POSIXct <- local({
  now <- Sys.time()
  d <- diff(sapply(c("","GMT"),function(x) as.POSIXct(format(now),x)))
  function(x)
    ## this function converts integers (numeric) converted from POSIXct
    ## with tz = "" back to POSIXct time with tz = ""
    as.POSIXct(format(ISOdatetime(1970,1,1,0,0,0,"GMT")+x+d))
})
