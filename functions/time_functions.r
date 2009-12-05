####################
## PSCF program
## ~time_functions.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

formatchron <- function(x,fmt="%Y-%m-%d %H:%M:%S")
  ## chr is a chron object
  ## example usage: format(chronobject,"%m/%d")
  format(as.POSIXct(paste(as.Date(dates(x)),times(x)%%1)),fmt)
strptime2chron <- function(x,fmt="%m/%d/%y %T")
  ## x is a character vector.
  ## example usage: chronobject <- strptime2chron(c('2/27/09 12:00:00'))
  as.chron(strptime(x,fmt))
chron2POSIXct <- function(x,tz="")
  ## this character converts chron object to POSIXct
  as.POSIXct(formatchron(x),tz=tz)
seq.chron <- function(...) {
  ## this is a wrapper function for seq.POSIXt for chron objects
  ## from, to are chron objects
  ## additional arguments: by, length.out, along.with
  ## by can be:
  ## - a times object (e.g., times('00:15:00') or
  ##   times('00:30:00')-times('00:15:00')
  ## - A character string, containing one of "sec", "min",
  ##   "hour","day", "DSTday", "week", "month" or "year". This can
  ##   optionally be preceded by a (positive or negative) integer and
  ##   a space, or followed by "s".
  ## length.out: integer, optional. desired length of the sequence.
  ## along.with: take the length from the length of this argument.
  ## see ?seq.POSIXt for additional details
  as.chron(do.call(seq.POSIXt,
                   (function(lst) 
                    Map(function(x,m)
                        (if(!is.na(pmatch(m,c("from","to"))) ||
                            inherits(x,c("chron","dates")) )
                         ## from or to
                         chron2POSIXct(x,"GMT")
                        else if(!is.na(pmatch(m,"by")) &&
                                inherits(x,"times") &&
                                !inherits(x,c("chron","dates")))
                         ## by
                         unclass(x%%1)*24*3600
                        else
                         x),
                        lst,(function(.x) if(is.null(.x)) "" else .x
                             )(names(lst))))(list(...))))
}
num2POSIXct <- local({
  now <- Sys.time()
  d <- diff(sapply(c("","GMT"),function(x) as.POSIXct(format(now),x)))
  function(x)
    ## this function converts integers (numeric) converted from POSIXct
    ## with tz = "" back to POSIXct time with tz = ""
    as.POSIXct(format(ISOdatetime(1970,1,1,0,0,0,"GMT")+x+d))
})
