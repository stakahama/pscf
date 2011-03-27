####################
## PSCF program
## ~forcoords.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

read.tdump <-
  function(con,hdr=c("ensemble","V2","year","month","day","hour",
                 "V7","V8","Dt","lat","lon","alt","hPa")) 
  (function(txt)
   as.data.frame('colnames<-'(do.call(rbind,lapply(strsplit(tail(txt,length(txt)-grep("PRESSURE",txt)),"[ ]+"),as.numeric))[,-1],hdr))
   )(readLines(con))
needn <- function(x,n=8) 
  while(TRUE) {
    if(nrow(x)>=n) return(x)
    x <- rbind(x,rep(NA,ncol(x)))
  }
sortbyname <- function(x) x[order(names(x))]
makecoords <-
  function(folder,prefix="tdump[_-]")
  t(sapply((function(x)
            sortbyname('names<-'(x,sub(prefix,"",basename(x))))
            )(list.files(folder,prefix,full=TRUE,rec=TRUE)),
           function(x) needn(read.tdump(x)[c("lat","lon","alt")])))
