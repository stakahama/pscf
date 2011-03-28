####################
## PSCF program
## ~runHYSPLIT.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

## ============== paths/files ======================
eval(parse(text=readLines("userinputs/runHYSPLIT_parms.txt")))
addslash <- function(x)
  if(substring(x,nchar(x),nchar(x))=="/") x else file.path(x,"")
Meteo_path <- addslash(Meteo_path)

## =============== process ======================
filekey <- function(filename) {
  x <- do.call(rbind,strsplit(filename,"\\."))
  as.POSIXct(strptime(paste(x[,2],(as.integer(sub("w","",x[,3]))-1)*7+1),
                      "%b%y %d"),tz="GMT")
}
sortmeteo <- function(x) {
  key <- filekey(x)
  x[!is.na(key)][order(na.omit(key))]
}
Meteo_files <- sortmeteo(list.files(Meteo_path))
Nmet <- length(Meteo_files)
Nloc <- 1

## ============== function defs ======================
time2time <- function(x) 
  format(as.POSIXct(x,"%m/%d/%y %T",tz="GMT"),"%y %m %d %H")

## ============== read data ======================
infile <- read.delim(Input_file,colClasses="character")

## ============  run HYSPLIT ======================
for( i in 1:nrow(infile) ) {
  file.remove(Control)
  Output_file <- paste(Output_base,
                       with(infile[i,],
                            sprintf("%s-%.0f",
                                    gsub("[ ]","_",time2time(Time)),
                                    as.numeric(Altitude))),sep="")
  with(infile[i,],
       cat(time2time(Time),                         ## time
           Nloc,                                    ## num loc
           paste(Latitude,Longitude,Altitude),
           Run_hours,
           Vert_coord,
           Model_top,                               ## coord
           Nmet,                                    ## num met
           rbind(rep(Meteo_path,Nmet),Meteo_files), ## met files
           Output_path,                             ## outpath
           Output_file,                             ## outfile
           sep="\n",
           filename=Control))
  system(Exec_file)
}
