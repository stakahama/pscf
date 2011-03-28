####################
## PSCF program
## ~generateInput.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

source("functions/time_functions.r")
library(chron)

## === user input ===
Start_lat <- 32.52
Start_lon <- -117.03700
Start_alt <- c(10,100,500) ##10
Start_times <- seq(from=as.chron("5/15/10 00:00:00","%m/%d/%y %T"),
                   to=as.chron("7/01/10 00:00:00","%m/%d/%y %T"),
                   by=times("01:00:00"))
                                        # alternatively: by="hour"
                                        # and so on
Run_hours <- -144
Vert_coord <- 0
Model_top <- 1000000

## === output ===
fm <- function(x) formatC(x,format="f")
ParmTable <- expand.grid(Latitude=fm(Start_lat),
                         Longitude=fm(Start_lon),
                         Altitude=fm(Start_alt),
                         Time=format(Start_times,enclosed=rep("",2)),
                         Run_hours=Run_hours,
                         Vert_coord=Vert_coord,
                         Model_top=fm(Model_top))
write.table(ParmTable,"userinputs/input.txt",
            row.names=FALSE,quote=FALSE,sep="\t")
