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
Start_lat <- 32.12
Start_lon <- -116.97
Start_alt <- c(1000,1200)
Start_times <- seq.chron(from=strptime2chron("4/25/09 00:00:00"),
                         to=strptime2chron("4/26/09 00:00:00"),
                         by=times("02:00:00"))
                                        # alternatively: by="hour"
                                        # and so on
Run_hours <- -5
Vert_coord <- 0
Model_top <- 1000000

## === output ===
fm <- function(x) formatC(x,format="f")
DataFrame <- expand.grid(Latitude=fm(Start_lat),
                         Longitude=fm(Start_lon),
                         Altitude=fm(Start_alt),
                         Time=format(Start_times,enclosed=rep("",2)),
                         Run_hours=Run_hours,
                         Vert_coord=Vert_coord,
                         Model_top=fm(Model_top))
write.table(DataFrame,"userinputs/input.txt",
            row.names=FALSE,quote=FALSE,sep="\t")
