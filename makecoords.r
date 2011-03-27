####################
## PSCF program
## ~makecoords.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

source("functions/forcoords.r")
coords <- makecoords(folder="trajectories/",
                     prefix="tdump-")
                                        # note: 
                                        # folder is the same as Output_path
                                        # prefix is the same as Output_base
                                        # from 'runHYSPLIT.py'
save(coords,file="outputs/coords.rda")
