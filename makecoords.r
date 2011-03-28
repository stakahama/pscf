####################
## PSCF program
## ~makecoords.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

source("functions/forcoords.r")
eval(parse(text=readLines("userinputs/runHYSPLIT_parms.txt")))
coords <- makecoords(folder=Output_path,
                     prefix=Output_base)
                                        # note: 
                                        # folder is the same as Output_path
                                        # prefix is the same as Output_base
                                        # from 'runHYSPLIT.py'
save(coords,file=Coords_file)
