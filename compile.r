####################
## PSCF program
## ~compile.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

#!/usr/bin/Rscript
Sweave("Readme.Rnw")
system("pdflatex Readme")
Stangle("PSCFdemo.Rnw")
Sweave("PSCFdemo.Rnw")
system("pdflatex PSCFdemo")
system("pdflatex PSCFdemo")
filerm <- function(x) invisible(Map(file.remove,x))
filerm(list.files("figures","fig[-].+",full=TRUE))
filerm(list.files(,"\\.(aux|log)$"))
file.remove("Rplots.pdf")
