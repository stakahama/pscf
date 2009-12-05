####################
## PSCF program
## ~extract.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################


setGeneric("extract",function(traj,type,...) standardGeneric("extract"))
setMethod("extract","Traj",function(traj,type,...) {
  if( !type %in% c("density","pscf") ) stop("not available")
  mat <- quote(if (irreg) projectsurface(gr(xygrid),s) else makesurf(gr(xygrid),s))
  b <- body(traj@funcs[[type]])
  f <- `body<-`(traj@funcs[[type]],value=`[[<-`(b,length(b),mat))
  do.call(f,(function(x) `names<-`(x,sub("^groupindex$","i",names(x))))(list(...)))
})

## example usage
## source("functions/extract.r")
## output <- extract(trajectories,type="pscf",groupindex=1)
## image(output)
## writeLines(format(output$x),con="xvalues.txt")
## write(output$x,file="xvalues.txt",ncol=1)
## write(t(output$z),file="zvalues.txt",ncol=ncol(output$z)) 

