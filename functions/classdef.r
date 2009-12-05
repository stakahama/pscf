####################
## PSCF program
## ~classdef.r~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################


## =======================================================
##  XYGrid class
## =======================================================
setClass("XYGrid",representation(xygrid="list"))
definegrid <- function(longrid=NULL,latgrid=NULL,traj=NULL,len=50) {
  if(is.null(traj))
    new("XYGrid",xygrid=list(x=longrid,y=latgrid))
  else
    new("XYGrid",xygrid=
        (function(m) `names<-`(split(m,col(m)),c("x","y"))
         )(apply(coords(traj)[,c("lon","lat")],2,function(x)
                 (function(.x) seq(min(.x),max(.x),,len)
                  )(na.omit(unlist(x))))))
}
## =======================================================
##  Getter/setter functions
setGeneric("gr",function(object) standardGeneric("gr"))
setMethod("gr","XYGrid",function(object) object@xygrid)

## =======================================================
##  Traj class
## =======================================================
setClass("Traj",
         representation(coords="matrix",
                        groups="list",
                        accumulate="function",
                        accumultype="function",
                        tmwt="function",
                        funcs="list"))
readtrajectories <- function(fi) {
  load(fi)
  traj <- new("Traj",coords=coords)
  traj@funcs <- list(diagnose=function(i,...)
                     apply(coords(traj),1,function(X)
                           with(X,lines(mapproject(lon,lat),
                                        col=alpha("midnightblue")))))
  traj
}
readgroups <- function(groupfilename,fmt="%m/%d/%y %H:%M:%S")
  (function(x) split(df2chron(x[1:2],fmt),x$Group)
            )(read.delim(groupfilename,row.names=1,as.is=TRUE))

## =======================================================
##  Getter/setter functions
setGeneric("groups",function(obj) standardGeneric("groups"))
setMethod("groups","Traj",function(obj) obj@groups)
setGeneric("groups<-",function(obj,value) standardGeneric("groups<-"))
setReplaceMethod("groups","Traj",function(obj,value)
                 {obj@groups <- value; obj})
setGeneric("coords",function(obj) standardGeneric("coords"))
setMethod("coords","Traj",function(obj) obj@coords)
setGeneric("coords<-",function(obj,value) standardGeneric("coords<-"))
setReplaceMethod("coords","Traj",function(obj,value)
                 {obj@coords <- value; obj})
## =======================================================
##  Additional functions
setGeneric("shorten",function(traj,ndays) standardGeneric("shorten"))
setMethod("shorten","Traj",function(traj,ndays) {
  crd <- coords(traj)
  for( i in 1:nrow(crd) )
    crd[i,1:2] <-
      lapply(crd[i,1:2],`[`,1:(ndays*24+1))
  `coords<-`(traj,crd)
})
setGeneric("grpname",function(traj,index) standardGeneric("grpname"))
setMethod("grpname","Traj",function(traj,index) {
  g <- groups(traj)
  n <- sub("(.*)","(n=\\1)",nrow(g[[index]]))
  if( is.na(as.integer(names(g)[index]))) paste(names(g)[index],n)
  else paste("Group",index,n)
})
setGeneric("random",function(traj,fraction) standardGeneric("random"))
setMethod("random","Traj",function(traj,fraction) {
  if(missing(fraction)) fraction <- 0.5
  set.seed(200)
  traj@coords <-
    (function(m,f) m[sample(1:nrow(m),floor(nrow(m)*f)),]
     )(traj@coords,fraction)
  traj
})
setGeneric("cumuldensp",function(traj) standardGeneric("cumuldensp"))
setMethod("cumuldensp","Traj",function(traj) {
  ## retrieve data
  n <- get("campaigntot",environment(traj@funcs[["pscf"]]))
  y <- seq(0,1,,100)
  cdf <- function(n,y) list(x=quantile(n,y,na.rm=TRUE),y=y)
  lg <- do.call(`:`,as.list((function(x) sign(x)*ceiling(abs(x))
                             )(log10(range(n,na.rm=TRUE)))))
  ## make the plot
  par(las=1)
  cdens <- cdf(n,y)
  plot(cdens,type="n",axes=FALSE,ann=FALSE,log="x",yaxs="i")
  abline(h=seq(0,1,.1),col=8); abline(v=10^lg,col=8)
  axis(1,at=10^lg,parse(text=paste(10,lg,sep="^")))
  axis(1,outer(1:9,10^lg),FALSE,tck=-.01)
  axis(2); box(); lines(cdens,col=4)
  title(xlab="Count/cell",
        ylab="Cumulative density")
})

## =======================================================
##  Map class
## =======================================================
setClass("Map",representation(database="character",
                              xlim="numeric",ylim="numeric",
                              borders="ANY"))
definemap <- function(db,xlim=numeric(0),ylim=numeric(0))
  new("Map",database=db,xlim=xlim,ylim=ylim,borders=getmapc(db,xlim,ylim))


## =======================================================
##  Multiply-dispatched functions
## =======================================================

## =======================================================
##  Overlay trajectory on grid
setGeneric("overlay",function(traj,xygrid,type,...)
           standardGeneric("overlay"))
setMethod("overlay",c("Traj","XYGrid"), function(traj,xygrid,type,...) {
  ## Attach data to functions
  retIndex <- distfromgrid(gr(xygrid))
  traj@accumulate <- accumulator(gr(xygrid),coords(traj),retIndex,...)
  traj@tmwt <- timeweights(coords(traj),...)
  traj@accumultype <- type    
  traj
})

## =======================================================
##  Split trajectories by group
setGeneric("prepareforvis",function(traj,xygrid,...)
           standardGeneric("prepareforvis"))
setMethod("prepareforvis",c("Traj","XYGrid"),function(traj,xygrid,...) {
  ## Apply functions
  wts <- sapply(traj@groups,function(R)
                rowSums(apply(R,1,function(X)
                              do.call(traj@tmwt,as.list(X)))))
  totwts <- rowSums(wts)
  campaigntot <- traj@accumulate(totwts,traj@accumultype)
  normalized <- lapply(split(wts,col(wts)),function(x,y,f)
                       traj@accumulate(x,f)/y,campaigntot,
                       traj@accumultype)
  
  cth <- function(x,threshold,y=t(get("campaigntot",environment(cth))))
    (if( !is.null(threshold) ) 
     replace(x,y < quantile(y,threshold,na.rm=TRUE), NA)
    else x) 

  irreg_grid <- quote({
    image(projectsurface(gr(xygrid),s,ninterp),col=tim.colors(ncol),
          add=TRUE,ann=FALSE,axes=FALSE)
    })
  reg_grid <- quote({
    image(makesurf(gr(xygrid),s,ninterp),col=tim.colors(ncol),
          add=TRUE,ann=FALSE,axes=FALSE)
    })    
  
  traj@funcs <- c(traj@funcs["diagnose"],list(spaghetti=function(i,...) {
    w <- (if( i==0 ) TRUE else wts[,i] > 0)
    apply(coords(traj)[w,],1,function(X)
          with(X,lines(mapproject(lon,lat),col=alpha("midnightblue"))))
  },density=function(i,threshold=NULL,irreg=TRUE,ncol=64,ninterp=100) {
    s <- (if( i==0 ) t(campaigntot) else t(normalized[[i]]*campaigntot))
    s <- cth(s,threshold)
    if(irreg) eval(irreg_grid) else eval(reg_grid)
  },pscf=function(i,threshold=NULL,irreg=TRUE,ncol=64,ninterp=100) {
    s <- cth(t(normalized[[i]]),threshold)
    if(irreg) eval(irreg_grid) else eval(reg_grid)    
  }))
  traj
})

## The following function is not multiply dispatchced but follows the
## syntax of showmap() closely.
setGeneric("extract",function(traj,type,...) standardGeneric("extract"))
setMethod("extract","Traj",function(traj,type,...) {
  ## example usage
  ## source("functions/extract.r")
  ## output <- extract(trajectories,type="pscf",groupindex=1)
  ## image(output)
  ## writeLines(format(output$x),con="xvalues.txt")
  ## write(output$x,file="xvalues.txt",ncol=1)
  ## write(t(output$z),file="zvalues.txt",ncol=ncol(output$z)) 
  if( !type %in% c("density","pscf") ) stop("not available")
  mat <- quote(if (irreg) projectsurface(gr(xygrid),s) else makesurf(gr(xygrid),s))
  b <- body(traj@funcs[[type]])
  f <- `body<-`(traj@funcs[[type]],value=`[[<-`(b,length(b),mat))
  do.call(f,(function(x) `names<-`(x,sub("^groupindex$","i",names(x))))(list(...)))
})

## =======================================================
##  Map visualizations
## =======================================================
mpj <-
  function(parms,toadd) function(mobj,obj1,...) {
    ## functions
    rpv <- function(x,val) if(!is.null(x)) x else val    
    zerolist2null <- function(x) if(length(x) > 0) x else NULL
    ##
    parms <- c(parms,"shiptrack","gridlines","threshold","irregulargrid")
    args <- list(...)
    exargs <- zerolist2null(args[!names(args) %in% parms])
    mplist <- (function(x) x[sapply(x,length)>0]
               )(list(mobj@borders,xlim=mobj@xlim,ylim=mobj@ylim))

    ## set up plotting region
    local(rm(.Last.projection),globalenv())
    m <- do.call(map,c(mplist,type='n',exargs))
    ## add gridlines
    if( "gridlines" %in% names(args) && args$gridlines )
      #do.call(addgrid,mplist)
      addgrid(mobj@borders)
    ## add grid points    
    if( "grid" %in% toadd )
      points(do.call(mapproject,do.call(expand.grid,gr(obj1))),col="darkblue",
             pch=3)
    ## add trajectories    
    if( "trajectories" %in% toadd )
      obj1@funcs[[args$type]](args$groupindex,args$threshold,
                              irreg=rpv(args[["irregulargrid"]],TRUE),
                              ninterp=args$ninterp)
    ## draw borders
    do.call(map,c(mplist,list(col=8,add=TRUE,wrap=TRUE),exargs))
    ## add shiptrack
    if( !is.null(args$shiptrack) )
      lines(do.call(mapproject,args$shiptrack),col="maroon",lwd=2)
    invisible()
}

setGeneric("showmap",function(mobj,obj1,...) standardGeneric("showmap"))
setMethod("showmap","Map",mpj("",""))
setMethod("showmap",c("Map","XYGrid"),mpj("","grid"))
setMethod("showmap",c("Map","Traj"),
          mpj(c("groupindex","type","gridlines"),"trajectories") )
