####################
## PSCF program
## ~pscf_functions.r~
## $Rev: 3 $
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

str2chron <- function(x,fmt="%y_%m_%d_%H",subf=function(x)
                      paste(x,collapse="_"),...)
  as.chron(strptime(sub(sprintf("^%s",subf(rep("([0-9]{2})",4))),
                        subf(sprintf("\\%d",1:4)),x),fmt))
df2chron <- function(x,fmt)
  `[<-`(x,,value=lapply(x,function(x) str2chron(x,fmt)))


## =======================================================
##  read HYSPLIT files
## =======================================================
## .. moved to 'forcoords.r'

## =======================================================
##  Grid functions
## =======================================================
makegrid <- function(x,y) matrix(rep(x,length(y)),ncol=length(y))
distfromgrid <- function(xygrid) {
  latgrid <- makegrid(xygrid$y,xygrid$x)
  longrid <- t(makegrid(xygrid$x,xygrid$y))
  function(lon,lat)
    which.min((longrid-lon)^2+(latgrid-lat)^2)
    ## apply(outer(c(longrid),c(lon),`-`)^2 +
    ##       outer(c(latgrid),c(lat),`-`)^2,2,
    ##       which.min)
}

## =======================================================
## interpolation (for trajectory) functions
interp <- function(X,n)
  tryCatch('names<-'(with(X,approx(lat,lon,
                          do.call(seq,as.list(c(range(lat,na.rm=TRUE),
                                                length=length(lat)*n))))),
            c("lat","lon")),error=function(e) list(lat=NA,lon=NA))

## =======================================================
##  for plotting
replwzero <- function(x) replace(x,is.na(x),0)
replwna <- function(x) replace(x,x==0,NA)
isg <- interp.surface.grid
makesurf <- function(xygrid,mat,len=NULL) {
  if(is.null(len)) len <- 100
  surf <-
    isg(obj=c(xygrid,list(z=replwzero(mat))),
        grid.list=lapply(xygrid,function(x)
        do.call(seq,as.list(c(range(x),length=len)))))
  surf$z[] <- replwna(surf$z)
  surf
}

## =======================================================
##  weighting function
timeweights <- function(coords,...) {
  tms <- (function(x) as.numeric(str2chron(x,...))
          )(rownames(coords))
  fun <- function(s,e) {
    w <- rep(0,length(tms))
    w[which(tms >= s & tms <= e-1/24)] <- 1
    x <- which(tms > s & (tms-s) < 1/24)-1
    if( length(x) > 0 ) w[x] <- w[x]+abs(tms[x]-s)*24
    y <- which(tms < e & (e-tms) < 1/24)
    if( length(y) > 0 ) w[y] <- w[y]+abs(e-tms[y])*24
    w
  }
  function(Start,End) {
    if(length(Start)==1) fun(as.numeric(Start),as.numeric(End)) else
    rowSums(mapply(fun,as.numeric(Start),as.numeric(End)))
  }
}

## =======================================================
##  accounting function
accumulator <- function(xygrid,coords,retIndex,n.interp=4,...) {
    if(!is.null(n.interp))
        icoords <- do.call(rbind,apply(coords,1,interp,n.interp))
    else
        icoords <- `[<-`(coords[,1:2],,value=
                         lapply(coords[,1:2],na.omit))
                                
  function(wts,type=unique) {

    ## returns a matrix

    sel <- which(wts > 0)

    if( length(sel)==0 ) 
      return(matrix(NA,nrow=length(xygrid$y),ncol=length(xygrid$x)))

    if( identical(type,identity) ) {
      ## method 1: count trajectory points (default)
      ## type == identity()
      
      indices <- retIndex(do.call(cbind,icoords[sel,"lon"]),
                          do.call(cbind,icoords[sel,"lat"]))
      
      wtvec <- rep(wts[sel],sapply(icoords[sel,"lat"],length))        

      agg <- aggregate(list(Wts=c(wtvec)),by=list(Index=c(indices)),
                       sum,na.rm=TRUE)
      
    } else if( identical(type,unique) ) {
      ## method 2: count trajectories
      ## type == unique()
      ## (more general; can actually be used for method 1 but
      ## repeated call to identity() creates overhead.)
      
      indices <- Map(function(x,y) unique(retIndex(x,y)),
                     icoords[sel,"lon"],
                     icoords[sel,"lat"])
      
      wtvec <-
        Map(function(w,x) rep(w,times=length(unlist(x))),
            wts[sel],indices)
      
      agg <- aggregate(list(Wts=unlist(wtvec)),
                       by=list(Index=unlist(indices)),
                       sum,na.rm=TRUE)
    }
    ###
    container <-
      replace(matrix(NA,nrow=length(xygrid$y),ncol=length(xygrid$x)),
              as.integer(agg$Index),agg$Wts)
  }
}

## =======================================================
##  Surface projection
## =======================================================
morepoints <- function(x,n) if(is.null(n)) x else seq(min(x),max(x),,n)
projectsurface <- function(grid,z,npts=NULL) {
  if( is.null(npts) ) npts <- 100
  lonlat <- do.call(mapproject,do.call(expand.grid,grid))[1:2]
  akobj <- do.call(akima::interp,
                   c(subset(data.frame(lonlat,z=c(replwzero(z))),!is.na(z)),
                     `names<-`(lapply(lonlat,morepoints,npts),c('xo','yo'))))
  akobj$z[] <- replwna(akobj$z)
  akobj
}

getmapc <- function(db,xlim,ylim) {
  mp <- do.call(map,(function(x) x[sapply(x,length)>0]
                     )(list(db,xlim=xlim,ylim=ylim,plot=FALSE)))
  if( length(xlim) > 0 ) {
    w <- which(mp$x < min(xlim) | mp$x > max(xlim))
    mp$x <- mp$x[-w]; mp$y <- mp$y[-w]
  }
  mp
}  
## =======================================================
##  Cosmetic functions
## =======================================================
addgrid <- function(mp,xlim=NULL,ylim=NULL) {
  if(is.null(xlim) || is.null(ylim)) lim <- mp else lim <- c(xlim,ylim)
    map.grid(lim,lty=1,lab=FALSE,col=alpha("grey90"),xpd=TRUE)
  }
alpha <- function(x) do.call(rgb,as.list(c(col2rgb(x)/255,alpha=0.35)))
addlegend <- function(m1=10,m2=1,m3=10,m4=3,...) {
  ## m1-m4 are margin parameters
  par(mar=c(m1,m2,m3,m4),pty="m")
  (function(z) image(1,z,t(matrix(z)),col=tim.colors(64),ann=FALSE,
                     axes=FALSE))(seq(0,1,,64))
  box(); axis(4,las=1,...)
}
## =======================================================

