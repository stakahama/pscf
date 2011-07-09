library(climatol)

make.windfr <- function(category=NULL,angles,labels=NULL,breaks=NULL) {
  if( is.null(breaks) && is.null(labels) ) {
    labels <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
    breaks <- seq(0,360,,length(labels)+1)
    breaks[length(breaks)] <- breaks[length(breaks)]+1
  }
  direction <- cut(angles,breaks=breaks,labels=labels)
  t <- unclass(table(category,direction,dnn=NULL))
  as.data.frame(t)
}

calcdegrees <- function(p,p0,convention="geo",
                        var=c(geo="x",math="y")[convention],
                        unitv=list(geo=c(0,1),math=c(1,0))[[convention]]) {
  ## p, p0 should have two components corresponding to (x,y)
  ## convention is either "geo"  (clockwise from (0,1))
  ##                   or "math" (counterclockwise from (1,0))
  centered <- structure(p-p0,names=c("x","y"))
  angle <- acos(crossprod(centered/sqrt(crossprod(centered)),unitv))*180/pi
  ifelse(centered[var]>=0,angle,360-angle)
}

testplot <- function(p1,p0) {
  n1 <- `colnames<-`(sweep(p1,2,p0,`-`),c("x","y"))
  n1[] <- t(apply(n1,1,function(x) x/sqrt(crossprod(x))))
  plot(c(-1,1),c(-1,1),type="n",asp=1)
  segments(0,0,n1[,1],n1[,2])  
}

