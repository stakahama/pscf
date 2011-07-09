## just an example of using latlon2km() function
## and windrose function

library(chron)
source("functions/windrose.r")
source("functions/lat-lon-dist.r")
load("outputs/coords.rda")

alt <- function(x="character") as(sub(".+\\-([0-9]+)$","\\1",rownames(coords)),x)
rosavent <- climatol::rosavent
formals(rosavent)$margen <- c(0,0,0,0)
body(rosavent)[[11]] <- quote(par(mar=margen))
body(rosavent) <-
  as.call(append(as.list(body(rosavent)),list(quote(par(fig=ex[ix,],new=TRUE)),
                                              quote(ix <<- ix + 1)),1))

ex <- expand.grid(c(0,.5),c(0,.5))
ex <- ex[rev(order(1:nrow(ex))),]
ex <- cbind(ex[,1],ex[,1]+.5,ex[,2],ex[,2]+.5)

pdf("outputs/windroses-met.pdf",width=9,height=6)
ix <- 1

p0 <- sapply(coords[1,2:1],`[`,1)
p1 <- t(apply(coords[,2:1],1,function(X) sapply(X,`[`,2)))
## testplot(p1,p0)
angle <- apply(p1,1,calcdegrees,p0)
win <- make.windfr(factor(alt()),angle)
rosavent(win, 4, 5, ang=-3*pi/16, uni="altitude")
p1 <- do.call(rbind,
              lapply(c(2,24,48),function(i)
                     cbind(t(apply(coords[alt()=="100",2:1],1,function(X) sapply(X,`[`,i))),
                           hour=i)))
angle <- apply(p1[,1:2],1,calcdegrees,p0)
win <- make.windfr(factor(as.integer(p1[,3])),angle)
rosavent(win, 4, 10, ang=-3*pi/16, uni="hours back")

p1 <- t(apply(coords[alt()=="100",2:1],1,function(X) sapply(X,`[`,2)))
angle <- apply(p1,1,calcdegrees,p0)
time <- as.chron(substring(rownames(coords[alt()=="100",]),1,11),"%y_%m_%d_%H")-8/24
timeofday <- function(h)
  factor(findInterval(h,c(7,13,19)) %% 3,0:2,c("evening","morning","afternoon"))
win <- make.windfr(timeofday(hours(time)),angle)
rosavent(win, 4, 5, ang=-3*pi/16, uni="time of day")

nf <- read.delim("../../data/Met/Naifang-categories.txt")
p1 <- t(apply(coords[alt()=="100",2:1],1,function(X) sapply(X,`[`,2)))
angle <- apply(p1,1,calcdegrees,p0)
time <- as.chron(substring(rownames(coords[alt()=="100",]),1,11),"%y_%m_%d_%H")-8/24
win <- make.windfr(with(nf,Met.category[match(format(dates(time)),
                           format(dates(Date)))]),
                   angle)
rosavent(win, 4, 5, ang=-3*pi/16, uni="met categories (MCE2)")

dev.off()


p0 <- sapply(coords[1,2:1],`[`,1)
p1 <- t(apply(coords[alt()=="100",2:1],1,function(X) sapply(X,`[`,2)))
## testplot(p1,p0)

angle <- apply(p1,1,calcdegrees,p0)
distkm <- mapply(latlon2km,
                 lat1=p1[,"lat"],lon1=p1[,"lon"],
                 MoreArgs=structure(as.list(rev(p0)),names=c("lat2","lon2")))

win <- make.windfr(cut(distkm*1e3/3600,breaks=c(0,2,5,10)),angle)

pdf("outputs/windroses-speed.pdf",width=5,height=5)
climatol::rosavent(win, 4, 5, ang=-3*pi/16, uni="speed (m/s)",
                   margen=rep(0,4))
dev.off()

newdfr <- data.frame(hr=hours(as.chron(substring(rownames(coords),1,11),"%y_%m_%d_%H")-8/24),
                     speed=distkm*1e3/3600,
                     alt=alt())
library(lattice)

out <- xyplot(speed~hr | alt, data=newdfr,
              strip=FALSE,strip.left=TRUE,
              cex=.6,
              xlab="Hour of day (PST)",layout=c(1,3),
              ylab="Approx. wind speed (m/s)")

pdf("outputs/windspeeds.pdf",width=8,height=6)
print(out)
dev.off()
