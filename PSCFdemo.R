###################################################
### chunk number 1:  eval=FALSE
###################################################
##   install.packages("chron",repos="http://cran.r-project.org")


###################################################
### chunk number 2: 
###################################################
  if( file.exists("figures") ) {
    invisible(Map(file.remove,list.files("figures","^fig[-][0-9]{3,3}\\.pdf$",
                                         full=TRUE)))
  } else {
    dir.create("figures")
  }


###################################################
### chunk number 3: 
###################################################
##system("svn co https://svn.r-project.org/R/trunk/share/texmf/")
  invisible(capture.output({
    library(chron)
    library(fields)
    library(maps)
    library(mapproj) 
    library(akima)
  }))
mapf.env <-
  (if( all(regexpr("mapfunctions",search())<0) )
   attach(NULL,2,name="mapfunctions")
  else
   pos.to.env(grep("mapfunctions",search())))
sys.source("functions/pscf_functions.r",mapf.env)
source("functions/classdef.r")
options(stringsAsFactors=FALSE)


###################################################
### chunk number 4: 
###################################################
Coords_file <- "outputs/coords_vocals.rda"
Group_file <- "userinputs/groupfile-example_alcf.txt"



###################################################
### chunk number 5: 
###################################################
head(read.delim(Group_file,row.names=1))



###################################################
### chunk number 6: 
###################################################
trajectories <- readtrajectories(Coords_file) 
trajectories <- shorten(trajectories,ndays=3)



###################################################
### chunk number 7: 
###################################################
trajectories <- random(trajectories,fraction=0.5)



###################################################
### chunk number 8: 
###################################################
mp <- definemap("world",xlim=c(-110,-50),ylim=c(-60,5))


###################################################
### chunk number 9: 
###################################################
trajectories <- transform(trajectories,mp)


###################################################
### chunk number 10: 
###################################################
xygrid <- definegrid(traj=trajectories,len=40)



###################################################
### chunk number 11:  eval=FALSE
###################################################
## xygrid <- definegrid(longrid=seq(-95,50,5),latgrid=seq(35,93,3))


###################################################
### chunk number 12: 
###################################################
par(mfrow=c(1,2),mar=rep(1,4))
showmap(mp,gridlines=TRUE)
showmap(mp,xygrid)



###################################################
### chunk number 13: 
###################################################
trajectories <- overlay(trajectories,xygrid,identity)



###################################################
### chunk number 14: 
###################################################
groups(trajectories) <- readgroups(Group_file)
trajectories <- addfirst(trajectories)
trajectories <- prepareforvis(trajectories,xygrid)



###################################################
### chunk number 15: 
###################################################

par(mfrow=c(1,2),mar=rep(1,4))
showmap(mp,trajectories,
        type="spaghetti",groupindex=0,gridlines=TRUE)
showmap(mp,trajectories,
        type="density",groupindex=0,gridlines=TRUE)



###################################################
### chunk number 16: 
###################################################
  par(mfrow=c(1,1),mar=c(4.5,4.5,1.5,1.5),mgp=c(2.5,1,0),pty="s")
  cumuldensp(trajectories)


###################################################
### chunk number 17: 
###################################################

ngr <- length(groups(trajectories))
layout(matrix(1:(ngr+1),nrow=1),width=c(rep(5,ngr),1))
par(mar=c(1,1,1.5,1),mgp=c(1,1,0),lend=3,pty="s")
for( i in 1:ngr ) {
  showmap(mp,trajectories,threshold=0.4,
          type="pscf",groupindex=i,gridlines=TRUE)
  title(main=grpname(trajectories,i),cex.main=1.2)
}
addlegend(m1=2,m2=1.5,m3=2,m4=2,mgp=c(2,.5,0),cex.axis=0.6)



###################################################
### chunk number 18:  eval=FALSE
###################################################
## shiptrack <-
##   lapply(colnames(coords(trajectories))[2:1],function(x,y)
##          sapply(y[,x],`[`,1),coords(trajectories))


###################################################
### chunk number 19:  eval=FALSE
###################################################
## mp <- definemap("world")
## 


###################################################
### chunk number 20:  eval=FALSE
###################################################
## showmap(mp,shiptrack=shiptrack,
##         projection="orthographic",orientation=c(90,0,-12.5))
## 


###################################################
### chunk number 21:  eval=FALSE
###################################################
## par(mfrow=c(1,2),mar=rep(1,4))
## showmap(mp,gridlines=TRUE)
## showmap(mp,xygrid)
## 


###################################################
### chunk number 22:  eval=FALSE
###################################################
## 
## ngr <- length(groups(trajectories))
## layout(matrix(1:(ngr+1),,nrow=1),width=c(rep(5,ngr),1))
## par(mar=c(1,1,1.5,1),mgp=c(1,1,0),lend=3,pty="s")
## for( i in 1:ngr ) {
##   showmap(mp,trajectories,
##           shiptrack=shiptrack,
##           type="pscf",gridlines=TRUE,groupindex=i,
##           projection="stereographic")
##   title(main=grpname(trajectories,i),cex.main=1.2)
## }
## addlegend(m1=2,m2=1.5,m3=2,m4=2,mgp=c(2,.5,0),cex.axis=0.6)
## 


###################################################
### chunk number 23: 
###################################################
showmap(mp,trajectories,type="pscf",gridlines=TRUE,groupindex=1,ninterp=30)
title(main=grpname(trajectories,1),cex.main=1.2)



###################################################
### chunk number 24: 
###################################################
output <- extract(trajectories,type="pscf",groupindex=1,threshold=0.4)


###################################################
### chunk number 25:  eval=FALSE
###################################################
## write(output$x,file="xvalues.txt",ncol=1)
## write(output$y,file="yvalues.txt",ncol=1)
## write(t(output$z),file="zvalues.txt",ncol=ncol(output$z)) 


###################################################
### chunk number 26: 
###################################################
image(output,col=grey.colors(64),asp=1)


