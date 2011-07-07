
strip.white <- function(x) sub("^[ ]+|[ ]+$","",x)
readmet <- function(x,ix=1,header=c("X1","X2","YEAR","MONTH","DAY","Hour","MIN","SEC","TRAJ_HOUR","LAT","LON","ALT","PRESSURE","THETA","AIR_TEMP","RAINFALL","MIXDEPTH","RELHUMID","SUN_FLUX")) {
  lines <- readLines(x)
  dfr <- data.frame(do.call(rbind,strsplit(strip.white(lines[(1+grep("PRESSURE",lines)):length(lines)]),"[ ]+")))
  dfr[] <- lapply(dfr,type.convert,as.is=TRUE)
  names(dfr)[ix:ncol(dfr)] <- header
  dfr[1,]
}

library(chron)
prefix <- "mdump-"

for( i in 0:2 ) {
  fi <- list.files(sprintf("trajectories-calmex-met_%d",i),prefix,full=TRUE)
  metmatrix <- do.call(rbind,lapply(fi,readmet))
  metmatrix <- cbind(datetime=as.chron(basename(fi),paste(prefix,"%y_%m_%d_%H-",sep="")),
                     alt=as.numeric(sub(".+[-]([0-9]+)$","\\1",basename(fi))),
                     metmatrix)
  metmatrix$datetime <- format(metmatrix$datetime,enclose=c("",""))
  write.table(metmatrix[,c("datetime","alt",eval(as.list(args(readmet))$header))],
              sprintf("outputs/met_%d.txt",i),sep="\t",
              row.names=FALSE,quote=FALSE)
}
