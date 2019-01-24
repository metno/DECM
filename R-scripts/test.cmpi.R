# test.cmpi.R
setwd(system("find $HOME -name calculate_statistics.R -exec dirname {} \\;",intern=TRUE))

library(DECM)
stats <- NULL
#data("statistics.cmip.era.tas.1981-2010")
load("statistics.cmip.era.tas.1981-2010.rda")
stats$tas$present <- store
load("statistics.cmip.era.pr.1981-2010.rda")
stats$pr$present <- store

load("metaextract.rda")

statistics <- c("spatial.sd","mean","corr","rms","e")
gcms <- names(stats$tas$present)
gcms <- gcms[grep("gcm",gcms)]
regions <- names(stats$tas$present[[gcms[1]]])
regions <- regions[!regions %in% statistics]
r <- regions[1]
cmpi <- sapply(gcms,function(x) stats$tas$present[[x]][[r]][["e"]])
corr <- sapply(gcms,function(x) stats$tas$present[[x]][[r]][["corr"]])
spsd <- sapply(gcms,function(x) stats$tas$present[[x]][[r]][["spatial.sd"]])



x <- cmpi#spsd["ann",]
y <- corr["ann",]
z <-  cmpi
vstep <- seq(-1,1,0.1)
cstep <- colscal(n=length(vstep)-1, col="burd")
i <- sapply(z,function(z.i) which(z.i>=vstep[1:(length(vstep)-1)] & 
                                  z.i<vstep[2:(length(vstep))]))
col.z <- cstep[i]
col.z <- adjustcolor("blue",alpha=0.5)
dev.new()
plot(x,y,col=col.z,pch=20,cex=2,lwd=2,
     xlab="Spatial standard deviation",
     ylab="Spatial correlation")

if (new) dev.new()
cols <- 1:length(gcms)
rows <- 1:length(gcms)

vstep <- seq(-1,1,0.1)
cticks <- vstep[2:length(vstep)]-dv/2
cmin <- rgb(239,138,98,max=255) # blue
cmid <- rgb(247,247,247,max=255) # white
cmax <- rgb(103,169,207,max=255) # red
rgb.palette <- colorRampPalette(c(cmax,cmid,cmin),space="rgb")
cstep <- rgb.palette(n=length(vstep)-1)
cstep <- colscal(n=length(vstep)-1, col="burd")

image(cols,rows,t(trends),breaks=vstep,col=cstep,
      xlab='start year',ylab='length of period (years)',
      main=paste(c(varlabel," trend (",unitlabel,"/decade)"),collapse=""))