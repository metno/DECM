## helpers.R
## Help functions for the shiny app "gcmeval"
library(plotrix)

regions <- function(type=c("srex","prudence"),region=NULL) {
  if(is.null(type) | length(type)>1) region <- NULL
  if(is.null(type) | "srex" %in% tolower(type)) {
    f <- "referenceRegions.shp"#find.file("referenceRegions.shp")
    x <- get.shapefile(f,with.path=TRUE)
    ivec <- 1:nrow(x)
    if(!is.null(region)) {
      if(is.numeric(region)) {
        ivec <- region
      } else if(region %in% x$LAB) {
        ivec <- sapply(region, function(y) which(y==x$LAB))
      } else if(region %in% x$NAME) {
        ivec <- sapply(region, function(y) which(y==x$NAME))
      } else {
        print(paste("Unknown region",region))
      }
    }
    y <- list(name=as.character(x$NAME[ivec]), 
              label=as.character(x$LAB[ivec]), 
              usage=as.character(x$USAGE[ivec]),
              type=rep("srex",length(ivec)),
              coords=lapply(ivec, function(i) t(coordinates(x@polygons[[i]]@Polygons[[1]]))))
  } else {
    y <- NULL
  }
  if(is.null(type) | "prudence" %in% tolower(type)) {
    f <- "RegionSpecifications.csv"#find.file("RegionSpecifications.csv")
    x <- read.table(f,sep=",")
    ivec <- 2:nrow(x)
    names <- as.character(x[2:nrow(x),1])
    labels <- as.character(x[2:nrow(x),2])
    if(!is.null(region)) {
      if(is.numeric(region)) {
        ivec <- region
      } else if(region %in% labels) {
        ivec <- sapply(region, function(y) which(y==labels)+1)
      } else if(region %in% names) {
        ivec <- sapply(region, function(y) which(y==names)+1)
      } else {
        print(paste("Unknown region",region))
      }
    }
    prudence <- list(name=as.character(x[ivec,1]),
                  label=as.character(x[ivec,2]),
                  usage=rep("land",length(ivec)),
                  type=rep("prudence",length(ivec)),
                  coords=lapply(ivec, function(i) 
                    t(matrix(sapply(c(4,5,5,4,4,6,6,7,7,6), 
                             function(j) factor2numeric(x[i,j])),
                             nrow=5,ncol=2))))
                  #west=as.numeric(x[2:nrow(x),4]),
                  #east=as.numeric(x[2:nrow(x),5]),
                  #south=as.numeric(x[2:nrow(x),6]),
                  #north=as.numeric(x[2:nrow(x),7]))
    if(is.null(y)) {
      y <- prudence 
    } else {
      y <- mapply(c, y, prudence, SIMPLIFY=FALSE)
    }
  }
  invisible(y)
}

#Taylor diagram
taylor.diagram_Rsd <- function (R, sd.r, sd.f, add = FALSE, col = "red", pch = 19, pos.cor = TRUE, 
                                xlab = "", ylab = "", main = "Taylor Diagram", show.gamma = TRUE, 
                                ngamma = 3, gamma.col = 8, sd.arcs = 0, ref.sd = FALSE, sd.method = "sample", 
                                grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9), pcex = 1, cex.axis = 1, 
                                normalize = FALSE, mar = c(5, 4, 6, 6), maxsdfac=1.5, ...) 
{
  grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 
                      1)
  
  if (normalize) {
    sd.f <- sd.f/sd.r
    sd.r <- 1
  }
  maxsd <- maxsdfac * max(sd.f, sd.r,na.rm=T)
  oldpar <- par("mar", "xpd", "xaxs", "yaxs")
  if (!add) {
    if (pos.cor) {
      if (nchar(ylab) == 0) 
        ylab = "Standard deviation"
      par(mar = mar)
      plot(0, xlim = c(0, maxsd), ylim = c(0, maxsd), xaxs = "i", 
           yaxs = "i", axes = FALSE, main = main, xlab = xlab, 
           ylab = ylab, type = "n", cex = cex.axis, ...)
      if (grad.corr.lines[1]) {
        for (gcl in grad.corr.lines) lines(c(0, maxsd * 
                                               gcl), c(0, maxsd * sqrt(1 - gcl^2)), lty = 3)
      }
      segments(c(0, 0), c(0, 0), c(0, maxsd), c(maxsd, 
                                                0))
      axis.ticks <- pretty(c(0, maxsd))
      axis.ticks <- axis.ticks[axis.ticks <= maxsd]
      axis(1, at = axis.ticks, cex.axis = cex.axis)
      axis(2, at = axis.ticks, cex.axis = cex.axis)
      if (sd.arcs[1]) {
        if (length(sd.arcs) == 1) 
          sd.arcs <- axis.ticks
        for (sdarc in sd.arcs) {
          xcurve <- cos(seq(0, pi/2, by = 0.03)) * sdarc
          ycurve <- sin(seq(0, pi/2, by = 0.03)) * sdarc
          lines(xcurve, ycurve, col = "blue", lty = 3)
        }
      }
      if (show.gamma[1]) {
        if (length(show.gamma) > 1) 
          gamma <- show.gamma
        else gamma <- pretty(c(0, maxsd), n = ngamma)[-1]
        if (gamma[length(gamma)] > maxsd) 
          gamma <- gamma[-length(gamma)]
        labelpos <- seq(45, 70, length.out = length(gamma))
        for (gindex in 1:length(gamma)) {
          xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] + 
            sd.r
          endcurve <- which(xcurve < 0)
          endcurve <- ifelse(length(endcurve), min(endcurve) - 
                               1, 105)
          ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
          maxcurve <- xcurve * xcurve + ycurve * ycurve
          startcurve <- which(maxcurve > maxsd * maxsd)
          startcurve <- ifelse(length(startcurve), max(startcurve) + 
                                 1, 0)
          lines(xcurve[startcurve:endcurve], ycurve[startcurve:endcurve], 
                col = gamma.col)
          if (xcurve[labelpos[gindex]] > 0) 
            boxed.labels(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]], 
                         gamma[gindex], border = FALSE)
        }
      }
      xcurve <- cos(seq(0, pi/2, by = 0.01)) * maxsd
      ycurve <- sin(seq(0, pi/2, by = 0.01)) * maxsd
      lines(xcurve, ycurve)
      bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
      medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
      smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
      segments(cos(bigtickangles) * maxsd, sin(bigtickangles) * 
                 maxsd, cos(bigtickangles) * 0.97 * maxsd, sin(bigtickangles) * 
                 0.97 * maxsd)
      par(xpd = TRUE)
      if (ref.sd) {
        xcurve <- cos(seq(0, pi/2, by = 0.01)) * sd.r
        ycurve <- sin(seq(0, pi/2, by = 0.01)) * sd.r
        lines(xcurve, ycurve)
      }
      points(sd.r, 0, cex = pcex)
      text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 
             1.05 * maxsd, sin(c(bigtickangles, acos(c(0.95, 
                                                       0.99)))) * 1.05 * maxsd, c(seq(0.1, 0.9, by = 0.1), 
                                                                                  0.95, 0.99))
      text(maxsd * 0.8, maxsd * 0.8, "Spatial correlation with reference", srt = 315)
      segments(cos(medtickangles) * maxsd, sin(medtickangles) * 
                 maxsd, cos(medtickangles) * 0.98 * maxsd, sin(medtickangles) * 
                 0.98 * maxsd)
      segments(cos(smltickangles) * maxsd, sin(smltickangles) * 
                 maxsd, cos(smltickangles) * 0.99 * maxsd, sin(smltickangles) * 
                 0.99 * maxsd)
    }
    else {
      if (add == FALSE) {
        maxray <- 1.5 * max(sd.f, sd.r)
        plot(c(-maxray, maxray), c(0, maxray), type = "n", 
             asp = 1, bty = "n", xaxt = "n", yaxt = "n", 
             xlab = xlab, ylab = ylab, main = main, cex = cex.axis)
        discrete <- seq(180, 0, by = -1)
        listepoints <- NULL
        for (i in discrete) {
          listepoints <- cbind(listepoints, maxray * 
                                 cos(i * pi/180), maxray * sin(i * pi/180))
        }
        listepoints <- matrix(listepoints, 2, length(listepoints)/2)
        listepoints <- t(listepoints)
        lines(listepoints[, 1], listepoints[, 2])
        lines(c(-maxray, maxray), c(0, 0))
        lines(c(0, 0), c(0, maxray))
        for (i in grad.corr.lines) {
          lines(c(0, maxray * i), c(0, maxray * sqrt(1 - 
                                                       i^2)), lty = 3)
          lines(c(0, -maxray * i), c(0, maxray * sqrt(1 - 
                                                        i^2)), lty = 3)
        }
        for (i in grad.corr.full) {
          text(1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                                                         i^2), i, cex = 0.6)
          text(-1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                                                          i^2), -i, cex = 0.6)
        }
        seq.sd <- seq.int(0, 2 * maxray, by = (maxray/10))[-1]
        for (i in seq.sd) {
          xcircle <- sd.r + (cos(discrete * pi/180) * 
                               i)
          ycircle <- sin(discrete * pi/180) * i
          for (j in 1:length(xcircle)) {
            if ((xcircle[j]^2 + ycircle[j]^2) < (maxray^2)) {
              points(xcircle[j], ycircle[j], col = "darkgreen", 
                     pch = ".")
              if (j == 10) 
                text(xcircle[j], ycircle[j], signif(i, 
                                                    2), cex = 0.5, col = "darkgreen")
            }
          }
        }
        seq.sd <- seq.int(0, maxray, length.out = 5)
        for (i in seq.sd) {
          xcircle <- (cos(discrete * pi/180) * i)
          ycircle <- sin(discrete * pi/180) * i
          if (i) 
            lines(xcircle, ycircle, lty = 3, col = "blue")
          text(min(xcircle), -0.03 * maxray, signif(i, 
                                                    2), cex = 0.5, col = "blue")
          text(max(xcircle), -0.03 * maxray, signif(i, 
                                                    2), cex = 0.5, col = "blue")
        }
        text(0, -0.08 * maxray, "Standard Deviation", 
             cex = 0.7, col = "blue")
        text(0, -0.12 * maxray, "Centered RMS Difference", 
             cex = 0.7, col = "darkgreen")
        points(sd.r, 0, pch = 22, bg = "darkgreen", cex = 1.1)
        text(0, 1.1 * maxray, "Correlation Coefficient", 
             cex = 0.7)
      }
      S <- (2 * (1 + R))/(sd.f + (1/sd.f))^2
    }
  }
  # points(sd.f * R, sd.f * sin(acos(R)), pch = pch, col = col, 
         # cex = pcex)
  text(sd.f * R, sd.f * sin(acos(R)), labels=as.character(pch), col = col, 
         cex = pcex)
  
  text(0.5, 0.1, "Centered RMS Difference", col = "lightgrey")
  invisible(oldpar)
}
