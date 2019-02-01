x <- getGCMs(select=1, varid="tas", experiment="rcp45")
cdo.command(c("-monmean","-selyear","-sellonlatbox"),
            c("","1990/2000","0,30,55,70"),
            x[[1]]$filename, "gcm.nc")

y <- getERA("tas", start=1990, end=2000)
cdo.command(c("-sellonlatbox"),
            c("0,30,55,70"),
            y$filename, "ref.nc")