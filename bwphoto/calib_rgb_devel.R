# source("calib_rgb_devel.R")

TESTING  <- TRUE
##############################################################
### create correction functions for RGB
##############################################################
# L calibration values of the q-13
L.Cal.ref <- c(95.6,87.4,79.7,72.7,66.1,60.1,54.4,49.2,44.4,39.9,35.8,32.0,28.4,25.2,22.1,19.3,16.7,14.3,12.0,10.0)
# grey levels of the 21-step print
Grey.Print   <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
Grey.Print51 <- seq(from=0,to=100,by=2)
VDens20      <- seq(from=0.05,to=1.95,by=0.1)
VDens51      <- seq(from=0.05,to=1.95,length=51)
VDens101     <- seq(from=0.05,to=1.95,length=101)
VDens256     <- seq(from=0.05,to=1.95,length=256)
# read data file
stin   <- 'in_read_scan_51wide_vista_nocol.txt'
stout <- sub('in_','calfn_mk2_',stin)
stout <- sub('txt','RSave',stout)
SAVEPLOT <- T

coln <- c('r.Cal.Measured','g.Cal.Measured','b.Cal.Measured','r.Print','g.Print','b.Print')
t1   <- read.table(stin,header=F,skip=0,,na.string="na",colClasses='numeric',col.name=coln,as.is=T)

#cal.sc.rgb <- cbind(10*(2:4),20*(2:4),30*2:4)
cal.sc.rgb <- na.omit(cbind(t1$r.Cal.Measured,t1$g.Cal.Measured,t1$b.Cal.Measured))
pnt.sc.rgb <- na.omit(cbind(t1$r.Print,t1$g.Print,t1$b.Print))

cal.sc.mrgb <- apply(cal.sc.rgb,1,mean)
pnt.sc.mrgb <- apply(pnt.sc.rgb,1,mean)
#  plot(VDens20,cal.sc.rgb[,1],ylim=c(0,260),col=2)
#points(VDens20,cal.sc.rgb[,2],col=3)
#points(VDens20,cal.sc.rgb[,3],col=4)
#points(VDens20,cal.sc.mrgb,col=5,pch=4)

# transfer function for cal-target
f.cal.sc.DtoM  <- smooth.spline(VDens20,cal.sc.mrgb,spar=0.4)
f.cal.sc.MtoD  <- smooth.spline(cal.sc.mrgb,VDens20,spar=0.4)
cal.sc.mrgb101 <- predict(f.cal.sc.DtoM,VDens101)$y
cal.sc.mrgb256 <- predict(f.cal.sc.DtoM,VDens256)$y
plot(f.cal.sc.DtoM,ylim=c(0,260),xlab='VDens',ylab='Mean RGB')
lines(VDens101,cal.sc.mrgb101)
leg1 <- 'CalTarget-sc-mRGB'
col1 <- 1
pch1 <- 1


# convert scanned print mrgb to VDens
pnt.sc.mrgb.vd <- predict(f.cal.sc.MtoD,pnt.sc.mrgb)$y
#points(pnt.sc.mrgb.vd,pnt.sc.mrgb,col=2)

# plot the image mrgb wrt actual printed VDens
im.mrgb <- rev(c(0,seq(from=6,to=51,by=5),seq(from=57,to=102,by=5),seq(from=108,to=153,by=5),seq(from=159,to=199,by=5),204,seq(from=210,to=255,by=5)))

points(pnt.sc.mrgb.vd,im.mrgb,col=3)
leg1 <- c(leg1,'Printed VDens for im.mrgb')
col1 <- c(col1,3)
pch1 <- c(pch1,1)
legend(0,50,leg1,col=col1,pch=pch1)

# transfer function for scanned pnt.mrgb to pnt.VDens (ie green points above), input mrgb
f.pnt.sc.MtoD   <- smooth.spline(im.mrgb,pnt.sc.mrgb.vd,spar=0.4)
pnt.sc.VDens256 <- predict(f.pnt.sc.MtoD,255:0)$y
lines(pnt.sc.VDens256,255:0,col=3)   # green line
#f.pnt.sc.MtoDb   <- smooth.spline(pnt.sc.mrgb[(51-6):51],VDens51[(51-6):51],spar=0.1)
#pnt.sc.VDens256b <- predict(f.pnt.sc.MtoDb,pnt.sc.mrgb[(51-6)]:(min(pnt.sc.mrgb)-5))$y

# maximum VDens the printer can print
pnt.maxVD <- max(pnt.sc.mrgb.vd)
# corresponding mrgb
imaxVD <- which(pnt.sc.mrgb.vd > (pnt.maxVD-0.01))
im.mrgb.maxVD <- max(im.mrgb[imaxVD])

# minimum VDens the printer can print
pnt.minVD <- min(pnt.sc.mrgb.vd)
# corresponding mrgb
###NOT SURE ? NEED TO BRING DOWN THE TOP MRGB TO REFLECT THE PAPER IS NOT AS WHITE OR WILL THE PRINTER CORRECTION DEAL WITH THIS?
iminVD <- which(pnt.sc.mrgb.vd < (pnt.minVD+0.01))
im.mrgb.minVD <- max(im.mrgb[iminVD])

# now need to compress the image mrgb to span the available range the printer can cope with, then adjust for printer non linearities ie need black to green fn
mrgb.in  <- 0:255
mrgb.out <- seq(from=im.mrgb.maxVD, to=im.mrgb.minVD, length=256)
f.compress <- smooth.spline(mrgb.in,mrgb.out,spar=0.4)  # function to compress image rgb to those the printer can cope with

# function to correct for printer non-linearities eg black to green in mrgb space
f.nonlin.mrgb <- smooth.spline(255:0,cal.sc.mrgb256, spar=0.4)

# so feed in compressed im.mrgb and get out the mrgb you need to ask the printer to get the Vdens you wanted
im.mrgb.comp.corr <- predict(f.nonlin.mrgb,rev(mrgb.out))$y
points(pnt.sc.VDens256,im.mrgb.comp.corr,col=2,pch=1,cex=.3)

# ok pretend we have an image
im.in <- 255:0
im.in.com <- predict(f.compress,im.in)$y
im.in.tr  <- predict(f.nonlin.mrgb,predict(f.compress,im.in)$y)$y
# convert the transformed im.in into VDens using the original scanned printer response
im.in.tr.vd <- predict(f.pnt.sc.MtoD,im.in.tr)$y
points(im.in.tr.vd,im.in.tr,col=4)

