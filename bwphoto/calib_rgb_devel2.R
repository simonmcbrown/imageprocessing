# source("calib_rgb_devel2.R")

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

# function to scan-rgb to VDens
f.sc.vd  <- smooth.spline(cal.sc.mrgb,VDens20,spar=0.4)

# convert scanned printer RGB to vd
pr.vd    <- predict(f.sc.vd,pnt.sc.mrgb)$y

# funcion to convert image-rgb to VD the printer would produce
im.mrgb51 <- rev(c(0,seq(from=6,to=51,by=5),seq(from=57,to=102,by=5),seq(from=108,to=153,by=5),seq(from=159,to=199,by=5),204,seq(from=210,to=255,by=5)))
f.irgb.vd <- smooth.spline(im.mrgb51,pr.vd,spar=0.4) 

# funcion to convert VD to image-rgb 
f.vd.irgb <- smooth.spline(pr.vd,im.mrgb51,spar=0.4) 

# use equally spaced VD to predict required image rgb values reduced to the range the printer can produce
# maximum VDens the printer can print
pnt.maxVD <- max(pr.vd)
#pnt.maxVD <- 1.47
# minimum VDens the printer can print
pnt.minVD <- min(pr.vd)
VDens256.comp <- seq(from=pnt.minVD, to=pnt.maxVD, length=256)
i2.mrgb <- predict(f.vd.irgb,VDens256.comp)$y

# function to convert image.mrgb to corrected image.mrgb
f.i.i2 <- smooth.spline(255:0,i2.mrgb)

#plots
plot(VDens20,cal.sc.mrgb,ylim=c(0,260),xlab='VDens',ylab='Mean RGB')
#points(pnt.sc.mrgb.vd,pnt.sc.mrgb,col=2)
points(pr.vd,im.mrgb51,col=3)
points(VDens256.comp,i2.mrgb,col=4)


# ok pretend we have an image
im.in     <- 255:0
im.in.tr  <- predict(f.i.i2,im.in)$y
x11()
plot(im.in,im.in.tr)
abline(0,1)

