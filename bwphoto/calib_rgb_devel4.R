# source("calib_rgb_devel4.R")

# calib 4 just alters the way data is input.  one single column. calib data first 19 values, print second 51 values

ncalib <- 20
nprint <- 51

TESTING  <- TRUE
spar1    <- 0.38 #0.38

q13.vd      <- seq(from=0.05,to=1.95,by=0.1)
q13.mrgb     <- seq(from=255,to=0,length=20)

# read data
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/in_read_scan_51wide_vista_nocol.txt'
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scan_51_epMt_aRGB_g18_con-20_mk2.txt'
stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/Scan7520_linux_w51-1col_epMt_aRGB_g22.txt'
coln <- c('id','value')
t1   <- read.table(stin,header=F,skip=0,,na.string="na",colClasses='numeric',col.name=coln,as.is=T)

# assign values to variables
cal.sc.mrgb <- t1$value[1:ncalib]
pr.sc.mrgb  <- rev(t1$value[(ncalib+1):(ncalib+nprint)])

# function to convert scan measured rgb values to real VDens values
f.sc.vd  <- smooth.spline(cal.sc.mrgb,q13.vd,spar=spar1)

# convert scanned printer RGB values to real Vdens vlues
pr.vd51    <- predict(f.sc.vd,pr.sc.mrgb)$y

### We now know what a specific RGB value produces in terms of VDens.  
### Now need to produce sequence of RGB values that produce a linear progression of Vdens

# funcion to convert VD to image-rgb 
im.mrgb51 <- rev(c(0,seq(from=6,to=51,by=5),seq(from=57,to=102,by=5),seq(from=108,to=153,by=5),seq(from=159,to=199,by=5),204,seq(from=210,to=255,by=5)))
### think the issue is here as spline is smoothing into a region the printer cannot produce. ? need to specify the limit by hand and then work out how to compress the bottom end
ivdmax    <- 51 #49 ### this is specific to each scan - needs to be set by eye
f.vd.irgb <- smooth.spline(pr.vd51[1:ivdmax],im.mrgb51[1:ivdmax],spar=spar1) 

# use equally spaced VD to predict required image rgb values reduced to the range the printer can produce
# maximum VDens the printer can print
pr.maxVD <- max(pr.vd51)
#pr.maxVD <- 1.47
# minimum VDens the printer can print
pr.minVD   <- min(pr.vd51)
vd256.comp  <- seq(from=pr.minVD, to=pr.maxVD, length=256) # white to black
i.mrgb.corr <- predict(f.vd.irgb,vd256.comp)$y

# predict the vd these corrected mrgb will produce
f.irgb.vd <- smooth.spline(im.mrgb51,pr.vd51,spar=spar1) 
pr.vd.corr <- predict(f.irgb.vd,i.mrgb.corr)$y

## plots
#up.2by2()
#plot(pr.vd51,im.mrgb51,xlim=c(0,2),ylim=c(0,260),main=spar1)
#points(q13.vd,q13.mrgb)
#points(vd256.comp,i.mrgb.corr,col=2,cex=.3)
#points(pr.vd.corr,255:0,col=3,cex=.3)
#
## ok pretend we have an image
#im.in     <- 255:0
#plot(im.in,i.mrgb.corr)
#abline(0,1)

## fn to go from im.in to im.corr
#f.rgbin.rgbout <- smooth.spline(im.in,i.mrgb.corr,spar=spar1)


#  other plots
#x11()
postscript('temp2.ps',horiz=F)
up.3()
plot(f.sc.vd$y, f.sc.vd$x ,xlab='Real Vis Density values',ylab='Scanned RGB-values',main='Converting scanned RGB value to VisDen',xlim=c(0,2),ylim=c(0,260))
points(pr.vd51, pr.sc.mrgb, cex=.3,col=2)
legend (1.0, 250,c('Scanned Q13 values','w51 scaned-rgb\nconverted to Vdens'),pch=1,col=1:2)

plot(pr.vd51,im.mrgb51,xlab='Vis Density',ylab='RGB-value', main='converting VDens to image-rgb',xlim=c(0,2),ylim=c(0,260))
points(f.vd.irgb,col=2,cex=.3)


plot(f.vd.irgb,xlab='Vis Density',ylab='RGB-value',main='Converting scanned RGB value to VisDen',xlim=c(0,2),ylim=c(0,260))
points(vd256.comp,i.mrgb.corr,col=2,cex=.3)
points(vd256.comp,255:0,col=3,cex=.3)
points(pr.vd.corr,255:0,col=4,cex=.3)
legend(1.0,250,c('Q13 actual-vd v. measured-rgb','printed rgb','perfect equal step vd','actual corrected equal step vd '),col=1:4,pch=1)
dev.off()