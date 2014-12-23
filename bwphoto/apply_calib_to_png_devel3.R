# source("apply_calib_to_png_devel3.R")
library(png)

#stimg <- 'sjb_ruin_51step_target.png'
stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/sjb_51_grey_step_wide_rgb_aRGB.png'
new_postfix <- '-g18c-20-mk2-a_print.png'
TESTING  <- FALSE


spar1    <- 0.38

q13.vd      <- seq(from=0.05,to=1.95,by=0.1)
q13.mrgb     <- seq(from=255,to=0,length=20)

# read data
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/in_read_scan_51wide_vista_nocol.txt'
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scan_51_epMt_aRGB_g18_con-20.txt'      # think this is from hand selecting w gimp
stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scan_51_epMt_aRGB_g18_con-20_mk2.txt'  # think this is from get_calib_from_scan.R

### HAVE A PROBLEM WITH THE G18 AS AS THE TRANSFORMED IMAGE DOES NOT HAVE PRECISION IN THE DARKER LEVELS (MANY SAME VALUES) ALSO 255 GETS TRANSFORMED TO 0
coln <- c('r.Cal.Measured','g.Cal.Measured','b.Cal.Measured','r.Print','g.Print','b.Print')
t1   <- read.table(stin,header=F,skip=0,,na.string="na",colClasses='numeric',col.name=coln,as.is=T)

#cal.sc.rgb <- cbind(10*(2:4),20*(2:4),30*2:4)
cal.sc.rgb <- na.omit(cbind(t1$r.Cal.Measured,t1$g.Cal.Measured,t1$b.Cal.Measured))
pr.sc.rgb <- na.omit(cbind(t1$r.Print,t1$g.Print,t1$b.Print))

cal.sc.mrgb <- apply(cal.sc.rgb,1,mean)
pr.sc.mrgb <- apply(pr.sc.rgb,1,mean)

# function to scan-rgb to VDens
f.sc.vd  <- smooth.spline(cal.sc.mrgb,q13.vd,spar=spar1)

# convert scanned printer RGB to vd
pr.vd51    <- predict(f.sc.vd,pr.sc.mrgb)$y

# funcion to convert VD to image-rgb 
im.mrgb51 <- rev(c(0,seq(from=6,to=51,by=5),seq(from=57,to=102,by=5),seq(from=108,to=153,by=5),seq(from=159,to=199,by=5),204,seq(from=210,to=255,by=5)))
f.vd.irgb <- smooth.spline(pr.vd51,im.mrgb51,spar=spar1) 

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

# fn to go from im.in to im.corr
im.in256       <- 255:0
f.rgbin.rgbout <- smooth.spline(im.in256,i.mrgb.corr,spar=spar1)


##############################################################
### apply to "image"
##############################################################

if(TESTING) {
  img.rgba  <-array(0,dim=c(256,5,3))
  img.rgba[,,1]<-c(0:255)
  img.rgba[,,2]<-c(0:255)
  img.rgba[,,3]<-c(0:255)
} else {
  # readPNG(source, native = FALSE, info = FALSE)
  img.rgba  <- readPNG(stimg)
  # library(fields)
  # image.plot(img.rgba[,,1])
  #or   plot(1:2, type='n')
  #     rasterImage(img.rgba, 1.0, 1.0, 2.0, 1.1)
  if(dim(img.rgba)[3] < 3) {
    img.rgba0 <- img.rgba
    img.rgba  <- array(0,dim=c(dim(img.rgba)[1],dim(img.rgba)[2],3))
    img.rgba[,,1] <- img.rgba0[,,1]
    img.rgba[,,2] <- img.rgba0[,,1]
    img.rgba[,,3] <- img.rgba0[,,1]
  }
}

#img.mrgb      <- apply(img.rgba[,,1:3],c(1,2),mean)  # this is the slow bit
img.mrgb <- img.rgba[,,1]
for (y in 1:dim(img.mrgb)[2]) img.mrgb[,y] <- (img.rgba[,y,1]+img.rgba[,y,2]+img.rgba[,y,3])/3.0

img.mrgb      <- 255*img.mrgb
img.mrgb.corr <- img.mrgb
for (y in 1:dim(img.mrgb)[2]) img.mrgb.corr[,y] <- predict(f.rgbin.rgbout,img.mrgb[,y])$y

if(!TESTING) {

# for output image
out.rgba <- img.rgba
out.rgba[,,1] <- img.mrgb.corr/255
out.rgba[,,2] <- img.mrgb.corr/255
out.rgba[,,3] <- img.mrgb.corr/255

stout <- sub('.png',new_postfix,stimg)
writePNG(out.rgba, target=stout)
}