# source("apply_calib_to_png_devel4.R")

# calib 4 just alters the way data is input.  one single column. calib data first 19 values, print second 51 values

library(png)

stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/sjb_ruin_51step_target.png'
#stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/sjb_51_grey_step_wide_rgb_aRGB.png'
new_postfix <- '-linux_epMt_aRGB_g22_d4print.png'

ncalib   <- 20
nprint   <- 51
TESTING  <- FALSE
spar1    <- 0.38

q13.vd      <- seq(from=0.05,to=1.95,by=0.1)
q13.mrgb     <- seq(from=255,to=0,length=20)

# read data
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/in_read_scan_51wide_vista_nocol.txt'
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scan_51_epMt_aRGB_g18_con-20.txt'      # think this is from hand selecting w gimp
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scan_51_epMt_aRGB_g18_con-20_mk2.txt'  # think this is from get_calib_from_scan.R
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