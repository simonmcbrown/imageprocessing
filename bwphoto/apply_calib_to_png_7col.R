# source("apply_calib_to_png_7col.R")

# 7 column print. imput file should be 
# col 1    calib data 19 values
# col 2:8  print data 51 values each column

library(png)

#stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/images/sjb_ruin_51step_target.png'
stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/images/sjb_51_grey_step_wide_rgb_aRGB.png'
new_postfix <- '-linux_epMt_aRGB_g22_c7printRGB.png'

ncalib   <- 20
nprint   <- 51
TESTING  <- FALSE
col1     <- c('brown','black','blue','cyan','red','magenta','yellow')
spar1    <- c(0.5,     0.6,    0.38,  0.38,  0.38, 0.5,     1.1)

q13.vd    <- rev(seq(from=0.05,to=1.95,by=0.1))
q13.mrgb  <- rev(seq(from=255,to=1,length=20))#q

# read data
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/in_read_scan_51wide_vista_nocol.txt'
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scan_51_epMt_aRGB_g18_con-20.txt'      # think this is from hand selecting w gimp
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scan_51_epMt_aRGB_g18_con-20_mk2.txt'  # think this is from get_calib_from_scan.R
#stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/Scan7520_linux_w51-1col_epMt_aRGB_g22.txt'
stin   <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/code/calib_MP600_linux_w51-7col_epMt_aRGB_g22.txt'

col1 <- c('brown','black','blue','cyan','red','magenta','yellow')
coln <- c('cal','BCx','B','CMx','C','MYx','M','Y')
t1   <- read.table(stin,header=F,skip=0,na.string="na",colClasses='numeric',col.name=coln,as.is=T,strip.white =T )

# assign values to variables
cal.sc.mrgb <- t1$cal[1:ncalib]
pr.sc.mrgb  <- array(NA,dim=c(7,51))
for (c in 1:7) pr.sc.mrgb[c,] <- t1[[c+1]]
#plot( pr.sc.mrgb[1,],ylim=c(0,260),col=col1[1])
#for (c in 1:7) points(pr.sc.mrgb[c,],col=col1[c])
### fix wierd red values
pr.sc.mrgb[5,40:51] <- pr.sc.mrgb[5,39]

# function to convert scan measured rgb values to real VDens values
f.sc.vd  <- smooth.spline(cal.sc.mrgb,q13.vd,spar=spar1[1])

# convert scanned printer RGB values to real Vdens vlues
pr.vd51    <- array(NA,dim=c(7,51))
for (c in 1:7) pr.vd51[c,]  <- predict(f.sc.vd,pr.sc.mrgb[c,])$y
#plot( pr.vd51[1,],ylim=range(pr.vd51),col=col1[1])
#for (c in 1:7) points(pr.vd51[c,],col=col1[c])

### We now know what a specific RGB value produces in terms of VDens.  
### Now need to produce sequence of RGB values that produce a smooth linear progression of Vdens
### each of the 7 columns has a different sequence of [r,g,b]
### however im.mrgb51 is a dummy variable which we map onto [rgb] values with i1RGB

# funcion to convert VD to image-rgb 
#Zim.mrgb51 <- rev(c(0,seq(from=6,to=51,by=5),seq(from=57,to=102,by=5),seq(from=108,to=153,by=5),seq(from=159,to=199,by=5),204,seq(from=210,to=255,by=5)))
#im.mrgb51 <- c(0,seq(from=6,to=51,by=5),seq(from=57,to=102,by=5),seq(from=108,to=153,by=5),seq(from=159,to=199,by=5),204,seq(from=210,to=255,by=5))
im.mrgb51 <- round(seq(from=1, to=255, length.out=51)) # black to white
ivdmax    <- 51 #49 ### array index. this is specific to each scan - needs to be set by eye
f.vd.irgb <- list()
for (l in 1:7) f.vd.irgb[[l]] <- smooth.spline(pr.vd51[l,1:ivdmax],im.mrgb51[1:ivdmax],spar=spar1[l]) 
#plot(f.vd.irgb[[1]],xlim=c(0,1.5),ty='n')
#for (l in 1:7) lines(f.vd.irgb[[l]],col=l)

# use equally spaced VD to predict required image rgb values reduced to the range the printer can produce
# maximum VDens the printer can print
pr.maxVD <- max(pr.vd51)
#pr.maxVD <- 1.47
# minimum VDens the printer can print
pr.minVD   <- min(pr.vd51)
#vd255.comp  <- seq(from=pr.maxVD, to=pr.minVD, length=255) # black to white
vd255.comp  <- seq(from=pr.minVD, to=pr.maxVD, length=255) # white to black  
i.mrgb.corr <- array(NA,dim=c(7,255))
for (l in 1:7) i.mrgb.corr[l,] <- predict(f.vd.irgb[[l]],vd255.comp)$y
ix <- which(i.mrgb.corr < 0)
i.mrgb.corr[ix] <- 0
ix <- which(i.mrgb.corr > 255)
i.mrgb.corr[ix] <- 255
plot( vd255.comp,i.mrgb.corr[1,],ylim=c(-10,260),col=col1[1],cex=.3)
for (l in 1:7) points(vd255.comp,i.mrgb.corr[l,],col=col1[l],cex=.3)

### need to specify which ink combinatin we want to use for each value of imput 0:255 value and combine with i.mrgb.corr to form a single fn
#255 vector of which ink combination to use white to black
#for (i in 1:256) cat(i, i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], 
for (i in 1:255) cat(i,formatC(round(c(vd255.comp[i],i.mrgb.corr[,i]),2), format='f', digits=2 ),sep='\t',cr)

iink <- rep(NA,255) # start all white

#mk1 sharp boundaries between ink choices gave some banding
iink[  1: 13] <- 7 # Y
iink[ 14: 26] <- 6 # M 
iink[ 27: 34] <-  5 # R 
iink[ 35: 94] <-  3 # Blue 
iink[ 95:255] <-  1 # Bk+Cx 

# mk2 tries to dither slighly at ink boundaries
iink[ 15]     <- 7 # Y
iink[ 12]     <- 6 # M 
iink[ 28]     <- 6 # M 
iink[ 25]     <- 5 # R 
iink[ 36]     <- 5 # R 
iink[ 33]     <- 3 # Blue 
iink[ 96]     <- 3 # Blue 
iink[ 93]     <- 1 # Bk+Cx 

# select the corrected mrgb for each possible pixel value
c1 <- i.mrgb.corr[,1]
for (i in 1:255) c1[i] <- i.mrgb.corr[iink[i],i]  
c1[is.na(c1)]     <- 1
c1                <- round(c1)
c1[which(c1<1)]   <- 1
c1[which(c1>255)] <- 255

# create 255 RGB vaues for each of the selected inks
# bit odd. gimp suggests byte range goes from 1 to 255 NOT 0:255 or 1:256
# from make_target_sepparated_colours_7columns.R
stepd      <- 255:1                 
stepu      <- 1:255                 
i7RGB  <- array(255,dim=c(255,3,7)) # [pixel_value, R_or_G_or_B, ink] 1:B+Cx  7:Y  as above  NB set all colours to white ie [255,255,255]
# wht -> yellow  
i7RGB[,1,7] <- 255      
i7RGB[,2,7] <- 255      
i7RGB[,3,7] <- stepd      
# wht -> magenta 
i7RGB[,1,6] <- 255      
i7RGB[,2,6] <- stepd      
i7RGB[,3,6] <- 255      
# yellow -> red 
i7RGB[,1,5] <- 255
i7RGB[,2,5] <- stepd      
i7RGB[,3,5] <- 1      
# wht -> cyan
i7RGB[,1,4] <- stepd      
i7RGB[,2,4] <- 255      
i7RGB[,3,4] <- 255      
# magenta -> blue 
i7RGB[,1,3] <- stepd
i7RGB[,2,3] <- 1
i7RGB[,3,3] <- 255
# black
i7RGB[,,2]  <- stepd  
# cyan -> black
i7RGB[,1,1]  <- 1      
i7RGB[,2,1]  <- stepd      
i7RGB[,3,1]  <- stepd 

l<-1; plot(i7RGB[,1,l],ty='l',col=2,ylim=c(0,260)); lines(i7RGB[,2,l],col=3); lines(i7RGB[,3,l],col=4)

# collapse into one 255 by RGB array by selecting the right 'ink'
i1RGB <- i7RGB[,,1]
ir255 <- 255:1
for (i in 1:255) i1RGB[i,] <- i7RGB[ir255[c1[i]],,iink[i]]  # for each inRGB select the right ink and the corrected RGB
 plot(vd255.comp,i1RGB[,1],ty='n',ylim=c(0,260))  # v useful plot
lines(vd255.comp,i1RGB[,1],col=2)
lines(vd255.comp,i1RGB[,2]+1,col=3)
lines(vd255.comp,i1RGB[,3]-1,col=4)

readline("Stop")

##############################################################
### apply to "image"
##############################################################

if(TESTING) {
  img.rgba  <-array(1,dim=c(255,5,3))
  img.rgba[,,1]<-c(1:255)
  img.rgba[,,2]<-c(1:255)
  img.rgba[,,3]<-c(1:255)
} else {
  # readPNG(source, native = FALSE, info = FALSE)
  img.rgba  <- readPNG(stimg)
  # library(fields)
  # image.plot(img.rgba[,,1])
  #or   plot(1:2, type='n')
  #     rasterImage(img.rgba, 1.0, 1.0, 2.0, 1.1)
}


if(dim(img.rgba)[3] < 3) { # monochrome
    img.mrgb <- img.rgba[,,1]
} else { # RGB
    #img.mrgb      <- apply(img.rgba[,,1:3],c(1,2),mean)  # this is the slow bit
    img.mrgb <- img.rgba[,,1]
    for (y in 1:dim(img.mrgb)[2]) img.mrgb[,y] <- (img.rgba[,y,1]+img.rgba[,y,2]+img.rgba[,y,3])/3.0
}
if (max(img.mrgb)<2) img.mrgb      <- 255*img.mrgb
img.mrgb[which(img.mrgb>255)] <- 255
img.mrgb[which(img.mrgb<1)]   <- 1

img.mrgb.corr <- array(NA,dim=c(dim(img.mrgb)[1:2],3))
# need to invert the inRGB as a low value is a dark value and in i1RGB this is the opposite
for (y in 1:dim(img.mrgb)[2]) for (x in 1:dim(img.mrgb)[1]) img.mrgb.corr[x,y,] <- i1RGB[ ir255[img.mrgb[x,y]] , ]

if(!TESTING) {
    # for output image
    img.mrgb.corr <- img.mrgb.corr/255
    
    #writePNG(img.mrgb.corr, target='testc7.png')
    stout <- sub('.png',new_postfix,stimg)
    writePNG(img.mrgb.corr, target=stout)
}