#! /usr/bin/env Rscript

# source("apply_calib_7col_dev.Rscript.R")

# 7 column print. imput file should be 
# col 1    calib data 19 values
# col 2:8  print data 51 values each column

library(png)
library(jpeg)

cargs <- commandArgs(TRUE)
ncargs <- length(cargs)
cat('ncargs ',ncargs,'\n')

DOPLOT <- FALSE

#stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/images/sjb_ruin_51step_target.png'
#stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/images/sjb_51_grey_step_wide_rgb_aRGB.png' ### TESTING
#stimg <- 'sjb_51_grey_step_wide_rgb_aRGB.png' ### TESTING
#stcal <- 'calib_MP600_linux_w51-7col_epMt_aRGB_g22.txt'### TESTING

if (ncargs == 2) {
    stimg <- cargs[1]  
    stcal <- cargs[2] 
    cat('Image: ',stimg,'\n')
    cat('Calib: ',stcal,'\n')
} else if (ncargs == 1) {
    stimg <- cargs[1]  
    stcal <- 'calib_MP600_linux_w51-7col_epMt_aRGB_g22.txt'
    cat('Image: ',stimg,'\n')
    cat('Calib: ',stcal,'\n')
} else {
    cat('ERROR: no image supplied \n')
    return(0)
}
  
new_postfix <- '-linux_epMt_aRGB_g22_c7printRGBb'
#new_postfix <- '-dev'

ncalib   <- 20
nprint   <- 51
TESTING  <- FALSE
col1     <- c('brown','black','blue','cyan','red','magenta','yellow3')
spar1    <- c(0.5,     0.6,    0.38,  0.38,  0.38, 0.5,     1.1)

q13.vd    <- rev(seq(from=0.05,to=1.95,by=0.1))
q13.mrgb  <- rev(seq(from=255,to=1,length=20))#q

# read data
coln <- c('cal','BCx','B','CMx','C','MYx','M','Y')
t1   <- read.table(stcal,header=F,skip=0,na.string="na",colClasses='numeric',col.name=coln,as.is=T,strip.white =T )
# format of the calibration file is 
# col 1 scanned monochrome value of the q13 chart (20 values)
# col 2 scanned monochrome value of the cyan_max to black (51 values)     == 18%max + 100%
# col 3 scanned monochrome value of the white to black (51 values)        == 100%
# col 4 scanned monochrome value of the magenta max to blue (51 values)   == 6%max + 18%
# col 5 scanned monochrome value of the white to cyan (51 values)         == 18%
# col 6 scanned monochrome value of the yellow_max to red (51 values)     == 2%max + 6% 
# col 7 scanned monochrome value of the white to magenta (51 values)      == 6% ink
# col 8 scanned monochrome value of the white to yello (51 values)        == 2% ink
txt1 <- c('ink: 18%x+100%','ink: 100%','ink: 6%x+18%','ink: 18%','ink: 2%x+6%','ink: 6%','ink: 2%')

if(DOPLOT) {
    plot(t1$Y,ylim=c(0,250),col='yellow3')
    points(t1$M,col='magenta')
    points(t1$MYx,col='red')
    points(t1$C,col='cyan')
    points(t1$CMx,col='blue')
    points(t1$B,col='black')
    points(t1$BCx,col='brown')
}

# assign values to variables
cal.sc.mrgb <- t1$cal[1:ncalib]
pr.sc.mrgb  <- array(NA,dim=c(7,51))
for (c in 1:7) pr.sc.mrgb[c,] <- t1[[c+1]]
#plot( pr.sc.mrgb[1,],ylim=c(0,260),col=col1[1])
#for (c in 1:7) points(pr.sc.mrgb[c,],col=col1[c])
### fix wierd red values
pr.sc.mrgb[5,40:51] <- pr.sc.mrgb[5,39]

# function to convert scan measured rgb values to real VDens values
# use the known visual density values of the q13 card to calibrate the scanner values
f.sc.vd  <- smooth.spline(cal.sc.mrgb,q13.vd,spar=spar1[1]) # fn(input_scan_val) -> Vdens

# convert scanned printer RGB values to real Vdens vlues
pr.vd51    <- array(NA,dim=c(7,51))
for (c in 1:7) pr.vd51[c,]  <- predict(f.sc.vd,pr.sc.mrgb[c,])$y
#plot( pr.vd51[1,],ylim=range(pr.vd51),col=col1[1])
#for (c in 1:7) points(pr.vd51[c,],col=col1[c])
### So we have removed the biases of the scanner by converting the scanned values into Vdens

### From the printed data we can calculate what a specific input image value produces in terms of VDens.  

### Now need to produce sequence of RGB values that produce a smooth linear progression of Vdens
### each of the 7 columns has a different sequence of [r,g,b]
### however im.mrgb51 is a dummy variable which we map onto [rgb] values with i1RGB

# funcion to convert printed VD to image-rgb 
#Zim.mrgb51 <- rev(c(0,seq(from=6,to=51,by=5),seq(from=57,to=102,by=5),seq(from=108,to=153,by=5),seq(from=159,to=199,by=5),204,seq(from=210,to=255,by=5)))
#im.mrgb51 <- c(0,seq(from=6,to=51,by=5),seq(from=57,to=102,by=5),seq(from=108,to=153,by=5),seq(from=159,to=199,by=5),204,seq(from=210,to=255,by=5))
im.mrgb51 <- round(seq(from=1, to=255, length.out=51)) # black to white in 51 steps as in the image that was printed
ivdmax    <- 51 #49 ### array index. this is specific to each scan - needs to be set by eye
f.vd.irgb <- list()
for (l in 1:7) f.vd.irgb[[l]] <- smooth.spline(pr.vd51[l,1:ivdmax],im.mrgb51[1:ivdmax],spar=spar1[l]) # fn(Vdens) -> image_value
#plot(f.vd.irgb[[1]],xlim=c(0,1.5),ty='n',xlab='Vdens printed',ylab='input image pixel value')
#for (l in 1:7) lines(f.vd.irgb[[l]],col=l)

# use equally spaced VD to predict required image rgb values reduced to the range the printer can produce
# maximum VDens the printer can print
pr.maxVD <- max(pr.vd51)
#pr.maxVD <- 1.47
# minimum VDens the printer can print
pr.minVD   <- min(pr.vd51)
#vd255.comp  <- seq(from=pr.maxVD, to=pr.minVD, length=255) # black to white
vd255.comp  <- seq(from=pr.minVD, to=pr.maxVD, length=255) # white to black in 255 equal steps of visual density
i.mrgb.corr <- array(NA,dim=c(7,255))   # 7 ink combinations that represent the 255 Vdens values
for (l in 1:7) i.mrgb.corr[l,] <- predict(f.vd.irgb[[l]],vd255.comp)$y
# reset predicted values that are out of range
ix              <- which(i.mrgb.corr < 0)
i.mrgb.corr[ix] <- 0
ix              <- which(i.mrgb.corr > 255)
i.mrgb.corr[ix] <- 255
if (DOPLOT) {
    plot( vd255.comp,i.mrgb.corr[1,],ylim=c(-10,260),col=col1[1],cex=.3,xlab='Required VDens',ylab='Image value needed to achieve requested VDens')
    for (l in 1:7) points(vd255.comp,i.mrgb.corr[l,],col=col1[l],cex=.7)
    legend(par()$xaxp[2],par()$yaxp[2],txt1, yjust=1, xjust=1,col=col1,pch=1) 
    readline("Continue?")
}

### need to specify which ink combinatin we want to use for each value of imput 0:255 value and combine with i.mrgb.corr to form a single fn
### for smoothness and neutral grey need to maximise use of the dilute inks first
#255 vector of which ink combination to use white to black
#for (i in 1:256) cat(i, i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], i.mrgb.corr[,i], 
cat(c('           ',sub('ink: ','  ',txt1),cr))
for (i in 1:255) cat(i,formatC(round(c(vd255.comp[i],i.mrgb.corr[,i]),2), format='f', digits=2 ),sep='\t',cr)

iink <- rep(NA,255) # start all white

# mk1 sharp boundaries between ink choices gave some banding
# mk2 tries to dither slighly at ink boundaries
#pre20150617 iink[  1: 13] <- 7 # Y
#pre20150617 iink[ 14: 26] <- 6 # M 
#pre20150617 iink[ 27: 34] <-  5 # R 
#pre20150617 iink[ 35: 94] <-  3 # Blue 
#pre20150617 iink[ 95:255] <-  1 # Bk+Cx 
#pre20150617 iink[ 15]     <- 7 # Y
#pre20150617 iink[ 12]     <- 6 # M 
#pre20150617 iink[ 28]     <- 6 # M 
#pre20150617 iink[ 25]     <- 5 # R 
#pre20150617 iink[ 36]     <- 5 # R 
#pre20150617 iink[ 33]     <- 3 # Blue 
#pre20150617 iink[ 96]     <- 3 # Blue 
#pre20150617 iink[ 93]     <- 1 # Bk+Cx 

iink[  1: 13] <- 7 # Y
iink[ 14: 29] <- 6 # M 
iink[ 30: 59] <-  5 # R 
iink[ 60: 109] <-  3 # Blue 
iink[ 110:255] <-  1 # Bk+Cx 
#do dither at ink boundaries
iink[ 12]  <- 6 # M 
iink[ 15]  <- 7 # Y
iink[ 28]  <- 5 # M 
iink[ 31]  <- 6 # M 
iink[ 58]  <- 3 # R 
iink[ 61]  <- 5 # R 
iink[ 108] <- 1 # Blue 
iink[ 111] <-  3 # Blue 

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

if (DOPLOT) {
    l<-1
    plot(i7RGB[,1,l],ty='l',col=2,ylim=c(0,260)); lines(i7RGB[,2,l],col=3); lines(i7RGB[,3,l],col=4)
    readline("Continue?")
}

# collapse into one 255 by RGB array by selecting the right 'ink'
i1RGB <- i7RGB[,,1]
ir255 <- 255:1
for (i in 1:255) i1RGB[i,] <- i7RGB[ir255[c1[i]],,iink[i]]  # for each inRGB select the right ink and the corrected RGB
if (DOPLOT) {  # this shows that we have dithered which inks at the boundary between ink choices
    plot(vd255.comp,i1RGB[,1],ty='n',ylim=c(0,260))  # v useful plot
    lines(vd255.comp,i1RGB[,1],col=2)
    lines(vd255.comp,i1RGB[,2]+1,col=3)
    lines(vd255.comp,i1RGB[,3]-1,col=4)
    readline("Continue?")
}
#readline("Stop")
cat('Completed pre calib \n')

##############################################################
### apply to "image"
##############################################################


    if(length(grep('jpg',stimg))>0) {
         img.rgba  <- readJPEG(stimg)
    } else if(length(grep('png',stimg))>0) {
         img.rgba  <- readPNG(stimg)
    } else {
    cat("Image file not recognised.  Use jpg or png \n")
    exit()
    }

if(length(dim(img.rgba)) == 3) { 
    img.mrgb <- img.rgba[,,1] 
    if(dim(img.rgba)[3] >= 3) for (y in 1:dim(img.mrgb)[2]) img.mrgb[,y] <- (img.rgba[,y,1]+img.rgba[,y,2]+img.rgba[,y,3])/3.0
} else if(length(dim(img.rgba)) == 2) { 
    img.mrgb <- img.rgba[,] 
} else {
    cat('Error in dims of image\n') 
    exit()
}


if (max(img.mrgb)<2) img.mrgb <- 255*img.mrgb
img.mrgb[which(img.mrgb>255)] <- 255
img.mrgb[which(img.mrgb<1)]   <- 1

img.mrgb.corr <- array(NA,dim=c(dim(img.mrgb)[1:2],3))
# need to invert the inRGB as a low value is a dark value and in i1RGB this is the opposite
# or reverse the i1RGB
i1RGB <- apply(i1RGB,2,rev)

# unreversed:: for (y in 1:dim(img.mrgb)[2]) for (x in 1:dim(img.mrgb)[1]) img.mrgb.corr[x,y,] <- i1RGB[ ir255[img.mrgb[x,y]] , ]
#   reversed:: for (y in 1:dim(img.mrgb)[2]) for (x in 1:dim(img.mrgb)[1]) img.mrgb.corr[x,y,] <- i1RGB[ img.mrgb[x,y] , ]
# but now use vapply for speed
# function to apply correction
"appcorr" <- function(inRGB, cor3)
    {
    outRGB <- cor3[inRGB,]
    }
# slowest but output good system.time(img.mrgb.corr <- apply(img.mrgb, c(1,2), appcorr, i1RGB)
# faster output needs reformatting system.time(img.mrgb.corr <- sapply(img.mrgb, appcorr,simplify=T, i1RGB))
# fastest so far but outptu needs reformatiing
#system.time(img.mrgb.corr <- vapply(img.mrgb, appcorr, FUN.VALUE=c(0.0,0.0,0.0), i1RGB))
img.mrgb.corr      <- vapply(img.mrgb, appcorr, FUN.VALUE=c(0.0,0.0,0.0), i1RGB)
img.mrgb.corr      <- t(img.mrgb.corr)
dim(img.mrgb.corr) <- c(dim(img.mrgb)[1:2],3)

if(!TESTING) {
    # for output image
    img.mrgb.corr <- img.mrgb.corr/255
    
    #writePNG(img.mrgb.corr, target='testc7.png')
    ### cant write out jpg as compression messes up the ink boundaries and introduces erronious grey 
         if(length(grep('jpg',stimg))>0) {
             stout <- sub('.jpg',paste(new_postfix,'.jpg',sep=''),stimg)
             writeJPEG(img.mrgb.corr, target=stout, quality=0.9)
         } else if(length(grep('png',stimg))>0) {
             stout <- sub('.png',paste(new_postfix,'.png',sep=''),stimg)
             writePNG(img.mrgb.corr, target=stout)
         }
}