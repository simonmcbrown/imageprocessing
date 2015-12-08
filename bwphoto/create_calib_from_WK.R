# source("create_calib_from_WK.R")

library(png)
library(fields)
library(caTools)

sparq13  <- 0.5
spar1    <- c(0.6) #  spar for WK spline
stcal <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/inkrightMt_WMMBBK2_calib.png'
stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/ScanWork_WK.print.crop.png'

stout <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/ScanWork_WK_calibration.RSave'

#####################################################################################
# step 1 read q13 calibration data and make spline to convert scan_rgb to L
#####################################################################################
q13.vd   <- seq(from=0.05,to=1.95,by=0.1)  #not used but here for reference  density of 0 == white
q13.L    <- (rev(c(95.63,87.39,79.75,72.68,66.12,60.06,54.55,49.24,44.24,39.95,35.82,31.99,28.45,25.16,22.12,19.31,16.70,14.28,12.04,9.97)))/100.0 # L of 1 == white
COLPRINT <- TRUE  # are the images ordered as a column or row?
dx       <- 12  # size of window for sd calc for the calibration image 
CALPLOT  <- TRUE
 # ps1 <- c('A',paste(1:6),'M','8','9',letters[1:11])
 # plot(q13.vd,rev(q13.L),pch=ps1)
 # abline(h=0.5)

### get calibration numbers
imcal  <- readPNG(stcal)
mimcal <- imcal[,,1]
for (y in 1:dim(mimcal)[2]) mimcal[,y] <- (imcal[,y,1]+imcal[,y,2]+imcal[,y,3])/3.0
  #plot(1:2, type='n')
  #rasterImage(mimcal, 1.0, 1.0, 2.0, 2.0)

mimcal <- mimcal*255
if (COLPRINT) mimcal <- t(mimcal)

# determine threshold in sd that identifies the 19 different calibration patches
mmimcal <- mimcal[1,]
#for (y in 1:length(mmimcal)) mmimcal[y] <- mean(mimcal[,y]) # average accross the column. Use st_thresh = 0.5
# try median as dust may skew distribution -> not necessarily better
for (y in 1:length(mmimcal)) mmimcal[y] <- median(mimcal[,y]) # average accross the column. Use st_thresh = 0.8
  #plot(mmimcal)

sdmmimcal <- mmimcal
for (y in (1+dx) : (length(sdmmimcal)-dx)) sdmmimcal[y] <- sd(mmimcal[(y-dx):(y+dx)])

if(CALPLOT) {
    #x11()
    sd_thresh <- 1.4 #0.8  ###  this number will need tuning 
    up.2()
    plot(sdmmimcal,ylim=c(0,2)) ### plots the sd of sliding window to find breaks between patches
    abline(h=sd_thresh)
    plot(mmimcal)
    ix <- which(sdmmimcal>sd_thresh) ### this number will need to be tuned with each scan, where SD too big set to NA
    mmimcal[ix] <- NA
    points(mmimcal,cex=.3,col=2)

    x11()
    up.2by2()
    par(mfcol=c(4,5))
    cal.sc.mrgb <- NULL
    i       <- 1
    tmp1    <- NULL
    for (x in dx:(length(mmimcal))) {
      if (is.finite(mmimcal[x])) tmp1 <- c(tmp1,mmimcal[x])
      else {
        if (length(tmp1)>30){  ### this number will need to be tuned with each scan
          plot(tmp1,main=mean(tmp1,na.rm=T))
          #abline(h=median(tmp1,na.rm=T),col=2)
          #cat(i,median(tmp1,na.rm=T),cr)
          #cal.sc.mrgb[i] <- median(tmp1,na.rm=T)
          abline(h=mean(tmp1,na.rm=T),col=2)
          #cat(i,mean(tmp1,na.rm=T),cr)
          cal.sc.mrgb[i] <- mean(tmp1,na.rm=T)
          #readline("continue?")
          i          <- i+1
          tmp1       <- NULL
        } else tmp1  <- NULL
      }
    }
    
    x11()
    up.1()
    plot(q13.L,cal.sc.mrgb,main='Q13 Calibration data',ylab='Scanned value',xlab='dark <-     Lab value    -> light',xlim=c(0,1),ylim=c(0,260))
}

### this seems to work for the q13 calibration target
cat('Mean RGB values for Calibration target',cr)
for (l in 0:19) cat(l,round(cal.sc.mrgb[l+1],2),cr)

# function to convert scan measured rgb values to real Lab values
# use the known L values of the q13 card to calibrate the scanner values
f.sc.L  <- smooth.spline(cal.sc.mrgb,q13.L,spar=sparq13) # fn(input_scan_val) -> Lab
  # plot(f.sc.L)

readline("Continue to WK?")

    

#####################################################################################
# step 2 read WK curve and make spline  to convert L to image_k
#####################################################################################

img.rgba  <- readPNG(stimg)

if(is.finite(dim(img.rgba)[3])) {
    if(dim(img.rgba)[3] == 3) {
        img2 <- apply(img.rgba[,,1:3],1:2,sum)
        img2 <- img2/3
    } else if(dim(img.rgba)[3] == 2) {
        img2 <- img.rgba[,,1]
    } else {
        cat("Error in dimensions of image",cr)
        stop()
    }
} else img2 <- img.rgba
  # plot(1:2, type='n')
  # rasterImage(img2, 1.0, 1.0, 2.0, 2.0)

### extract the WK curve
sy0 <- 255*apply(img2,1,sum)/dim(img2)[2]  # mean along gradients  scaled back to 0:255
sx  <- 255*apply(img2,2,sum)/dim(img2)[1]  # mean across gradients scaled back to 0:255

# this does not seem to help
sx.norm <- sx/mean(sx)
for(i in 1:(dim(img2)[2]) ) img2[,i] <- img2[,i]/sx.norm[i]

# fit weak spline to deal with noise
#sy0.sp <- smooth.spline(cal.sc.mrgb,q13.L,spar=sparq13)
#sy0.sm <- filter(sy0,(rep(1,7)/7.0))
#sy0.sm <- smooth(sy0, kind = "3")

k1     <- kernel("daniell", c(3, 2))
sy0.sm <- kernapply(sy0, k1)
#plot(sy0)
#lines(sy0.sm,col=2)

# convert scanned printed WK 255 values to real Lab vlues
sy.L  <- rev(predict(f.sc.L,sy0.sm)$y)

# do by hand as so much easyier
### will need to repeat this, by eye, for each new scan of the WK print
up.1()
i1 <- 1:300
plot(i1,sy.L[i1],ty='b',pch=3,cex=.3)
grid()
ig0b <- 30
ig1a <- 80
points(ig0b:ig1a,sy.L[ig0b:ig1a],col=2,cex=.8)
readline("Continue?")

i2 <- (length(sy.L)-300):length(sy.L)
plot(i2,sy.L[i2],ty='b',pch=3,cex=.3)
grid()
ig1b <- length(sy.L)-50
ig2a <- length(sy.L)
points(ig1b:ig2a,sy.L[ig1b:ig2a],col=2,cex=.8)
readline("Continue?")

gr0 <- list()
gr0[[1]] <- sy.L[ig1a:ig1b]  # K -> W
plot(gr0[[1]],ylim=c(0,1),cex=.3)
readline("Continue?")


### So we have removed the biases of the scanner by converting the scanned values into Lab
### From the printed data we can calculate what a specific input image value produces in terms of Lab.  
### so fit smooth.spine to WK
fn.dnin.prL <- NULL
for(i in 1:1) fn.dnin.prL[[i]] <- smooth.spline(seq(from=0, to=255, length=length(gr0[[i]])), gr0[[i]],spar=spar1[i])
plot(fn.dnin.prL[[1]],ylim=c(0,1),cex=.3,xlab='Image RGB', ylab='Printed L value')

ans0 <- readline("Good enough? (y/n) ")
if(ans0=='n') exit()

# now need to reverse this so we generate a function to convert specific value of 
# printed L to the required value in the image-rgb 
fn.prL.dn <- list()
for (i in 1:1) fn.prL.dn[[i]] <- smooth.spline(gr0[[i]],seq(from=0, to=255, length=length(gr0[[i]])),spar=spar1[i]) # fn(Lab) -> image_value

# create 256 linear steps between the max and min rpinter L values
pr.L.limits <- range(unlist(gr0))  # need to be careful here, the scanner did not record L=100
pr.L.256    <- seq(from=pr.L.limits[1], to=pr.L.limits[2], length.out=256)
# calculate the required image DN values that will produce these prL values
dn.corrected <- array(NA,dim=c(256,1))   # 1 ink only that represent the 256 Lab values
for (i in 1:1) dn.corrected[,i] <- predict(fn.prL.dn[[i]],pr.L.256)$y
for (i in 1:1) dn.corrected[(dn.corrected[,i]>255),i] <- 255
for (i in 1:1) dn.corrected[(dn.corrected[,i]<0),  i] <- 0
# dn.corrected[1,1] == 39 and not 0 because the spline is totally flat below 39 so cannot go below # reset by had for peace of mind
dn.corrected[1,1] = 0
dn.corrected[256,1] = 255
dn.corrected.i<- round(dn.corrected) # convert to integers  # MIGHT BE BETTER TO DO THIS AT THE LOOKUP TABLE STAGE

up.2()
    plot(pr.L.256,dn.corrected[,1],col=1,pch=3,xlim=c(0,1),ylim=c(0,256),xlab='Requested L',ylab='Required DN')
  grid()
    plot(0:255,dn.corrected.i[,1],col=1,pch=3,xlim=c(0,256),ylim=c(0,256),xlab='DN in',ylab='DN out')
  grid()
dn.corrected.i <- drop(dn.corrected.i)
for (i in 1:256) cat(i,'\tL',pr.L.256[i],'\tKW',dn.corrected.i[i],cr)

dn_in_to_out <- dn.corrected.i

#####################################################################################
# save splines and DN_in to DN_out array
#####################################################################################
save(file=stout, fn.dnin.prL, f.sc.L, dn_in_to_out)
