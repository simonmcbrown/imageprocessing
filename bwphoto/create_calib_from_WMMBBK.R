# source("create_calib_from_WMMBBK.R")

library(png)
library(fields)
library(caTools)

sparq13  <- 0.5
spar1    <- c(0.6,    0.6,  0.6) #  sepparate spar for WM, MB, BK
stout <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/inkrightMt_WMMBBK2_calibration.RSave'
stcal <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/inkrightMt_WMMBBK2_calib.png'
stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/inkrightMt_WMMBBK2_print.png'

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

readline("Continue to WMMBBK?")

    

#####################################################################################
# step 2 read WMMBBK curves and make splines  to convert L to image_rgb
#####################################################################################


#stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/ScanToSelf_20150703_inkriteMt_gradient_WMMBBK_crop.png'

img.rgba  <- readPNG(stimg)

if(!is.na(dim(img.rgba)[3]) ) {
    img2 <- apply(img.rgba[,,1:3],1:2,sum)
    img2 <- img2/3
} else img2 <- img.rgba
  # plot(1:2, type='n')
  # rasterImage(img2, 1.0, 1.0, 2.0, 2.0)

### extract the three curves WM, MB, BK
sy0 <- 255*apply(img2,1,sum)/dim(img2)[2]  # mean along gradients  scaled back to 0:255
sx  <- 255*apply(img2,2,sum)/dim(img2)[1]  # mean across gradients scaled back to 0:255
# convert scanned printed WMMBBK RGB values to real Lab vlues
sy.L  <- predict(f.sc.L,sy0)$y

# do by hand as so much easyier
### will need to repeat this, by eye, for each new scale of the WMMBBK print
up.1()
i1 <- 1:300
plot(i1,sy.L[i1],ty='b',pch=3,cex=.3)
grid()
ig1a <- 16
points(1:ig1a,sy.L[1:ig1a],col=2,cex=.8)
# might need to flatten the end of the black to remove the rise
sy.L[(ig1a):150] <- min(sy.L[151:160])
readline("Continue?")

i2 <- 860:960
plot(i2,sy.L[i2],ty='b',pch=3,cex=.3)
grid()
ig1b <- 896
ig2a <- 917
points(ig1b:ig2a,sy.L[ig1b:ig2a],col=2,cex=.8)
readline("Continue?")

i3 <- 1770:1840
plot(i3,sy.L[i3],ty='b',pch=3,cex=.3)
grid()
ig2b <- 1800
ig3a <- 1819
points(ig2b:ig3a,sy.L[ig2b:ig3a],col=2,cex=.8)
readline("Continue?")

i4 <- 2660:dim(img2)[1]
plot(i4,sy.L[i4],ty='b',pch=3,cex=.3)
grid()
ig3b <- 2700
ig4a <- dim(img2)[1]
points(ig3b:ig4a,sy.L[ig3b:ig4a],col=2,cex=.8)
readline("Continue?")

gr0 <- list()
gr0[[1]] <- sy.L[ig1a:ig1b]  # K -> B
gr0[[2]] <- sy.L[ig2a:ig2b]  # B -> M
gr0[[3]] <- sy.L[ig3a:ig3b]  # M -> W
 plot(gr0[[1]],ylim=c(0,1),cex=.3)
 points(gr0[[2]],col=2,cex=.3)
 points(gr0[[3]],col=3,cex=.3)
 points(rev(gr0[[2]]),ylim=c(0,1),col=2,cex=.3)
readline("Continue?")


### So we have removed the biases of the scanner by converting the scanned values into Lab
### From the printed data we can calculate what a specific input image value produces in terms of Lab.  
### so fit smooth.spine to each of WM, MB, BK
fn.dnin.prL <- NULL
for(i in 1:3) fn.dnin.prL[[i]] <- smooth.spline(seq(from=0, to=255, length=length(gr0[[i]])), gr0[[i]],spar=spar1[i])
plot(fn.dnin.prL[[1]],ylim=c(0,1),cex=.3,xlab='Image RGB', ylab='Printed L value')
#points(fn.dnin.prL[[2]],cex=.3, col=4)
points(fn.dnin.prL[[2]],cex=.3)
points(rev(fn.dnin.prL[[2]]$x),fn.dnin.prL[[2]]$y,cex=.3,col=4)
points(fn.dnin.prL[[3]],cex=.3, col='magenta')

cat('Diff between WM & MB',tail(fn.dnin.prL[[1]]$y,1),fn.dnin.prL[[2]]$y[1],tail(fn.dnin.prL[[1]]$y,1)-fn.dnin.prL[[2]]$y[1],cr)
cat('Diff between MB & BK',tail(fn.dnin.prL[[2]]$y,1),fn.dnin.prL[[3]]$y[1],tail(fn.dnin.prL[[2]]$y,1)-fn.dnin.prL[[3]]$y[1],cr)

ans0 <- readline("Good enough? (y/n) ")
if(ans0=='n') exit()

# now need to reverse this so we generate a function to convert specific value of 
# printed L to the required value in the image-rgb 
fn.prL.dn <- list()
for (i in 1:3) fn.prL.dn[[i]] <- smooth.spline(gr0[[i]],seq(from=0, to=255, length=length(gr0[[i]])),spar=spar1[i]) # fn(Lab) -> image_value

# create 256 linear steps between the max and min rpinter L values
pr.L.limits <- range(unlist(gr0))  # need to be careful here, the scanner did not record L=100
pr.L.256    <- seq(from=pr.L.limits[1], to=pr.L.limits[2], length.out=256)
# calculate the required image DN values that will produce these prL values
dn.corrected <- array(NA,dim=c(256,3))   # 3 ink combinations that represent the 256 Lab values
for (i in 1:3) dn.corrected[,i] <- predict(fn.prL.dn[[i]],pr.L.256)$y
for (i in 1:3) dn.corrected[(dn.corrected[,i]>255),i] <- 255
for (i in 1:3) dn.corrected[(dn.corrected[,i]<0),  i] <- 0
# dn.corrected[1,1] == 39 and not 0 because the spline is totally flat below 39 so cannot go below # reset by had for peace of mind
dn.corrected[1,1] = 0
dn.corrected[256,3] = 255
dn.corrected.i<- round(dn.corrected) # convert to integers  # MIGHT BE BETTER TO DO THIS AT THE LOOKUP TABLE STAGE

up.2()
    plot(pr.L.256,dn.corrected[,1],col=1,pch=3,xlim=c(0,1),ylim=c(0,256),xlab='Requested L',ylab='Required DN')
  points(pr.L.256,dn.corrected[,2],col=4,cex=.3)
  points(pr.L.256,dn.corrected[,3],col='magenta')
  grid()
    plot(0:255,dn.corrected.i[,1],col=1,pch=3,xlim=c(0,256),ylim=c(0,256),xlab='DN in',ylab='DN out')
  points(0:255,dn.corrected.i[,2],col=4,cex=.3)
  points(0:255,dn.corrected.i[,3],col='magenta')
  grid()
for (i in 1:256) cat(i,'\tL',pr.L.256[i],'\tKB',dn.corrected.i[i,1],'\tBM',dn.corrected.i[i,2],'\tMW',dn.corrected.i[i,3],cr)
iink <- 0:255
ib1  <- 106
ib2  <- 215
iink[1      :ib1] <- 1 # KB  # these are set by eye by ooking at the printed table above choosing the mid point of the crossover between inks
iink[(ib1+1):ib2] <- 2 # BM
iink[(ib2+1):256] <- 3 # MW
cat("Set the iink boundaries using this table.  Currently: ",ib1, ib2,cr)
readline("Continue?")

# create lookup table
KB     <- array(0,dim=c(256,3))
KB[,3] <- 0:255 # bring up the blue from black
BM     <- array(0,dim=c(256,3))
BM[,3] <- 255   # blue at max
BM[,1] <- 0:255 # bring up the red to give blue to magenta
MW     <- array(0,dim=c(256,3))
MW[,3] <- 255   # blue & red at max to give magenta max
MW[,1] <- 255   # blue & red at max to give magenta max
MW[,2] <- 0:255 # bring up the green to give magenta to white
KBBMMW <- array(NA,dim=c(256,3,3))
KBBMMW[,,1] <- KB
KBBMMW[,,2] <- BM
KBBMMW[,,3] <- MW
dn_in_to_out <- KBBMMW[,,1]
for(i in 1:256) dn_in_to_out[i,] <- KBBMMW[ 1+dn.corrected.i[ i, iink[i] ], , iink[i]] # set correct RGB value for each ink that is requested

#####################################################################################
# save splines and rgb_in to RGB_out array
#####################################################################################
save(file=stout, fn.dnin.prL, f.sc.L,dn_in_to_out)
