# source("get_calib_from_scan_q13.R")

library(png)

# get calibration RGB numbers
#stcal <- '/home/h03/hadsx/me/photo/BW_bits/scans/scan_51_epMt_aRGB_g18_con-20_just_calib.png'
#stpr <- '/home/h03/hadsx/me/photo/BW_bits/scans/scan_51_epMt_aRGB_g18_con-20_just_print.png'

#stcal <- '/home/h03/hadsx/me/photo/BW_bits/scans/Scan7520_linux_w51-1col_epMt_aRGB_g22_calib2.png'
#stpr  <- '/home/h03/hadsx/me/photo/BW_bits/scan_fake_wedge_7column_51_cymkrbe.png'

stcal <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/inkrightMt_WMMBBK2_calib.png'
ncalib   <- 20
q13.vd    <- rev(seq(from=0.05,to=1.95,by=0.1))
q13.L     <- rev(c(95.63,87.39,79.75,72.68,66.12,60.06,54.55,49.24,44.24,39.95,35.82,31.99,28.45,25.16,22.12,19.31,16.70,14.28,12.04,9.97))


COLPRINT <- TRUE  # are the images ordered as a column or row?
dx  <- 12  # size of window for sd calc for the calibration image 

CALPLOT <- TRUE
DOCOL   <- TRUE

#####################################################################################
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
    cal.val <- NULL
    i       <- 1
    tmp1    <- NULL
    for (x in dx:(length(mmimcal))) {
      if (is.finite(mmimcal[x])) tmp1 <- c(tmp1,mmimcal[x])
      else {
        if (length(tmp1)>30){  ### this number will need to be tuned with each scan
          plot(tmp1,main=mean(tmp1,na.rm=T))
          #abline(h=median(tmp1,na.rm=T),col=2)
          #cat(i,median(tmp1,na.rm=T),cr)
          #cal.val[i] <- median(tmp1,na.rm=T)
          abline(h=mean(tmp1,na.rm=T),col=2)
          #cat(i,mean(tmp1,na.rm=T),cr)
          cal.val[i] <- mean(tmp1,na.rm=T)
          #readline("continue?")
          i          <- i+1
          tmp1       <- NULL
        } else tmp1  <- NULL
      }
    }
}
### this seems to work for the q13 calibration target
cat('Mean RGB values for Calibration target',cr)
for (l in 0:19) cat(l,round(cal.val[l+1],2),cr)
#readline("Continue?")

    plot(q13.L,cal.val,main='Q13 Calibration data',ylab='Scanned value',xlab='dark <-     Lab value    -> light',xlim=c(0,100),ylim=c(0,260))
