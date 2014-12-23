# source("get_calib_from_scan.R")

library(png)

# get calibration RGB numbers
#stcal <- '/home/h03/hadsx/me/photo/BW_bits/scans/scan_51_epMt_aRGB_g18_con-20_just_calib.png'
#stpr <- '/home/h03/hadsx/me/photo/BW_bits/scans/scan_51_epMt_aRGB_g18_con-20_just_print.png'

stcal <- '/home/h03/hadsx/me/photo/BW_bits/scans/Scan7520_linux_w51-1col_epMt_aRGB_g22_calib2.png'
stpr  <- '/home/h03/hadsx/me/photo/BW_bits/scans/Scan7520_linux_w51-1col_epMt_aRGB_g22_print.png'

COLPRINT <- TRUE  # are the images ordered as a column or row?
dx  <- 12  # size of window for sd calc
dx2 <- 6  # size of window for sd calc

imcal  <- readPNG(stcal)
mimcal <- imcal[,,1]
for (y in 1:dim(mimcal)[2]) mimcal[,y] <- (imcal[,y,1]+imcal[,y,2]+imcal[,y,3])/3.0
mimcal <- mimcal*255
if (COLPRINT) mimcal <- t(mimcal)
mmimcal <- mimcal[1,]
for (y in 1:length(mmimcal)) mmimcal[y] <- mean(mimcal[,y])

sdmmimcal <- mmimcal
for (y in (1+dx) : (length(sdmmimcal)-dx)) sdmmimcal[y] <- sd(mmimcal[(y-dx):(y+dx)])

#x11()
up.2()
plot(sdmmimcal,ylim=c(0,2)) ### plots the sd of sliding window to find breaks between patches
abline(h=0.5)
plot(mmimcal)
ix <- which(sdmmimcal>0.7) ### this number will need to be tuned with each scan, where SD too big set to NA
mmimcal[ix] <- NA
points(mmimcal,cex=.3,col=2)

x11()
cal.val <- NULL
i       <- 1
tmp1    <- NULL
for (x in dx:length(mmimcal)) {
  if (is.finite(mmimcal[x])) tmp1 <- c(tmp1,mmimcal[x])
  else {
    if (length(tmp1)>70){  ### this number will need to be tuned with each scan
      plot(tmp1)
      abline(h=median(tmp1,na.rm=T),col=2)
      cat(i,median(tmp1,na.rm=T),cr)
      #readline("continue?")
      cal.val[i] <- median(tmp1,na.rm=T)
      i          <- i+1
      tmp1       <- NULL
    } else tmp1       <- NULL
  }
}
### this seems to work for the q13 calibration target
cat('Mean RGB values for Calibration target',cr)
for (l in 0:19) cat(l,round(cal.val[l+1],2),cr)

#####################################################################################
# get print numbers
readline("Stop")
impr  <- readPNG(stpr)
mimpr <- impr[,,1]
for (y in 1:dim(mimpr)[2]) mimpr[,y] <- (impr[,y,1]+impr[,y,2]+impr[,y,3])/3.0
mimpr <- mimpr*255
if (COLPRINT) mimpr <- t(mimpr)
mmimpr <- mimpr[1,]
for (y in 1:length(mmimpr)) mmimpr[y] <- mean(mimpr[,y])

sdmmimpr <- mmimpr
for (y in (1+dx2) : (length(sdmmimpr)-dx2)) sdmmimpr[y] <- sd(mmimpr[(y-dx2):(y+dx2)])
#x11()
up.2()
plot(sdmmimpr,ylim=c(0,3)) ### plots the sd of sliding window to find breaks between patches
abline(h=1.5)
plot(mmimpr)
ix <- which(sdmmimpr>1.5) ### this number will need to be tuned with each scan, where SD too big set to NA
mmimpr[ix] <- NA
points(mmimpr,cex=.3,col=2)

x11()
pr.val <- NULL
i       <- 1
tmp1    <- NULL
for (x in dx2:length(mmimpr)) {
  if (is.finite(mmimpr[x])) tmp1 <- c(tmp1,mmimpr[x])
  else {
    if (length(tmp1)>14){  ### this number will need to be tuned with each scan
      plot(tmp1)
      abline(h=median(tmp1,na.rm=T),col=2)
      cat(i,median(tmp1,na.rm=T),cr)
      #readline("continue?")
      pr.val[i] <- median(tmp1,na.rm=T)
      i          <- i+1
      tmp1       <- NULL
    } else tmp1       <- NULL
  }
}
cat('Mean RGB values for Printer target',cr)
for (l in 0:52) cat(l,round(pr.val[l+1],2),cr)

