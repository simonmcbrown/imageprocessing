# source("get_calib_from_scan_7columns.R")

library(png)

# get calibration RGB numbers
#stcal <- '/home/h03/hadsx/me/photo/BW_bits/scans/scan_51_epMt_aRGB_g18_con-20_just_calib.png'
#stpr <- '/home/h03/hadsx/me/photo/BW_bits/scans/scan_51_epMt_aRGB_g18_con-20_just_print.png'

#stcal <- '/home/h03/hadsx/me/photo/BW_bits/scans/Scan7520_linux_w51-1col_epMt_aRGB_g22_calib2.png'
#stpr  <- '/home/h03/hadsx/me/photo/BW_bits/scan_fake_wedge_7column_51_cymkrbe.png'

stcal <- '/home/h03/hadsx/me/photo/BW_bits/scan_wedge_7column_51_cymkrbe_boundaries_calib.png'
stpr  <- '/home/h03/hadsx/me/photo/BW_bits/scan_wedge_7column_51_cymkrbe_boundaries_prnt.png'
stpr  <- '/home/h03/hadsx/me/photo/BW_bits/scan_wedge_7column_51_cymkrbe_boundaries_prnt2.png'

COLPRINT <- TRUE  # are the images ordered as a column or row?
dx  <- 12  # size of window for sd calc for the calibration image 
dx2 <- 12   # size of window for sd calc for the printer image columns
dx3 <- 6   # size of window for sd calc for the printer image columns

CALPLOT <- FALSE
DOCOL   <- FALSE

#####################################################################################
### get calibration numbers
imcal  <- readPNG(stcal)
mimcal <- imcal[,,1]
for (y in 1:dim(mimcal)[2]) mimcal[,y] <- (imcal[,y,1]+imcal[,y,2]+imcal[,y,3])/3.0
mimcal <- mimcal*255
if (COLPRINT) mimcal <- t(mimcal)
mmimcal <- mimcal[1,]
#for (y in 1:length(mmimcal)) mmimcal[y] <- mean(mimcal[,y]) # average accross the column. Use st_thresh = 0.5
# try median as dust may skew distribution -> not necessarily better
for (y in 1:length(mmimcal)) mmimcal[y] <- median(mimcal[,y]) # average accross the column. Use st_thresh = 0.8

sdmmimcal <- mmimcal
for (y in (1+dx) : (length(sdmmimcal)-dx)) sdmmimcal[y] <- sd(mmimcal[(y-dx):(y+dx)])

if(CALPLOT) {
    #x11()
    sd_thresh <- 0.8
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


#####################################################################################
### get print numbers
impr  <- readPNG(stpr)
# find average of the RGB values
mimpr <- impr[,,1]
for (y in 1:dim(mimpr)[2]) mimpr[,y] <- (impr[,y,1]+impr[,y,2]+impr[,y,3])/3.0
mimpr <- mimpr*255
if (COLPRINT) mimpr <- t(mimpr)

if(DOCOL) {
    ### find index of columns (NB gimp col,row swapped wrt R col,row
    sd_c   <- mimpr
    sd_c[,] <- NA
    for (x in 1 : dim(sd_c)[2]) {
        for (y in (1+dx2) : (dim(sd_c)[1]-dx2)) sd_c[y,x] <- sd(mimpr[(y-dx2):(y+dx2),x])
    }
    asd_c <- apply(sd_c,1,mean)

    x11()
    tolcol <- 4  # tolerance above which sd has to be below
    COLNOTDONE <- TRUE
    while (COLNOTDONE) {
        cat('Using tolcol: ',tolcol,cr)
        icol   <- list()
        ii     <- 1
        icol[[ii]] <- -1
        NEWCOL     <- FALSE
        for (i in 1:length(asd_c)) {
            if(!is.na(asd_c[i]) & asd_c[i] <= tolcol) {
                if (!NEWCOL) icol[[ii]] <- i else icol[[ii]] <- c(icol[[ii]],i)
                NEWCOL     <- TRUE
            } else {
                if(NEWCOL) {
                    ii     <- ii +1
                    NEWCOL <- FALSE
                }
            }
        }
        if (length(icol) != 7) cat('ERROR: 7 colums were not found',cr) 
        
        # trim the edges of the columns
        sdcol <- 0.5
        icol2 <- icol
        for (i in 1:length(icol)) {
            j         <- which( asd_c[icol[[i]]]<(mean(asd_c[icol[[i]]])+sdcol*sd(asd_c[icol[[i]]])) )
            while(diff(range(diff(j))) !=0) {
                #cat(i,'sdcol of ',sdcol,'failed to give a contiguous column',cr)
                #sdcol <- as.double(readline('Imput new sdcol scaling factor '))
                sdcol <- 1.1*sdcol
                j     <- which( asd_c[icol[[i]]]<(mean(asd_c[icol[[i]]])+sdcol*sd(asd_c[icol[[i]]])) )
            }
            icol2[[i]] <- icol[[i]][j]
            sdcol <- 0.5
        }

        up.1()
        plot(asd_c,main='Column SD diagnostic')
        for (l in 1:length(icol))  points( icol[[l]], asd_c[ icol[[l]]],col=2,cex=.3)
        for (l in 1:length(icol2)) points(icol2[[l]], asd_c[icol2[[l]]],col=3,cex=.3)
        abline(h=tolcol)
        # so icol2 is a list of valid row numbers for each column
        a <- as.double(readline("return to continue else enter new value for tolcol "))
        if (is.finite(a) & a>0) {
            COLNOTDONE <- TRUE
            tolcol <- a
        } else COLNOTDONE <- FALSE
    } # end while COLNOTDONE
}  # DOCOL  


### find index of rows (NB gimp col,row swapped wrt R col,row
sd_r   <- mimpr
sd_r[,] <- NA
for (x in 1 : dim(sd_r)[1]) {
    for (y in (1+dx3) : (dim(sd_r)[2]-dx3)) sd_r[x,y] <- sd(mimpr[x,(y-dx3):(y+dx3)])
}
asd_r <- apply(sd_r,2,mean)

x11()
tolrow <- 4  # tolerance above which sd has to be below
ROWNOTDONE <- TRUE
while (ROWNOTDONE) {
    cat('Using tolrow: ',tolrow,cr)
    irow   <- list()
    ii     <- 1
    irow[[ii]] <- -1
    NEWROW     <- FALSE
    for (i in 1:length(asd_r)) {
        if(!is.na(asd_r[i]) & asd_r[i] <= tolrow) {
            if (!NEWROW) irow[[ii]] <- i else irow[[ii]] <- c(irow[[ii]],i)
            NEWROW     <- TRUE
        } else {
            if(NEWROW) {
                ii     <- ii +1
                NEWROW <- FALSE
            }
        }
    }
    if (length(irow) != 53) cat('ERROR: 53 rows were not found',cr) else cat(length(irow),' rows were found.  Good.',cr)

    sdrow <- 0.5
    irow2 <- irow
    for (i in 1:length(irow)) {
        j         <- which( asd_r[irow[[i]]]<(mean(asd_r[irow[[i]]])+sdrow*sd(asd_r[irow[[i]]])) )
        while(diff(range(diff(j))) !=0) {
            #cat(i,'sdrow of ',sdrow,'failed to give a contiguous row',cr)
            #sdrow <- as.double(readline('Input new sdrow scaling factor '))
            sdrow <- 1.1*sdrow
            j     <- which( asd_r[irow[[i]]]<(mean(asd_r[irow[[i]]])+sdrow*sd(asd_r[irow[[i]]])) )
        }
        irow2[[i]] <- irow[[i]][j]
        sdrow <- 0.5
    }
    # so irow2 is a list of valid row numbers for each rowumn

    up.1()
    plot(asd_r,main='Row SD diagnostic')
    for (l in 1:length(irow))  points( irow[[l]], asd_r[ irow[[l]]],col=2,cex=.3)
    for (l in 1:length(irow2)) points(irow2[[l]], asd_r[irow2[[l]]],col=3,cex=.3)
    abline(h=tolrow)

    a <- as.double(readline("return to continue else enter new value for tolrow "))
    if (is.finite(a) & a>0) {
        ROWNOTDONE <- TRUE
        tolrow <- a
    } else ROWNOTDONE <- FALSE
} # end while ROWNOTDONE

# get not-row
### not working use the top and bottom grey boxes
#uirown <-notfound(unlist(irow),1:length(asd_r))
#bries <- mimpr[,uirown]
#abries <- apply(bries,1,mean)
agrtop <- apply(mimpr[,irow2[[1]]],1,mean)
agrbot <- apply(mimpr[,irow2[[length(irow2)]]],1,mean)
smagrtop <- smooth.spline(1:length(agrtop),agrtop,spar=0.9)$y
smagrbot <- smooth.spline(1:length(agrbot),agrbot,spar=0.9)$y
avecolcorr <- (smagrtop+smagrbot)/2.0
avecolcorr <- avecolcorr - mean(avecolcorr)
mimpr0 <- mimpr
mimpr.corr <- mimpr
for(x in 1:dim(mimpr)[1]) mimpr.corr[x,]  <- mimpr[x,]-avecolcorr[x]
mimpr<- mimpr.corr

### for each column and row calculate median/mean value
lpr          <- array(NA,dim=c(length(icol2),length(irow2)))
for (x in 1:length(icol2)) {
    isub <- mimpr[ icol2[[x]] , ] 
    for (y in 1:length(irow2)) {
        lpr[x,y] <- median(isub[ ,irow2[[y]] ])
    }
}
x11()
up.1()
col1 <- c('brown','black','blue','cyan','red','magenta','yellow')
plot(lpr[1,],ylim=c(0,260),col=col1[1],pch=3)
for(i in 2:7) points(lpr[i,],col=col1[i],pch=3)
for(i in 1:7) lines(lpr[i,],col=col1[i], lwd=1)
for(i in 1:7) abline(h=min(lpr[i,(2:52)]),col=col1[i], lwd=1, lty=2)
tx1 <- c('black+cya_max','black 100%','cyan+mag_max','cyan 18%','magenta+yel_max','magenta 6%','yellow 2%')
legend(30,0,tx1,col=col1,yjust=0,pch=1)

###############################
### sanity check on print image
if (F) {
x<-3
isub4    <- mimpr[ icol[[x]] , ]
tmp4     <- mimpr[ icol[[x]] , ]
isub4[,] <- NA
for (y in 1:length(irow)) isub4[ ,irow[[y]] ] <- tmp4[ ,irow[[y]] ]
lpr2          <- array(NA,dim=c(length(icol),length(irow)))
for (y in 1:length(irow)) lpr2[x,y] <- median(isub4[ ,irow[[y]] ])
points(lpr2[x,],pch=3)

x11()
plot(isub4[10,],ylim=c(170,260))
for(i in 1:length(irow)) for(j in irow[[i]]) points(rep(j,length(isub4[,j])),isub4[,j],col=i,cex=.3)
for(i in 1:length(irow)) for(j in irow[[i]]) points(j,median(isub4[,j]),col=i+1,cex=1)
for(i in 1:length(irow)) {
    t1 <- NULL
    for(j in irow[[i]]) t1 <- c(t1,isub4[,j])
    points(median(irow[[i]]),mean(t1),col=4,cex=1,pch=8)
}

masked <- mimpr
masked[,] <- NA
for (x in unlist(icol)) for (y in unlist(irow)) masked[x,y] <- mimpr[x,y]
writePNG(masked,target='maskcheck.png')
} ### end sanity check
###############################




# print out data in correct format
cat('Mean RGB values Calibration and Printer',cr)
cal.val.51 <- round(c(cal.val,rep(0,31)),2) # pad calibration to be 51 long
for (l in 1:51) cat(formatC(round(c(cal.val.51[l],lpr[,l+1]),2), format='f', digits=2 ),sep='\t',cr) # +1 to skip top grey bar

# save(file='calib_Scan7520_linux_w51-7col_epMt_aRGB_g22_small.RSave',cal.val,lpr,irow2,icol2,asd_r,asd_c)

