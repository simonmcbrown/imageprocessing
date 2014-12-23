# source("apply_calib_to_png_devel.R")


# 2014.09.16
# Current problems are:
# 1)the converted L values at the top end go greater than the max allowed
# 2) the bottom end is so flat, that the truncation to the 3x8 bit rgb means most of the dark values are all the same this might be due to the very sharp drop for the printed Lvalues with VDens.  A change in L of 0.01 changes RGB by ~3.5 at the dark end, this needs to be reduced by the reduced L range of the printer (73.07668). 3.5*.7307668=2.557684

library(png)
source("srgb_to_Lab.R")
source("Lab_to_srgb.R")

TESTING  <- TRUE

#stimg <- 'sjb_ruin_51step_target.png'
stimg <- 'sjb_51_grey_step_wide_rgb_aRGB.png'

##############################################################
### create correction functions from create_BW_correction_fn.R
##############################################################
# L calibration values of the q-13
L.Cal.ref <- c(95.6,87.4,79.7,72.7,66.1,60.1,54.4,49.2,44.4,39.9,35.8,32.0,28.4,25.2,22.1,19.3,16.7,14.3,12.0,10.0)
# grey levels of the 21-step print
Grey.Print   <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
Grey.Print51 <- seq(from=0,to=100,by=2)
VDens        <- seq(from=0.05,to=1.95,by=0.1)
VDens51      <- seq(from=0.05,to=1.95,length=51)
# read data file
stin   <- 'in_read_scan_51wide_vista_nocol.txt'
stout <- sub('in_','calfn_mk2_',stin)
stout <- sub('txt','RSave',stout)
SAVEPLOT <- T

coln <- c('r.Cal.Measured','g.Cal.Measured','b.Cal.Measured','r.Print','g.Print','b.Print')
t1   <- read.table(stin,header=F,skip=0,,na.string="na",colClasses='numeric',col.name=coln,as.is=T)

#cal.rgb <- cbind(10*(2:4),20*(2:4),30*2:4)
cal.rgb <- na.omit(cbind(t1$r.Cal.Measured,t1$g.Cal.Measured,t1$b.Cal.Measured))
pnt.rgb <- na.omit(cbind(t1$r.Print,t1$g.Print,t1$b.Print))


cal.lab <- rgb_to_Lab(cal.rgb)
pnt.lab <- rgb_to_Lab(pnt.rgb)

corr.fn           <- smooth.spline(na.omit(cal.lab[,1]),na.omit(L.Cal.ref),spar=0.5)
L.print.corrected <- predict(corr.fn,pnt.lab[,1])$y
# create spline for log(L.Cal.ref) = fn(VDens)
VtologL.ref.fn <- smooth.spline(VDens,log(L.Cal.ref),spar=0.5)
# calculate L.Cal.ref.51
L.Cal.ref.51 <- exp(predict(VtologL.ref.fn,VDens51)$y)
# step 1b compress VDens
# prob need to set the point at which VDens saturates by eye
max.pr.VD <- VDens51[length(VDens51)+1-3] # 3 == 3rd point from end
VD.comp   <- VDens51 - 0:50*(max(VDens51)-max.pr.VD)/50


### correction functions, all in L space (not log(L))
# step 1 convert the image L to the reduced gamut of the printer: gamm_cor.fn
# step 2 correct for nonliniearities of the printer:            : nonl_cor.fn

### step 1 compress L
# need to map cal-target [L,VDens] to printer reduced [L,VDens]
# correction 16.9.2014 need to compress the full sRGB gamut to the printer gamut (or range of L)
#mk1 dlLmin      <- min(log(L.print.corrected)) - min(log(L.Cal.ref.51))-(max(log(L.print.corrected)) - max(log(L.Cal.ref.51)))
#mk1 lL.comp     <- log(L.Cal.ref.51)+(max(log(L.print.corrected)) - max(log(L.Cal.ref.51)))         +dlLmin*(VDens51-min(VDens51))/diff(range(VDens51)) # compressed L values
#mk1 gamm_cor.fn  <- smooth.spline(L.Cal.ref.51,exp(lL.comp),spar=0.4)
dlLmin     <- (min(L.print.corrected) - 0) - (max(L.print.corrected) - 100)
L.comp     <- L.Cal.ref.51+(max(L.print.corrected) - max(L.Cal.ref.51))         +dlLmin*(VDens51-min(VDens51))/diff(range(VDens51)) # compressed L values
gamm_cor.fn  <- smooth.spline(L.Cal.ref.51,L.comp,spar=0.4)
# mk3 
# extend the Vdens to the full 0 to 100 L range
L_to_VD.fn <- smooth.spline(L.Cal.ref.51,VDens51,spar=0.4)
VDensfull  <- predict(L_to_VD.fn,0:100)$y
# convert L-in to L-out keeping the VDens-L relationship ### not sure about this, could try linear
dlLmin     <- diff(range(L.print.corrected))
L.comp2     <- max(L.print.corrected) -dlLmin*(VDensfull-min(VDensfull))/diff(range(VDensfull)) # compressed L values
gamm_cor.fn  <- smooth.spline(0:100,L.comp2,spar=0.4)
# tweaking to get it right
dlLmin       <- 78.2
L.comp2      <- max(L.print.corrected) -dlLmin*(VDensfull-min(VDensfull))/diff(range(VDensfull)) # compressed L values
gamm_cor.fn  <- smooth.spline(0:100,L.comp2,spar=0.4)


### step 2 correct for printer nonliniearities
# need to map the green onto the reduced VDens.comp then can calculate the L corrections needed to remove printer non-lin 
# mapping greenL to blackL
nonl_cor.fn <- smooth.spline(L.print.corrected,L.Cal.ref.51,spar=0.4) #0.4 
# usage
#L.image.outA <- predict(gamm_cor.fn,L.image.in)$y
#L.image.outB <- predict(nonl_cor.fn,L.image.outA)$y


##############################################################
### apply to "image"
##############################################################

if(TESTING) {
  #img.rgba<-array(0,dim=c(10,1,3))
  #image.Lab<-array(0,dim=c(10,1,3))
  #image.XYZ<-array(0,dim=c(10,1,3))
  #image.rgb2<-array(0,dim=c(10,1,3))
  #img.rgba[,1,1]<-c(0,6,11,16,21,235,240,245,250,255)/255
  #img.rgba[,1,2]<-c(0,6,11,16,21,235,240,245,250,255)/255
  #img.rgba[,1,3]<-c(0,6,11,16,21,235,240,245,250,255)/255
  img.rgba  <-array(0,dim=c(256,1,3))
  image.Lab <-array(0,dim=c(256,1,3))
  image.XYZ <-array(0,dim=c(256,1,3))
  image.rgb2<-array(0,dim=c(256,1,3))
  img.rgba[,1,1]<-c(0:255)/255
  img.rgba[,1,2]<-c(0:255)/255
  img.rgba[,1,3]<-c(0:255)/255
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
  image.Lab   <- img.rgba[,,1:3]
  image.Lab[] <- NA
  image.XYZ   <- image.Lab
  image.rgb2  <- image.Lab
}

if(max(img.rgba[,,1:3]) >1 & max(img.rgba[,,1:3]) <101 ) img.rgba[,,1:3] <- img.rgba[,,1:3]/100
if(max(img.rgba[,,1:3]) >100 )                           img.rgba[,,1:3] <- img.rgba[,,1:3]/255

image.rgb2[,,1]  <- ( ( img.rgba[,,1] + 0.055 ) / 1.055 ) ^ 2.4
image.rgb2[,,2]  <- ( ( img.rgba[,,2] + 0.055 ) / 1.055 ) ^ 2.4
image.rgb2[,,3]  <- ( ( img.rgba[,,3] + 0.055 ) / 1.055 ) ^ 2.4

i2             <- which(img.rgba[,,1:3] <=0.04045)
image.rgb2[i2] <- img.rgba[,,1:3][i2] / 12.92
image.rgb2     <- image.rgb2*100.0

image.XYZ[,,1]  <- (image.rgb2[,,1]*0.4124 + image.rgb2[,,2]*0.3576 + image.rgb2[,,3]*0.1805) / 95.047
image.XYZ[,,2]  <- (image.rgb2[,,1]*0.2126 + image.rgb2[,,2]*0.7152 + image.rgb2[,,3]*0.0722) / 100.000
image.XYZ[,,3]  <- (image.rgb2[,,1]*0.0193 + image.rgb2[,,2]*0.1192 + image.rgb2[,,3]*0.9505) / 108.883

i2            <- which(image.XYZ <=0.008856)
imgT          <- image.XYZ
image.XYZ     <- image.XYZ^(1/3)
image.XYZ[i2] <- (7.787*imgT[i2]) + (16/116)
imgT          <- NULL

image.Lab[,,1] <- ( 116 * image.XYZ[,,2] ) - 16
image.Lab[,,2] <- 500 * ( image.XYZ[,,1] - image.XYZ[,,2] )
image.Lab[,,3] <- 200 * ( image.XYZ[,,2] - image.XYZ[,,3] )


################################
image.L.outA <- predict(gamm_cor.fn,image.Lab[,,1])$y
image.L.outB <- predict(nonl_cor.fn,image.L.outA)$y
###ix <- which(image.L.outB < 0)
###image.L.outB[ix] <- 0
###ix <- which(image.L.outB > 100)
###image.L.outB[ix] <- 100

# convert back to rgba
##image.Lab.outB <- aperm(image.Lab,c(2,3,1))
##image.Lab.outB[,,1] <- image.L.outB
##out.rgb  <- apply(image.Lab.outB,c(1,2),Lab_to_srgb)
##out.rgb  <- aperm(out.rgb,c(2,3,1))

imgY <- ( image.L.outB + 16 ) / 116
imgX <- imgY + image.Lab[,,2] / 500
imgZ <- imgY - image.Lab[,,3] / 200

imgT <- imgY
imgY <- imgY^3
i2   <- which(imgY <=0.008856)
imgY[i2] <- ( imgT[i2] - 16 / 116 ) / 7.787

imgT <- imgX
imgX <- imgX^3
i2   <- which(imgX <=0.008856)
imgX[i2] <- ( imgT[i2] - 16 / 116 ) / 7.787

imgT <- imgZ
imgZ <- imgZ^3
i2   <- which(imgZ <=0.008856)
imgZ[i2] <- ( imgT[i2] - 16 / 116 ) / 7.787

imgX <- 0.95047 * imgX
imgY <- 1.00000 * imgY
imgZ <- 1.08883 * imgZ

imgR <- imgX *  3.2406 + imgY * -1.5372 + imgZ * -0.4986
imgG <- imgX * -0.9689 + imgY *  1.8758 + imgZ *  0.0415
imgB <- imgX *  0.0557 + imgY * -0.2040 + imgZ *  1.0570

imgT     <- imgR
i2       <- which(imgR <=0.0031308)
imgR     <- 1.055 * ( imgR ^ ( 1 / 2.4 ) ) - 0.055
imgR[i2] <- 12.92 * imgT[i2]

imgT     <- imgG
i2       <- which(imgG <=0.0031308)
imgG     <- 1.055 * ( imgG ^ ( 1 / 2.4 ) ) - 0.055
imgG[i2] <- 12.92 * imgT[i2]

imgT     <- imgB
i2       <- which(imgB <=0.0031308)
imgB     <- 1.055 * ( imgB ^ ( 1 / 2.4 ) ) - 0.055
imgB[i2] <- 12.92 * imgT[i2]

# for output image
out.rgba <- img.rgba
out.rgba[,,1] <- imgR
out.rgba[,,2] <- imgG
out.rgba[,,3] <- imgB

stout <- sub('.png','_mk3_2printb.png',stimg)
if(!TESTING) writePNG(out.rgba, target=stout)

if(TESTING) cat(round(image.L.outB[c(1:5)],2) ,cr)
if(TESTING) cat(round(image.L.outB[c(251:256)],2) ,cr,cr)
if(TESTING) cat(round((imgR*255)[c(1:5)],2) ,cr)
if(TESTING) cat(round((imgR*255)[c(251:256)],2) ,cr)


if(SAVEPLOT) postscript(sub('.txt','_devel.ps',stin),horiz=F) else if(TESTING) x11()
par(mfrow=c(3,2))
if (TESTING) {
  xnew <- 1:100
  ynew <- predict(corr.fn,xnew)$y
  plot(cal.lab[,1],L.Cal.ref,xlab='Lab in',ylab='Lab out',ty='b',xlim=c(0,100),ylim=c(0,100),main='Correction Spline fit to measured')
  abline(h=seq(from=0,to=100,by=10),v=seq(from=0,to=100,by=10),col='grey')
  lines(xnew,ynew,col=2,pch=2)
  legend(0,100,c('obs','spline'),col=c(1,2),lty=c(-1,1),pch=c(1,NA))

  cal.lab.corrected <- predict(corr.fn,na.omit(cal.lab[,1]))$y
  plot(VDens,cal.lab[,1],xlab='Vis density',ylab='L',ty='b',main='Verify calibration',ylim=c(0,100))
  abline(h=seq(from=0,to=100,by=10),v=seq(from=0,to=2,by=0.2),col='grey')
  lines(VDens,cal.lab.corrected,col=2)
  points(VDens,L.Cal.ref,       col=3,pch=2)
  legend(0.6,100,c('CalTarget measured','CalTarget corrected', 'CalTarget reference'),col=c(1,2,3),lty=c(1,1,-1),pch=c(1,NA,2))

  # Use corr.fn to correct the scanned L values of the printed strip
  L.print.corrected <- predict(corr.fn,pnt.lab[,1])$y
    plot(Grey.Print51,pnt.lab[,1],xlab='Grey',ylab='L',ty='b', main='51 Step values',ylim=c(0,100))
  abline(h=seq(from=0,to=100,by=10),v=seq(from=0,to=100,by=10),col='grey')
  points(Grey.Print51,L.print.corrected,col=2)
   lines(Grey.Print51,L.print.corrected,col=2)
  legend(0,30,c('21step measured','21step corrected'),col=c(1,2),lty=c(1,1),pch=c(1,1))

    plot(VDens51,L.Cal.ref.51,main='Reference-L, Reduced-L, Printed-L',xlim=c(0,2.5),ylim=c(0,100))
  points(VDens51,L.print.corrected,col=3)
  sm.L.print.corrected <- smooth.spline(L.print.corrected,spar=0.4)$y
  lines(VDens51,exp(sm.L.print.corrected))# smooth L.print.corrected
  #points(VD.comp,L.comp51,col=2) # compressed L values an VDens values
  #points(VD.comp,L.comp51,col=2) # compressed L values an VDens values
  #points(VD.comp,exp(test2),col=6)  ### this is it!!!!!!!
  grid(nx=50,col='darkgrey',lty=1)

  mtext(stin,outer=T,col=1,line=-1.1) # center justfied

#check over whole range of L
  L.check1 <- predict(gamm_cor.fn,0:100)$y

  plot(0:100,L.check1,xlim=c(0,100),ylim=c(0,100),main='Help',xlab='L-in',ylab='L-out')
  points(gamm_cor.fn,col=2,cex=.3)
  abline(0,1)
  L.check2 <- predict(nonl_cor.fn,L.check1)$y
  points(0:100,L.check2,col=3)
  points(nonl_cor.fn,col=4)
  legend(0,100,c('cor_gamut','fn.cor_gamut','cor_printer','fn.cor.print'),col=c(1:4),pch=1)

  plot(c(image.Lab[,1,1]),xlab='RGB-0-255',ylab='L value')
  points(c(image.L.outA),col=2,cex=.3)
  points(c(image.L.outB),col=3,cex=.3)
  legend(0,100,c('image.Lab','image.L.outA','image.L.outB'),col=c(1:3),pch=1)

}

if(SAVEPLOT) dev.off()