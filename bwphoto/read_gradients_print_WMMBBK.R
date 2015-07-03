# source("read_gradients_print.R")

library(png)
library(fields)
library(caTools)

stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/ScanToSelf_20150703_inkriteMt_gradient_WMMBBK_crop.png'

img.rgba  <- readPNG(stimg)

#img2 <- array(0,dim=c(dim(img.rgba)[1],dim(img.rgba)[2],3))
#img2 <- img.rgba[,,1:3]
#img2 <- apply(img.rgba[,,1:3],3,mean)
if(!is.na(dim(img.rgba)[3]) ) {
    img2 <- apply(img.rgba[,,1:3],1:2,sum)
    img2 <- img2/3
} else img2 <- img.rgba
### fake the white  endstop
img2[seq(from=dim(img2)[1]-10,to=dim(img2)[1]),] <- 0.9
### bodge the scan biases
i3 <- img2
iev <- seq(from=2,to=dim(img2)[1],by=2)
iod <- seq(from=1,to=dim(img2)[1]-1,by=2)
i3[iev,] <- (img2[iev,] + img2[iod,] ) /2.0
i3[iod,] <- (img2[iev,] + img2[iod,] ) /2.0
img2 <- i3
i3 <- NULL
plot(1:2, type='n')
rasterImage(img2, 1.0, 1.0, 2.0, 2.0)

# partition the image
sy <- apply(img2,1,sum)  # mean along gradients
sx <- apply(img2,2,sum)  # mean across gradients

### might need to flatten the end of the black to remove the rise


#ssp.sy <- smooth.spline(1:length(sy),sy)
#dsy <- c(0,diff(sy,lag=1))
#id <- which(abs(dsy) >4 | sy >(max(sy)-100) )

# do by hand as so much easyier
up.1()
i1 <- 1:100
plot(i1,sy[i1],ty='b',pch=3,cex=.3)
grid()
ig1a <- 15
points(1:ig1a,sy[1:ig1a],col=2,cex=.8)

i2 <- 890:960
plot(i2,sy[i2],ty='b',pch=3,cex=.3)
grid()
ig1b <- 896
ig2a <- 917
points(ig1b:ig2a,sy[ig1b:ig2a],col=2,cex=.8)

i3 <- 1780:1830
plot(i3,sy[i3],ty='b',pch=3,cex=.3)
grid()
ig2b <- 1798
ig3a <- 1817
points(ig2b:ig3a,sy[ig2b:ig3a],col=2,cex=.8)

i4 <- 2660:dim(img2)[1]
plot(i4,sy[i4],ty='b',pch=3,cex=.3)
grid()
ig3b <- 2686
ig4a <- dim(img2)[1]
points(ig3b:ig4a,sy[ig3b:ig4a],col=2,cex=.8)

gr0 <- list()
gr0[[1]] <- sy[ig1a:ig1b]/dim(img2)[2]
gr0[[2]] <- sy[ig2a:ig2b]/dim(img2)[2]
gr0[[3]] <- sy[ig3a:ig3b]/dim(img2)[2]
 plot(gr0[[1]],ylim=c(0,1))
 points(gr0[[2]],ylim=c(0,1),col=2,cex=.3)
 points(gr0[[3]],ylim=c(0,1),col=3,cex=.3)
 points(rev(gr0[[2]]),ylim=c(0,1),col=2,cex=.3)


###  fit smooth.spine to each gradient
gr.sp <- NULL
for(i in 1:3) gr.sp[[i]] <- smooth.spline(seq(from=0, to=255, length=length(gr0[[i]])), gr0[[i]],spar=.5)
plot(gr.sp[[1]],ylim=c(0,1))
points(gr.sp[[2]],cex=.3)
points(gr.sp[[2]],cex=.3)
points(rev(gr.sp[[2]]$x),gr.sp[[2]]$y,cex=.3,col=2)
points(gr.sp[[3]],cex=.3)
