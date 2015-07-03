# source("read_gradients_print.R")

library(png)
library(fields)
library(caTools)

stimg <- '/data/local/hadsx/home_overflow_ld1/me/photo/BW_bits/scans/ScanWork_gradients_inkriteMt_print.png'

img.rgba  <- readPNG(stimg)

#i2 <- array(0,dim=c(dim(img.rgba)[1],dim(img.rgba)[2],3))
#i2 <- img.rgba[,,1:3]
#i2 <- apply(img.rgba[,,1:3],3,mean)
i2 <- apply(img.rgba[,,1:3],1:2,sum)
i2 <- i2/3

 plot(1:2, type='n')
 rasterImage(i2, 1.0, 1.0, 2.0, 2.0)

# partition the image
sx <- apply(i2,1,sum)  # mean along gradients
sy <- apply(i2,2,sum)  # mean across gradients

dsy <- c(0,diff(sy,lag=1))
id <- which(abs(dsy) >20 | sy >(max(sy)-100) )
# fix the holes
dd <- which(diff(id)<50 & diff(id)>1)
for(j in dd) id <- c(id,seq(from=id[j],to=id[j+1]))

id <- unique(id)
id <- sort(id)


up.2()
plot(1:length(sy),dsy,ty='l')
points((1:length(sy))[id],dsy[id],col=2,cex=.3)

plot(1:length(sy),sy,ty='l')
points((1:length(sy))[id],sy[id],col=2,cex=.3)

i3 <- i2
i3[,id] <- NA  # mask out hte column borders
# nb i3[along-gradient,cross_gradients]
icol <- array(NA,dim=c(9,2))
i    <- 1
for (c1 in 1:9) {
    while(is.na(i3[500,i]) ) i = i+1
    icol[c1,1] <- i
    while(!is.na(i3[500,i]) ) i = i+1
    icol[c1,2] <- i
}    
# check
x0 <- 1:(dim(i3)[2])
plot(x0,i3[500,])
for(i in 1:9) points(seq(from=icol[i,1],to=icol[i,2]),i3[500,][seq(from=icol[i,1],to=icol[i,2])],col=2,cex=.3)

### icol gives begining and end of the column index that define the gradients.
gall <- list()
for(i in 1:9) gall[[i]] <- apply(i3[,(icol[i,1]):(icol[i,2])],1,mean,na.rm=T)
up.1()
plot(gall[[1]],cex=.3)
for(i in 2:9) points(gall[[i]],col=i,cex=.3)

### next find start and end of each gradient
galla      <- unlist(gall)             
dim(galla) <- c(length(gall[[1]]),9)
plot(galla[1:200,1])
for(i in 2:9) points(galla[1:200,i],col=i)
abline(v=183)

for(i in 1:9) {
    plot(galla[1:300,i],main=i,ylim=c(0,1))
    points(scale(runsd(galla[1:300,i],k=11),center=TRUE,scale=max(runsd(galla[1:300,i],k=11)))+0.5,col=2,cex=.3)
    abline(v=207)
    readline("Continue")
}
ltrim <- 207
galla <- galla[ (ltrim:length(galla[,1])) ,]

l1 <- seq(from=length(galla[,1])-400,to=length(galla[,1]))
for(i in 1:9) {
    plot(l1,galla[l1,i],main=i,ylim=c(0,1))
    points(l1,scale(runsd(galla[l1,i],k=11),center=TRUE,scale=max(runsd(galla[l1,i],k=11)))+0.5,col=2,cex=.3)
    abline(v=2012)
    readline("Continue")
}
rtrim <- 2007
galla <- galla[ 1:rtrim ,]
plot(galla[,1],ylim=c(0,1),type='l')
for(i in 2:9) lines(galla[,i],col=i,cex=.3)

###  fit smooth.spine to each gradient
gr.sp <- NULL
for(i in 1:9) gr.sp[[i]] <- smooth.spline(seq(from=0, to=255, length=length(galla[,i])),galla[,i],spar=.5)
plot(gr.sp[[1]],ylim=c(0,1))
points(gr.sp[[2]],cex=.3)
points(gr.sp[[3]],cex=.3)
