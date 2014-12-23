# source("apply_calib_to_png.R")

library(png)
source("srgb_to_Lab.R")
source("Lab_to_srgb.R")

stin <- 'sjb_ruin_51step_target.png'

# usage
#L.image.outA <- predict(gamm_cor.fn,L.image.in)$y
#L.image.outB <- predict(nonl_cor.fn,L.image.outA)$y
load('calfn_read_scan_51wide_vista_nocol.RSave')

# readPNG(source, native = FALSE, info = FALSE)
img.rgba  <- readPNG(stin)
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

# convert to Lab
image.Lab <- apply(img.rgba[,,1:3],c(1,2),srgb_to_Lab)
# apply no different in time to below
# Lab2 <- img.rgba[,,1:3]
# Lab2[,,] <- NA
# for(y in 1:dim(img.rgba)[2]) for(x in 1:dim(img.rgba)[1]) Lab2[x,y,] <- srgb_to_Lab(img.rgba[x,y,1:3])
# rasterImage(image.Lab[1,,], 1.0, 1.3, 2.0, 1.4)


################################
image.L.outA <- predict(gamm_cor.fn,image.Lab[1,,])$y
image.L.outB <- predict(nonl_cor.fn,image.L.outA)$y
ix <- which(image.L.outB < 0)
image.L.outB[ix] <- 0
ix <- which(image.L.outB > 100)
image.L.outB[ix] <- 100

#test
plot(c(image.Lab[1,190,],image.Lab[1,120,],image.Lab[1,40,]))
points(c(image.L.outA[190,],image.L.outA[120,],image.L.outA[40,]),col=2,cex=.3)
points(c(image.L.outB[190,],image.L.outB[120,],image.L.outB[40,]),col=3,cex=.3)

readline("Stop")
# convert back to rgba
image.Lab.outB <- aperm(image.Lab,c(2,3,1))
image.Lab.outB[,,1] <- image.L.outB
out.rgb  <- apply(image.Lab.outB,c(1,2),Lab_to_srgb)
out.rgb  <- aperm(out.rgb,c(2,3,1))
out.rgba <- img.rgba
out.rgba[,,1] <- out.rgb[,,1]/255
out.rgba[,,2] <- out.rgb[,,2]/255
out.rgba[,,3] <- out.rgb[,,3]/255

stout <- sub('.png','_2print.png',stin)
writePNG(out.rgba, target=stout)

