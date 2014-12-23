# source("make_target.R")

library(png)
library(fields)

stout      <- 'wedge_one_column_51.png'
step_start <- 1     ### bit odd. gimp suggests byte range goes from 1 to 255 NOT 0:255 or 1:256
step_end   <- 255
nsteps     <- 51
steps      <- round(seq(to=step_start, from=step_end, length.out=nsteps))

h1 <- 10
h2 <- 2
w1 <- 50
w2 <- 70
header <- h1

img.rgba  <- array(step_start,dim=c( (nsteps-1)*(h1+h2)+h1+2*header,   w1+w2))

for (i in 1:(nsteps-1)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:w1)] <- steps[i]

for (i in 1:(nsteps-1)) {
    if (steps[i] <= 127) img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:w1)] <- 255
    else                 img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:w1)] <- 1
}
img.rgba[(1:header),] <- 127
img.rgba[ (dim(img.rgba)[1]-header+1) : dim(img.rgba)[1],] <- 127

#image.plot(img.rgba[,])

writePNG(img.rgba, target=stout)