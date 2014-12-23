# source("make_target_sepparated_colours_7columns.R")

library(png)
library(fields)

# The which channles to make cyan/yellow/magenta are (1)/(3)/(2)
stout      <- 'wedge_7column_51_cymkrbe_boundaries.png'
# 7 channels
# y == 2%
# m == 6%
# c == 18%
# k == 100%
# r == ymax+m
# b == mmax+c
# e == cmax+b

step_start <- 1     ### bit odd. gimp suggests byte range goes from 1 to 255 NOT 0:255 or 1:256
step_end   <- 255
nsteps     <- 51
steps      <- round(seq(to=step_start, from=step_end, length.out=nsteps))

h1 <- 10
h2 <- 2
w1 <- 25
w2 <- 60
w3 <- 2
header <- h1
                                 #            height                     width       depth
img.rgba  <- array(step_start,dim=c( (nsteps-1)*(h1+h2)+h1+2*header,   7*w1+w2+3*w3, 3))

for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , (            1:        w1), 3] <- steps[i] # col1 yellow

for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+  w1+  w3):(  w3+2*w1)),2] <- steps[i] # col2 magenta

for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+2*w1+2*w3):(2*w3+3*w1)),2] <- steps[i] # col3 magenta + max_yellow
for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+2*w1+2*w3):(2*w3+3*w1)),3] <- step_end # col3 magenta + max_yellow == red

for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+3*w1+3*w3):(3*w3+4*w1)),1] <- steps[i] # col4 cyan

for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+4*w1+4*w3):(4*w3+5*w1)),1] <- steps[i] # col5 cyan + max_magenta
for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+4*w1+4*w3):(4*w3+5*w1)),2] <- step_end # col5 cyan + max_magenta

for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+5*w1+5*w3):(5*w3+6*w1)), ] <- steps[i] # col6 black

for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+6*w1+6*w3):(6*w3+7*w1)), ] <- steps[i] # col6 black + max_cyan
for (i in 1:(nsteps)) img.rgba[(header+1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)) , ((1+6*w1+6*w3):(6*w3+7*w1)),1] <- step_end # col6 black + max_cyan

# colour boundaries = but scanner has some form of response lag accross high contrast so need to put in mild contrast.
for (i in 1:(nsteps-1)) {
    ## just white
    #img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:(7*w1+6*w3)),] <- 1   # not in steps
    ## black and white
    #if (steps[i] <= 127) img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:(7*w1+6*w3)),] <- 255 # not in steps
    #else                 img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:(7*w1+6*w3)),] <- 2   # not in steps
    ## average accross row
    mn3 <- rep(mean(img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:(7*w1+6*w3)),]),3)
    img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:(7*w1+6*w3)),] <- round(mn3)   # not in steps

}

#for (i in 1:(nsteps-1)) {
#    for (j in 1:(nsteps-1)) {
#    if (steps[i] <= 127) img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:(7*w1+6*w3)),] <- 254 # not in steps
#    else                 img.rgba[(header+1+h1+(i-1)*(h1+h2)):(header+(i)*(h1+h2)),(1:(7*w1+6*w3)),] <- 2   # not in steps
#}
#}


img.rgba[(1:header),,] <- 127
img.rgba[ (dim(img.rgba)[1]-header+1) : dim(img.rgba)[1],,] <- 127

#image.plot(img.rgba[,])
readline("Continue?")

writePNG(img.rgba, target=stout)