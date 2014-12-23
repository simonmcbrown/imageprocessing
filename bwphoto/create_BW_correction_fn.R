# source("create_BW_correction_fn.R")

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
SAVEPLOT <- FALSE
TESTING  <- TRUE

coln <- c('r.Cal.Measured','g.Cal.Measured','b.Cal.Measured','r.Print','g.Print','b.Print')
t1   <- read.table(stin,header=F,skip=0,,na.string="na",colClasses='numeric',col.name=coln,as.is=T)

#cal.rgb <- cbind(10*(2:4),20*(2:4),30*2:4)
cal.rgb <- na.omit(cbind(t1$r.Cal.Measured,t1$g.Cal.Measured,t1$b.Cal.Measured))
pnt.rgb <- na.omit(cbind(t1$r.Print,t1$g.Print,t1$b.Print))

rgb_to_Lab <- function(rgb1) {
  n1    <- dim(rgb1)[1]
  Lab   <- rgb1
  Lab[] <- NA
  rgb1 <- rgb1/255
  rgb2 <- rgb1
  for (i in 1:n1) {
    if ( rgb2[i,1] > 0.04045 ) rgb1[i,1] <- ( ( rgb1[i,1] + 0.055 ) / 1.055 ) ^ 2.4    else  rgb1[i,1] <- rgb1[i,1] / 12.92   
    if ( rgb2[i,2] > 0.04045 ) rgb1[i,2] <- ( ( rgb1[i,2] + 0.055 ) / 1.055 ) ^ 2.4    else  rgb1[i,2] <- rgb1[i,2] / 12.92   
    if ( rgb2[i,3] > 0.04045 ) rgb1[i,3] <- ( ( rgb1[i,3] + 0.055 ) / 1.055 ) ^ 2.4    else  rgb1[i,3] <- rgb1[i,3] / 12.92 
    
    rgb1[i,] <- rgb1[i,]*100  
    
    X = (rgb1[i,1] * 0.4124 + rgb1[i,2] * 0.3576 + rgb1[i,3] * 0.1805)/95.047
    Y = (rgb1[i,1] * 0.2126 + rgb1[i,2] * 0.7152 + rgb1[i,3] * 0.0722)/100.000
    Z = (rgb1[i,1] * 0.0193 + rgb1[i,2] * 0.1192 + rgb1[i,3] * 0.9505)/108.883
    
    if ( X > 0.008856 ) X <- X ^ ( 1/3 )    else     X <- ( 7.787 * X ) + ( 16 / 116 )
    if ( Y > 0.008856 ) Y <- Y ^ ( 1/3 )    else     Y <- ( 7.787 * Y ) + ( 16 / 116 )
    if ( Z > 0.008856 ) Z <- Z ^ ( 1/3 )    else     Z <- ( 7.787 * Z ) + ( 16 / 116 )
   
    Lab[i,1] <- ( 116 * Y ) - 16
    Lab[i,2] <- 500 * ( X - Y )
    Lab[i,3] <- 200 * ( Y - Z )
  }
  Lab
}

cal.lab <- rgb_to_Lab(cal.rgb)
pnt.lab <- rgb_to_Lab(pnt.rgb)

corr.fn           <- smooth.spline(na.omit(cal.lab[,1]),na.omit(L.Cal.ref),spar=0.5)
L.print.corrected <- predict(corr.fn,pnt.lab[,1])$y

# create spline for log(L.Cal.ref) = fn(VDens)
VtologL.ref.fn <- smooth.spline(VDens,log(L.Cal.ref),spar=0.5)
# calculate L.Cal.ref.51
L.Cal.ref.51 <- exp(predict(VtologL.ref.fn,VDens51)$y)

### correction functions, all in L space (not log(L))
# step 1 convert the image L to the reduced gamut of the printer: gamm_cor.fn
# step 2 correct for nonliniearities of the printer:            : nonl_cor.fn

# step 1ncompress L
# need to map cal-target [L,VDens] to printer reduced [L,VDens]
# correction 16.9.2014 need to compress the full sRGB gamut to the printer gamut (or range of L)
#mk1 dlLmin      <- min(log(L.print.corrected)) - min(log(L.Cal.ref.51))-(max(log(L.print.corrected)) - max(log(L.Cal.ref.51)))
#mk1 lL.comp     <- log(L.Cal.ref.51)+(max(log(L.print.corrected)) - max(log(L.Cal.ref.51)))         +dlLmin*(VDens51-min(VDens51))/diff(range(VDens51)) # compressed L values
#mk1 gamm_cor.fn  <- smooth.spline(L.Cal.ref.51,exp(lL.comp),spar=0.4)
dlLmin     <- (min(L.print.corrected) - 0) - (max(L.print.corrected) - 100)
L.comp     <- L.Cal.ref.51+(max(L.print.corrected) - max(L.Cal.ref.51))         +dlLmin*(VDens51-min(VDens51))/diff(range(VDens51)) # compressed L values
gamm_cor.fn  <- smooth.spline(L.Cal.ref.51,L.comp,spar=0.4)
#check over whole range of L
if(TESTING) {
  L.check1 <- predict(gamm_cor.fn,0:100)$y
  plot(0:100,L.check1,xlim=c(0,100),ylim=c(0,100))
  points(gamm_cor.fn,col=4)
  abline(0,1)
}

# step 2 correct for printer nonliniearities
# need to map the green onto the reduced VDens.comp then can calculate the L corrections needed to remove printer non-lin 
# mapping greenL to blackL
nonl_cor.fn <- smooth.spline(L.print.corrected,L.Cal.ref.51,spar=0.8) #0.4 
if(TESTING) {
  L.check2 <- predict(nonl_cor.fn,L.check1)$y
  points(0:100,L.check2,col=2)
  points(nonl_cor.fn,col=3)
}

# usage
#L.image.outA <- predict(gamm_cor.fn,L.image.in)$y
#L.image.outB <- predict(nonl_cor.fn,L.image.outA)$y

save(file=stout, gamm_cor.fn, nonl_cor.fn)

