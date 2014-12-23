# source('pr_51_step_card_rgb.R')

#input rgb values for scanned q-13 and scanned 21-step target

# L calibration values of the q-13
L.Cal.ref <- c(95.6,87.4,79.7,72.7,66.1,60.1,54.4,49.2,44.4,39.9,35.8,32.0,28.4,25.2,22.1,19.3,16.7,14.3,12.0,10.0)
# grey levels of the 21-step print
Grey.Print   <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
Grey.Print51 <- seq(from=0,to=100,by=2)
VDens        <- seq(from=0.05,to=1.95,by=0.1)
VDens51      <- seq(from=0.05,to=1.95,length=51)

# read data file
st1  <- 'in_read_scan_51wide_vista_nocol.txt'
coln <- c('r.Cal.Measured','g.Cal.Measured','b.Cal.Measured','r.Print','g.Print','b.Print')
t1   <- read.table(st1,header=F,skip=0,,na.string="na",colClasses='numeric',col.name=coln,as.is=T)
SAVEPLOT <- T

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

if (SAVEPLOT) postscript(file=sub('txt','ps',st1),horiz=F) else x11()
par(mfrow=c(3,2))
#par(mar=c(3,3,2,1))
#par(mgp=c(2,1,0))

# this is the fn to correct L for scanner error.  Use the known q-13 L values to correct the scanned L values
# need to fiddle about with spar to get smooth but not too smooth Lab_out
corr.fn <- smooth.spline(na.omit(cal.lab[,1]),na.omit(L.Cal.ref),spar=0.5)
# if want to check smoothness of spline
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

mtext(st1,outer=T,col=1,line=-1.1) # center justfied

dev.off()

# paste this output into text file for the QTR-icc programme
cat('Gray','Lab_L',cr)
for (i in 1:length(Grey.Print51)) cat(Grey.Print51[i],L.print.corrected[i],cr)
#######################################################################################

