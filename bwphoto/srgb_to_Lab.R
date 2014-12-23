# taken from http://www.easyrgb.com/index.php?X=MATH

srgb_to_Lab <- function(rgb1) {
  # s == rgb values have already been standardised eg range 0 to 1
  # takes a single rgb triple
  
  rgb2  <- rgb1
  Lab   <- rgb1
  Lab[] <- NA
  
    if ( rgb2[1] > 0.04045 ) rgb1[1] <- ( ( rgb1[1] + 0.055 ) / 1.055 ) ^ 2.4    else  rgb1[1] <- rgb1[1] / 12.92   
    if ( rgb2[2] > 0.04045 ) rgb1[2] <- ( ( rgb1[2] + 0.055 ) / 1.055 ) ^ 2.4    else  rgb1[2] <- rgb1[2] / 12.92   
    if ( rgb2[3] > 0.04045 ) rgb1[3] <- ( ( rgb1[3] + 0.055 ) / 1.055 ) ^ 2.4    else  rgb1[3] <- rgb1[3] / 12.92 
    
    rgb1[] <- rgb1[]*100  
    
    X = (rgb1[1] * 0.4124 + rgb1[2] * 0.3576 + rgb1[3] * 0.1805)/95.047
    Y = (rgb1[1] * 0.2126 + rgb1[2] * 0.7152 + rgb1[3] * 0.0722)/100.000
    Z = (rgb1[1] * 0.0193 + rgb1[2] * 0.1192 + rgb1[3] * 0.9505)/108.883
    
    if ( X > 0.008856 ) X <- X ^ ( 1/3 )    else     X <- ( 7.787 * X ) + ( 16 / 116 )
    if ( Y > 0.008856 ) Y <- Y ^ ( 1/3 )    else     Y <- ( 7.787 * Y ) + ( 16 / 116 )
    if ( Z > 0.008856 ) Z <- Z ^ ( 1/3 )    else     Z <- ( 7.787 * Z ) + ( 16 / 116 )
   
    Lab[1] <- ( 116 * Y ) - 16
    Lab[2] <- 500 * ( X - Y )
    Lab[3] <- 200 * ( Y - Z )
 
  Lab
}

###############################################################################
srgb_to_Lab_1d <- function(rgb1) {
  # s == rgb values have already been standardised eg range 0 to 1
  # takes a 1d line of an image
  
  n1    <- dim(rgb1)[1]
  Lab   <- rgb1
  Lab[] <- NA
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

###############################################################################
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
