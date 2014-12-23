# taken from http://www.easyrgb.com/index.php?X=MATH

Lab_to_srgb <- function(Lab) {

### Lab to XYZ
var_Y = ( Lab[1] + 16 ) / 116
var_X =   Lab[2] / 500 + var_Y
var_Z =   var_Y  - Lab[3] / 200

if ( var_Y^3 > 0.008856 ) var_Y = var_Y^3  else  var_Y = ( var_Y - 16 / 116 ) / 7.787
if ( var_X^3 > 0.008856 ) var_X = var_X^3  else  var_X = ( var_X - 16 / 116 ) / 7.787
if ( var_Z^3 > 0.008856 ) var_Z = var_Z^3  else  var_Z = ( var_Z - 16 / 116 ) / 7.787

X = 95.047  * var_X     #  Observer= 2°, Illuminant= D65
Y = 100.000 * var_Y     
Z = 108.883 * var_Z     

### XYZ to RGB
var_X = X / 100        # X from 0 to  95.047      (Observer = 2°, Illuminant = D65)
var_Y = Y / 100        # Y from 0 to 100.000
var_Z = Z / 100        # Z from 0 to 108.883

var_R = var_X *  3.2406 + var_Y * -1.5372 + var_Z * -0.4986
var_G = var_X * -0.9689 + var_Y *  1.8758 + var_Z *  0.0415
var_B = var_X *  0.0557 + var_Y * -0.2040 + var_Z *  1.0570

if ( var_R > 0.0031308 ) var_R = 1.055 * ( var_R ^ ( 1 / 2.4 ) ) - 0.055  else  var_R = 12.92 * var_R
if ( var_G > 0.0031308 ) var_G = 1.055 * ( var_G ^ ( 1 / 2.4 ) ) - 0.055  else  var_G = 12.92 * var_G
if ( var_B > 0.0031308 ) var_B = 1.055 * ( var_B ^ ( 1 / 2.4 ) ) - 0.055  else  var_B = 12.92 * var_B

R = var_R * 255
G = var_G * 255
B = var_B * 255

c(R,G,B)

}

