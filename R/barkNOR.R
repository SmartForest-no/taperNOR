#' Bark model for spruce, pine, and birch for Norway
#'
#' The bark models are based on Gordon (1983) via Stängle (2016). Height is distance from stem base.
#'
#' @param d diameter (cm).
#' @param h height (m).
#' @param dbh diameter at breast height (cm).
#' @param h_top tree height (m).
#' @param sp species ('spruce','pine' or 'birch'; 1:3).
#' @return double bark thickness at h (mm).
#' @examples
#' barkNOR(d=30, h=1,dbh=25,h_top=30,sp="pine")
#'
#' taper<-taperNOR(h=(0:300)/10,dbh=25,h_top=30,sp="pine")
#' bark<-barkNOR(d=taper, h=(0:300)/10,dbh=25,h_top=30,sp="pine")
#' plot((0:300)/10,taper,type="l",col="brown",xlab="height (m)",ylab="taper/bark (cm)")
#' points((0:300)/10,taper-bark/10,type="l",col="green")
#'
#' plot((0:300)/10,bark,type="l",ylab="bark (mm)",xlab="height (m)")
#' @export


barkNOR <- function(d,h,dbh,h_top,sp="spruce"){


  if(sum(!c(class(h),class(dbh),class(h_top))%in%c("numeric","integer"))>0){
    stop("h, dbh, and h_top must be numeric.")
  }



  sp<-tolower(as.character(sp))

  if(sp%in%c("spruce","s","gran","g","1")){
    b0 <- 16.5854166123312
    b1 <- -16.3796705441853
    b2 <- 0.00322093293639776
    b3 <- 0.534944513368138
    b4 <- 0.593920452511068
    b5 <- -0.00317409820187845
    b6 <- -0.0216956105818279
  } else if (sp%in%c("pine","p","furu","f","2")){

    b0 <- 1.10316721739448
    b1 <- 1.36530542477874
    b2 <- 7.3674429189434
    b3 <- 1.76577924416511
    b4 <- 0.707317252886313
    b5 <- -0.0173465347111805
    b6 <- -0.278702673825782

  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                                  "lauv","l","3")){
    b0 <- -2.14888054022854
    b1 <- 2.08810690768153
    b2 <- 1.60626481869625
    b3 <- 2.72701321244184
    b4 <- 0.0800046470288129
    b5 <- 0.0392933980145204
    b6 <- 0.42805896337782

  } else{
    stop("sp must be in c(\"spruce\",\"pine\",\"birch\")")
  }

  b <- exp((b0 + b1 * (1 - (h/h_top))^b2 + b3 * (h/h_top)^(b4 * h_top) + b5 * (dbh) + b6 * h_top/dbh)) * d


  return(b)
}



