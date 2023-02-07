#' Bark model for spruce, pine, and birch for Norway
#'
#' The bark models are based on Gordon A. 1983. Estimating bark thickness of Pinus radiata. NZJ For Sci. 13(3):340–348.
#' via Stängle et al. 2017. Comparison of models for estimating bark thickness of Picea abies in southwest Germany: the role of tree, stand, and environmental factors. Ann For Sci. 74(1):16.
#'
#' @param d diameter (cm).
#' @param h height above ground (m).
#' @param dbh diameter at breast height, 1.3 m above ground (cm).
#' @param h_top tree height above ground (m).
#' @param sp species ('spruce','pine' or 'birch'; 1:3)
#' @return double bark thickness (mm) at height h with diameter d.
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
    b0 <- -89.5977269892
    b1 <- 90.0234341489
    b2 <- -0.0052588585
    b3 <- -0.5110831924
    b4 <- 0.0243184194
    b5 <- -0.0184999626
    b6 <- -0.4506571771
  } else if (sp%in%c("pine","p","furu","f","2")){

    b0 <- -0.9518643096
    b1 <- 1.9324863178
    b2 <- 3.0171238020
    b3 <- 1.6611366771
    b4 <- 0.1564753099
    b5 <- -0.0125847227
    b6 <- -0.0922072707

  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                                  "lauv","l","3")){
    b0 <- -2.8145935120
    b1 <- 2.0162402471
    b2 <- 1.7347110336
    b3 <- 2.6976118346
    b4 <- 0.0859727706
    b5 <- 0.0333428631
    b6 <- 0.3003342383

  } else{
    stop("sp must be in c(\"spruce\",\"pine\",\"birch\")")
  }

  b <- exp((b0 + b1 * (1 - (h/h_top))^b2 + b3 * (h/h_top)^(b4 * h_top) + b5 * (dbh) + b6 * h_top/dbh)) * d


  return(b)
}



