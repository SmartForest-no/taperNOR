#' Bark model for spruce, pine, and birch for Norway
#'
#' The bark models are based on Gordon (1983) via Stängle (2016).
#'
#' @param d diameter (cm)
#' @param h height (m)
#' @param dbh diameter at breast height (cm)
#' @param h_top tree height (m)
#' @param sp species ('spruce','pine' or 'birch'; 1:3)
#' @return double bark thickness at h (mm).
#' @examples
#' barkNO(d=30, h=1,dbh=25,h_top=30,sp="pine")
#'
#' taper<-taperNO(h=(0:300)/10,dbh=25,h_top=30,sp="pine")
#' bark<-barkNO(d=taper, h=(0:300)/10,dbh=25,h_top=30,sp="pine")
#' plot((0:300)/10,taper,type="l",col="brown",xlab="height (m)",ylab="taper/bark (cm)")
#' points((0:300)/10,taper-bark/10,type="l",col="green")
#'
#' plot((0:300)/10,bark,type="l",ylab="bark (mm)",xlab="height (m)")
#' @export


barkNO <- function(d,h,dbh,h_top,sp="spruce"){


  if(sum(!c(class(h),class(dbh),class(h_top))%in%c("numeric","integer"))>0){
    stop("h, dbh, and h_top must be numeric.")
  }



  sp<-tolower(as.character(sp))

  if(sp%in%c("spruce","s","gran","g","1")){
    b0 <- -0.70911260129541
    b1 <- 1.20831705951688
    b2 <- -0.113439799574141
    b3 <- 0.842104936997459
    b4 <- 0.397802546279246
    b5 <- -0.0279026048780487
    b6 <- -0.59671629442811
  } else if (sp%in%c("pine","p","furu","f","2")){

    b0 <- -2.6412634598466
    b1 <- 2.90603155213358
    b2 <- 1.67889307028464
    b3 <- 2.90163357037273
    b4 <- 0.140155177285443
    b5 <- 0.00766910899470643
    b6 <- 0.208988128031329

  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                                  "lauv","l","3")){
    b0 <- -2.63050223461604
    b1 <- 1.91404321986589
    b2 <- 1.76245533266245
    b3 <- 2.64437359388603
    b4 <- 0.0799908337558318
    b5 <- 0.0269315945651629
    b6 <- 0.229504417813453

  } else{
    stop("sp must be in c(\"spruce\",\"pine\",\"birch\")")
  }

  b <- exp((b0 + b1 * (1 - (h/h_top))^b2 + b3 * (h/h_top)^(b4 *      h_top) + b5 * (dbh) + b6 * h_top/dbh)) * d


  return(b)
}



