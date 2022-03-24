#' Bark model for spruce, pine, and birch for Norway
#'
#' The bark model is based on Gordon (1983) via Stängle (2016).
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

  d<-d*0.01
  dbh<-dbh*0.01


  sp<-tolower(as.character(sp))

  if(sp%in%c("spruce","s","gran","g","1")){
    b0 <- 97.9751241112519
    b1 <- 16.0930536754826
    b2 <- -0.369404168540033
    b3 <- 140.38588584026
    b4 <- 0.55313721751825
    b5 <- -167.735546192478
    b6 <- -0.255889265432384
  } else if (sp%in%c("pine","p","furu","f","2")){

    b0 <- -28.4644584730541
    b1 <- 197.642836269908
    b2 <- 3.32242243536864
    b3 <- 135.273697224011
    b4 <- 0.100154378165794
    b5 <- 26.5520186176326
    b6 <- 0.0552914705906784

  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                                  "lauv","l","3")){
    b0 <- -10.1271682597076
    b1 <- 95.8317340993314
    b2 <- 3.67940273452123
    b3 <- 131.744237520876
    b4 <- 0.0676981955803589
    b5 <- 103.747283634962
    b6 <- 0.0148335183306435

  } else{
    stop("sp must be in c(\"spruce\",\"pine\",\"birch\")")
  }

  b <- (b0 + b1 * (1 - (h / h_top))^ b2 + b3 * (h / h_top) ^ (b4 * h_top) + b5 * (dbh) + b6 * h_top/dbh) * d


  return(b)
}



