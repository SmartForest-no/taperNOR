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
    b0 <- -5419.06685603865    
    b1 <- 5419.50997494304  
    b2 <- -0.00008890974        
    b3 <- -0.53038541438  
    b4 <- 0.02453226240        
    b5 <- -0.01886516439 
    b6 <- -0.45171471751 
  } else if (sp%in%c("pine","p","furu","f","2")){

    b0 <- -0.95978811  
    b1 <- 1.94131645  
    b2 <- 3.00687284  
    b3 <- 1.66494025 
    b4 <- 0.15516859 
    b5 <- -0.01257692
    b6 <- -0.09208307 

  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                                  "lauv","l","3")){
    b0 <- -2.77956885  
    b1 <- 2.00041120  
    b2 <- 1.75407493  
    b3 <- 2.67852784 
    b4 <- 0.08633534  
    b5 <- 0.03298938  
    b6 <- 0.29434451 

  } else{
    stop("sp must be in c(\"spruce\",\"pine\",\"birch\")")
  }

  b <- exp((b0 + b1 * (1 - (h/h_top))^b2 + b3 * (h/h_top)^(b4 * h_top) + b5 * (dbh) + b6 * h_top/dbh)) * d


  return(b)
}



