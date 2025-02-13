#' Bark model for spruce, pine, and birch for Norway
#'
#' The bark models are based on Hannrup B. 2004. Funktioner för skattning av barkens tjocklek hos tall och gran vid avverkning med skördare. (Functions for prediction of bark thickness of Norway spruce and Scots pine at CTL-harvesting). Arbetsrapport 575. Skogforsk, Uppsala.
#' via Stängle et al. 2017. Comparison of models for estimating bark thickness of Picea abies in southwest Germany: the role of tree, stand, and environmental factors. Ann For Sci. 74(1):16.
#'
#' @param d diameter (cm).
#' @param h height above ground (m).
#' @param dbh diameter at breast height, 1.3 m above ground (cm).
#' @param h_top tree height above ground (m).
#' @param sp species ('spruce','pine' or 'birch'; 1:3)
#' @return double bark thickness (cm) at height h with diameter d.
#' @examples
#' barkNOR(d=30, h=1,dbh=25,h_top=30,sp="pine")
#'
#' taper<-taperNOR(h=(0:300)/10,dbh=25,h_top=30,sp="pine")
#' bark<-barkNOR(d=taper, h=(0:300)/10,dbh=25,h_top=30,sp="pine")
#' plot((0:300)/10,taper,type="l",col="brown",xlab="height (m)",ylab="taper/bark (cm)")
#' points((0:300)/10,taper-bark/10,type="l",col="green")
#' legend("topright",legend = c("taper","bark"),fill=c("brown","green"),border=NA)
#'
#' points((0:300)/10,bark,type="l",ylab="bark (cm)",xlab="height (m)")
#' @export


barkNOR <- function(d,h,dbh,h_top,sp="spruce"){


  if(sum(!c(class(h),class(dbh),class(h_top))%in%c("numeric","integer"))>0){
    stop("h, dbh, and h_top must be numeric.")
  }



  sp<-tolower(as.character(sp))

  if(sp%in%c("spruce","s","gran","g","1")){
    
    b0 <- 2.32391608    
    b1 <- 0.06830421  
    b2 <- 0.39936183
    
  } else if (sp%in%c("pine","p","furu","f","2")){

    b0 <- 2.9306985  
    b1 <- -0.4045155  
    b2 <- 1.2126856  

  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                                  "lauv","l","3")){
    
    b0 <- -0.48552367  
    b1 <- 0.04957885  
    b2 <- 0.84676248 

  } else{
    stop("sp must be in c(\"spruce\",\"pine\",\"birch\")")
  }

  b <- b0 + b1 * dbh + b2 * d


  return(b)
}



