#' Calculate volume
#'
#' Calculate volume from taper model by integerating the taper function.
#'
#' @param dbh numeric vector of diameters at breast height (cm).
#' @param h_top numeric vector of tree heights (m).
#' @param sp species
#' @return Timber volume (m).
#' @examples
#' volume(20, 30)
#' volume(c(20,25,30), c(30,25,37))
#' @export


volume<-function(dbh,h_top,sp="spruce"){

  if(class(dbh)!="numeric"|class(h_top)!="numeric"){
    stop("dbh and h_top must be numeric.")
  }

  if(length(dbh)!=length(h_top)) {
    stop("dbh and h_top must have the same length.")
  }

  if(length(sp)==1) {
    sp<-rep(sp,length(dbh))
  } else if (length(sp)!=length(dbh)){
    stop("sp must be of length 1 of same length as dbh.")
  }



  taper_integr<-unlist(apply( data.frame(dbh,h_top,sp),
         MARGIN = 1,
         FUN= function(x){
           stats::integrate(function(h,dbh,h_top,sp)(taperNO(h,dbh,h_top,sp)/2)^2,
                     dbh=as.numeric(x[1]),
                     h_top=as.numeric(x[2]),
                     sp=x[3],
                     lower = 0,
                     upper = as.numeric(x[2]))$value
         }
  ))


  return(pi*taper_integr)
}


