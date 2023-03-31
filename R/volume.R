#' Calculate volume
#'
#' Calculates volume from taper model by integrating the taper function.
#'
#' @param dbh numeric vector of diameters at breast height, 1.3 m above ground (cm).
#' @param h_top numeric vector of tree heights above ground (m).
#' @param h_vol_lower,h_vol_upper numeric vectors of heights between stem volume is calculated (m). Default stump height (h_vol_lower) is 1% of h_top. NA in h_vol_upper is replaced by h_top.
#' @param sp species.
#' @param with_bark calculate volume over (TRUE, default) or under bark (FALSE).
#' @return Timber volume (m^3).
#' @examples
#' volume(20, 30)
#' volume(dbh=c(20,25,30), h_top=c(30,25,37))
#' volume(dbh=rep(25,11), h_top=rep(30,11),h_vol_lower=seq(0,30,3))
#' volume(dbh=rep(25,11), h_top=rep(30,11),h_vol_upper=seq(0,30,3))
#' volume(dbh=rep(25,11), h_top=rep(30,11),h_vol_upper=seq(0,30,3),with_bark=FALSE)
#' @export


volume<-function(dbh,h_top,h_vol_lower=NA,h_vol_upper=NA,sp="spruce",with_bark=TRUE){

  if(class(dbh)!="numeric"|class(h_top)!="numeric"|class(h_vol_lower)!="numeric"){
    stop("dbh, h_top and h_vol_lower must be numeric.")
  }

  if(length(dbh)!=length(h_top)) {
    stop("dbh and h_top must have the same length.")
  }

  if(length(h_vol_upper)==1) {
    h_vol_upper<-rep(h_vol_upper,length(dbh))
  } else if (length(h_vol_upper)!=length(dbh)){
    stop("h_vol_upper must be of length 1 of same length as dbh.")
  }

  #replace NA with h_top
  h_vol_upper[is.na(h_vol_upper)]<-h_top[is.na(h_vol_upper)]

  if(length(h_vol_lower)==1) {
      h_vol_lower<-rep(h_vol_lower,length(dbh))
  } else if (length(h_vol_lower)!=length(dbh)){
    stop("h_vol_lower must be of length 1 of same length as dbh.")
  }
  
  #replace NA with 0.1*h_top
  h_vol_lower[is.na(h_vol_lower)]<-0.1*h_top[is.na(h_vol_lower)]
  
  if(any(h_vol_lower>h_vol_upper)){
    stop("h_vol_lower must not be larger than h_vol_upper.")
  }

  if(length(sp)==1) {
    sp<-rep(sp,length(dbh))
  } else if (length(sp)!=length(dbh)){
    stop("sp must be of length 1 of same length as dbh.")
  }

  if(length(with_bark)==1) {
    with_bark<-rep(with_bark,length(dbh))
  } else if (length(with_bark)!=length(dbh)){
    stop("with_bark must be of length 1 of same length as dbh.")
  }


  taper_integr<-unlist(apply( data.frame(dbh,h_top,h_vol_lower,h_vol_upper,sp,with_bark),
                              MARGIN = 1,
                              FUN= function(x){
                                stats::integrate(function(h,dbh,h_top,sp,with_bark)((taperNOR(h,dbh,h_top,sp,with_bark)/100)/2)^2,
                                                 dbh=as.numeric(x[1]),
                                                 h_top=as.numeric(x[2]),
                                                 sp=x[5],
                                                 with_bark=as.logical(x[6]),
                                                 lower = as.numeric(x[3]),
                                                 upper = as.numeric(x[4]))$value
                              }
  ))



  return(pi*unname(taper_integr))
}


