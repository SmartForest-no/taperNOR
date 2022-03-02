#' Plot taper curve
#'
#' Plots the taper curve
#'
#' @param dbh diameter at breast height (cm).
#' @param h_top tree height (m).
#' @param sp species
#' @examples
#' plot_taper(33,30)
#' @export


plot_taper<-function(dbh,h_top,sp="spruce"){

  plot(
    (0:(h_top*10))/10,
    taperNO((0:(h_top*10))/10,dbh,h_top,sp),
    type="l",
    xlab="h (m)",
    ylab="d (cm)"
  )
  points(1.3,taperNO(1.3,dbh,h_top,sp),pch=16)
  mtext(paste0("DBH= ",round(dbh,2)," cm; H= ",round(h_top,2)," m"),adj=0,line=0)


}


