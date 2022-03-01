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


}


