#' Estimate tree height from diameters
#'
#' Estimates tree height from diameters using optimization of the taper function.
#'
#' @param d diameters
#' @param h height of diameters
#' @param sp species
#' @param output output from optimization: 'H' outputs the tree height, 'all' result of optim.
#' @return Timber volume in m.
#' @examples
#'
#' hfromd(d=c(39,27),h=c(2,7),sp="birch")
#' @export

hfromd<-function(d,h,sp="spruce",output="H"){

  result<-
    stats::optim(par=c(50,20),
                 fn= function(x_o,h_o,d_o,sp_o){
                   sqrt(mean((d_o-taperNO(h=h_o,dbh=x_o[2],h_top=x_o[1],sp=sp_o))^2))
                 },
                 h_o=h,
                 d_o=d,
                 sp_o=sp)

  if(output=="H") {
    return(result$par[1])
  } else if(output=="all"){
    return(result)
  } else {
    return(result$par)
  }
}
