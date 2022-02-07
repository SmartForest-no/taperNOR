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
#' volume(20, 30)
#' volume(c(20,25,30), c(30,25,37))

hfromd<-function(d,h,sp="spruce",output="H"){

  result<-stats::optim(c(50,0.2),
                function(x,h,d,sp){
                  sqrt(mean((d-taperNO(h=h,dbh=x[2],h_top=x[1],sp=sp))^2))
                },
                h=h,
                d=d,
                sp=sp)

  if(output=="H") {
    return(result$par[1])
  } else if(output=="all"){
    return(result)
  } else {
    return(result$par)
  }
}
