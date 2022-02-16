#' Estimate tree height from diameters
#'
#' Estimates tree height from diameters using optimization of the taper function.
#'
#' @param d diameters in cm
#' @param h height of diameter measurements in m
#' @param sp species
#' @param output output from optimization: 'h' outputs the tree height,'d' diameter at breast height, 'all' result of optim.
#' @return Timber volume in m.
#' @examples
#'
#' hfromd(d=c(39,27),h=c(2,7),sp="birch")
#' @export

hfromd<-function(d,h,sp="spruce",output="h"){

  if(min(h)<=0.5){
    warning("Optimization is unstable with diameters at heights < 0.5 m. Observations are removed.")
    d<-d[h>=0.5]
    h<-h[h>=0.5]
  }

  #estimate starting values
  if(length(d)>=2){
  st<-
    c(
      predict(lm(d~h),newdata = data.frame(h=1.3)),
      predict(lm(h~d),newdata = data.frame(d=0))
    )
  } else {
    st<-c(25,20)
  }

  result<-
    stats::optim(par=st,
                 fn= function(x_o,h_o,d_o,sp_o){
                   return(
                     abs(
                       mean(
                         (d_o-
                            taperNO(
                              h=h_o,
                              dbh=x_o[2],
                              h_top=x_o[1],
                              sp=sp_o))
                       )
                     )
                   )
                 },
                 h_o=h,
                 d_o=d,
                 sp_o=sp#,
                 # method = "L-BFGS-B" ,lower=c(1.3,5),upper=c(50,150)
    )
  names(result$par)<-c("h","d")

  if(output=="h") {
    return(result$par[1])
  } else if(output=="d"){
    return(result$par[2])
  } else if(output=="all"){
    return(result)
  } else {
    return(result$par)
  }
}
