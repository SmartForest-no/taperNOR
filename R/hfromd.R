#' Estimate tree height from diameters
#'
#' Estimates tree height from diameters using optimization of the taper function.
#'
#' @param d diameters in cm
#' @param h height of diameter measurements in m
#' @param sp species
#' @param output output from optimization: 'h' outputs the tree height,'d' diameter at breast height, 'all' result of optim.
#' @param grd_search if TRUE optimization is run on a matrix of initial parameters. The result of the best fit is returned. Might give better result at cost of performance.
#' @return Timber volume in m.
#' @examples
#'
#' hfromd(d=c(39,27),h=c(2,7),sp="birch")
#' @export
hfromd<-function(d,h,sp="spruce",output="h",grd_search=F){

  if(min(h)<=0.5){
    warning("Optimization is unstable with diameters at heights < 0.5 m. Observations are removed.")
    d<-d[h>=0.5]
    h<-h[h>=0.5]
  }

  st<-c(25,20)
  result<-try(.hfromd_optim(st,h,d,sp),T)


  if(class(result)=="try-error"){
    st<-
      c(
        stats::predict(stats::lm(d~h),newdata = data.frame(h=1.3)),
        stats::predict(stats::lm(h~d),newdata = data.frame(d=0))
      )
    result<-try(.hfromd_optim(st,h,d,sp),T)
  }

  if(class(result)=="try-error"|grd_search){
    st_df<-expand.grid(st_h=seq(1.5,45.5,4),st_d=seq(10,80,10))
    st_df$optim<-apply( st_df,1,function(x){try(.hfromd_optim(x,h,d,sp)$value,silent = T)})
    st_df$optim[!grepl("\\d",st_df$optim)]<-NA
    st_df$optim<-as.numeric(st_df$optim)
    result<-try(.hfromd_optim(st_df[which.min(st_df$optim),1:2]))
  }

  if(class(result)=="try-error") return(.hfromd_optim(st,h,d,sp))

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



.hfromd_optim<-function(st,h_o,d_o,sp_o){
  stats::optim(par=st,
               fn= function(x_o,h_o,d_o,sp_o){
                 return(
                   mean(abs(
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
               h_o=h_o,
               d_o=d_o,
               sp_o=sp_o)
}


