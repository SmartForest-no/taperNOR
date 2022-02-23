#' Estimate height of diameters along the stem
#'
#' Estimates heights of given diameters using optimization of the taper function.
#'
#' @param dbh diameter at breast height (cm)
#' @param h_top height of diameter measurements (m)
#' @param d diameters (cm)
#' @param sp species
#' @return Timber volume (m^3).
#' @examples
#'
#' dlocation(dbh=35,h_top=35,d=c(25,40,12,1,70),sp="birch")
#'
#' D<-35
#' H<-34
#' T<-"G"
#'
#' #Case 1:
#' dia<-21
#' h_vol<-dlocation(d=dia,dbh=D,h_top=H,sp=T)
#' volume(D,H,sp=T,h_vol_lower = h_vol)
#'
#' #Case2:
#' dia<- c(21,16)
#' A<-1.2
#' h_vol<- dlocation(d=dia,dbh=D,h_top=H,sp=T)
#' if (diff(h_vol)>A) volume(D,H,sp=T,h_vol_lower = h_vol[1],h_vol_upper=h_vol[2])
#'
#' @export

dlocation<-function(dbh,h_top,d,sp="spruce"){

  st<-stats::predict(stats::lm(h~d,data=data.frame(h=c(1.3,h_top),d=c(dbh,0))),newdata = data.frame(d=d))
  st[st<0]<-0

  result<-apply(cbind(d,st),1,function(x)try(.dlocation_optim(x[2],x[1],dbh,h_top,sp)$par,T))





  return(round(result,2))

}



.dlocation_optim<-function(st,d_o,dbh_o,h_top_o,sp_o){
  stats::optim(par=st,
               fn= function(x_o,d_o,dbh_o,h_top_o,sp_o){
                 return(
                   mean(abs(
                     (d_o-
                        taperNO(
                          h=x_o,
                          dbh=dbh_o,
                          h_top=h_top_o,
                          sp=sp_o))
                   )
                   )
                 )
               },
               d_o=d_o,
               h_top_o=h_top_o,
               dbh_o=dbh_o,
               sp_o=sp_o,
               lower=0,
               upper=h_top_o,
               method="Brent")
}


