#' Estimate height of diameters along the stem
#'
#' Estimates heights of given diameters along the stem using optimization of the taper function. Height is distance from stem base.
#'
#' @param dbh diameter at breast height over bark (cm)
#' @param h_top height of diameter measurements (m)
#' @param d diameters (cm)
#' @param sp species
#' @param with_bark diameter with (TRUE, default) or without bark (FALSE).
#' @return Timber volume (m^3).
#' @examples
#'
#' dlocation(dbh=35,h_top=35,d=c(25,40,12,2,70),sp="birch")
#' dlocation(dbh=35,h_top=35,d=c(25,40,12,2,70),sp="birch",with_bark=FALSE)
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



dlocation<-function(dbh,h_top,d,sp="spruce",with_bark=TRUE){

  #estimate starting values
  st<-stats::predict(
    stats::lm(h~d,data=data.frame(h=c(1.3,h_top),d=c(dbh,0))),
    newdata = data.frame(d=d))
  st[st<0]<-0


  result<-apply(
    cbind(d,st),1,
    function(x)try(.dlocation_optim(st=x[2],
                                    d_o = x[1],
                                    dbh_o = dbh,
                                    h_top_o = h_top,
                                    sp_o = sp,
                                    with_bark_o = with_bark)$par)
  )

  return(round(result,2))

}


.dlocation_optim<-function(st,d_o,dbh_o,h_top_o,sp_o,with_bark_o){

  stats::optim(par=st,
               fn= function(x_o,d_o,dbh_o,h_top_o,sp_o,with_bark_o){
                 d_i<-taperNOR(
                   h=x_o,
                   dbh=dbh_o,
                   h_top=h_top_o,
                   sp=sp_o,
                   with_bark=with_bark_o)
                 min_measure<-mean(abs((d_o-d_i)))
                 return(min_measure)
               },
               d_o=d_o,
               h_top_o=h_top_o,
               dbh_o=dbh_o,
               sp_o=sp_o,
               with_bark_o= as.logical(with_bark_o),
               lower=0,
               upper=h_top_o,
               method="Brent")
}






