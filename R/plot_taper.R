#' Plot taper curve
#'
#' Plots the taper curve
#'
#' @param dbh diameter at breast height (cm).
#' @param h_top tree height (m).
#' @param sp species
#' @examples
#' plot_taper(33,30)
#' plot_taper(dbh=c(33,20,18),h_top=c(30,25,20),sp=c(1,1,3))
#' plot_taper(dbh=rep(25,3),h_top=rep(27,3),sp=1:3)
#' @export


plot_taper<-function(dbh,h_top,sp="spruce"){

  sp<-tolower(as.character(sp))
  sp<-ifelse(sp%in%c("spruce","s","gran","g","1"),"spruce",
             ifelse(sp%in%c("pine","p","furu","f","2"),"pine",
                    ifelse(sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                                   "lauv","l","3"),"birch",sp)))
  sp_num<-ifelse(sp=="spruce",1,
                 ifelse(sp=="pine",2,
                        ifelse(sp=="birch",3,NA)))

  if(length(dbh)!=length(h_top)){
    stop("dbh and h_top must have same length")
  }

  if(length(sp)>1&length(sp)!=length(dbh)){
    stop("sp must have length 1 or same length as dbh and h_top.")
  }
  if(length(sp)==1){
    sp<-rep(sp,length(dbh))
  }

  plotmat<-cbind(dbh,h_top,sp_num,1:length(dbh))

  plot(1,type="n",
       xlim=c(0,max(plotmat[,2])),
       ylim=c(0,
              taperNO(0,
                      plotmat[which.max(plotmat[,1]),1],
                      plotmat[which.max(plotmat[,1]),2],
                      plotmat[which.max(plotmat[,1]),3])),
       xlab="h (m)",
       ylab="d (cm)")

  apply(plotmat, 1, function(x){
    graphics::points((0:(x[2]*10))/10,
           taperNO((0:(x[2]*10))/10,x[1],x[2],x[3]),
           type="l",
           lty=x[3]
    )

    graphics::points(1.3,taperNO(1.3,x[1],x[2],x[3]),pch=16)
    if(length(dbh)>1){
      graphics::text(1.3,taperNO(1.3,x[1],x[2],x[3]),label=as.character(x[4]),adj=c(-0.5,-0.5))
      }
    graphics::points(x[2],0,pch=16)
    if(length(dbh)>1){
      graphics::text(x[2],0,label=as.character(x[4]),adj=c(0,-1))
      }



  })

  if(length(dbh)==1){
    graphics::mtext(paste0("DBH= ",round(dbh,2),
                 " cm; H= ",round(h_top,2),
                 " m","; species= ",sp),
          adj=0,line=0)
  } else {
    graphics::legend("topright",legend = c("spruce","pine","birch"),lty=1:3)
  }
}


