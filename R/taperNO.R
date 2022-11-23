#' Taper model for spruce, pine, and birch for Norway
#'
#' The taper model is based on Kozak A. 1988. A variable-exponent taper equation. Can J For Res. 18(11):1363–1368.
#'
#' @param h heights above ground where to return diameters (m).
#' @param dbh diameter at breast height over bark (cm).
#' @param h_top tree height (m) above ground.
#' @param sp species ('spruce','pine' or 'birch'; 1:3).
#' @param with_bark estimate diameter over (TRUE, default) or under bark (FALSE)
#' @return diameters at h (cm).
#' @examples
#' taperNO(h=1:30,dbh=20,h_top=30,sp="pine",with_bark=TRUE)
#' taperNO(h=1:30,dbh=20,h_top=30,sp="pine",with_bark=FALSE)
#' @export


taperNO <- function(h,dbh,h_top,sp="spruce",with_bark=TRUE){


  if(sum(!c(class(h),class(dbh),class(h_top))%in%c("numeric","integer"))>0){
    stop("h, dbh, and h_top must be numeric.")
  }

  sp<-tolower(as.character(sp))

  if(sp%in%c("spruce","s","gran","g","1")){
    b1 <-  1.4768600
    b2 <-  0.9285894
    b3 <-  1.0003802
    b4 <-  2.8243594
    b5 <- -0.88530667
    b6 <-  4.4725887
    b7 <- -2.49228505
    b8 <-  0.10127257
    p  <-  0.01265285
  } else if (sp%in%c("pine","p","furu","f","2")){
    b1 <-  0.9119251
    b2 <-  0.8646666
    b3 <-  1.0018507
    b4 <-  0.3266430
    b5 <- -0.08145126
    b6 <-  0.2050435
    b7 <-  0.05112187
    b8 <-  0.07639823
    p  <-  0.57378960
  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                    "lauv","l","3")){
    b1 <-  1.0262998
    b2 <-  0.9691802
    b3 <-  0.9952169
    b4 <-  0.8511005
    b5 <- -0.14380835
    b6 <-  0.4230682
    b7 <- -0.21312855
    b8 <-  0.20224229
    p  <-  0.20276079
  } else{
    stop("sp must be in c(\"spruce\",\"pine\",\"birch\")")
  }


  d <-
    b1 *
    dbh^b2 *
    b3^dbh *
    ((1 - sqrt(h/h_top))/(1 - sqrt(p)))^
    (b4 * (h/h_top)^2 +
       b5 * log((h/h_top) + 0.001) +
       b6 * sqrt(h/h_top) +
       b7 * exp(1)^(h/h_top) +
       b8 * (dbh/h_top))


  if(with_bark){
    return(d)
  } else {
    b<-barkNO(d = d,h = h,dbh = dbh,h_top = h_top,sp = sp)
    d_ub<-d-b/10
    d_ub[d_ub<0]<-0
    return(d_ub)
  }


}

