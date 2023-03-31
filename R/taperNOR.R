#' Taper model for spruce, pine, and birch for Norway
#'
#' The taper model is based on Kozak A. 1988. A variable-exponent taper equation. Can J For Res. 18(11):1363–1368.
#'
#' @param h heights above ground where to return diameters (m).
#' @param dbh diameter at breast height (1.3 m above ground) over bark (cm).
#' @param h_top tree height above ground (m).
#' @param sp species ('spruce','pine' or 'birch'; 1:3).
#' @param with_bark estimate diameter over (TRUE, default) or under bark (FALSE)
#' @return diameters at h (cm).
#' @examples
#' taperNOR(h=1:30,dbh=20,h_top=30,sp="pine",with_bark=TRUE)
#' taperNOR(h=1:30,dbh=20,h_top=30,sp="pine",with_bark=FALSE)
#' @export


taperNOR <- function(h,dbh,h_top,sp="spruce",with_bark=TRUE){


  if(sum(!c(class(h),class(dbh),class(h_top))%in%c("numeric","integer"))>0){
    stop("h, dbh, and h_top must be numeric.")
  }

  sp<-tolower(as.character(sp))

  if(sp%in%c("spruce","s","gran","g","1")){
    b1 <-  1.0442724  
    b2 <-  0.9607360  
    b3 <-  0.9980403  
    b4 <-  2.1636914 
    b5 <- -0.4873805  
    b6 <-  3.7219547 
    b7 <- -2.0015002  
    b8 <-  0.1728971  
    p  <-  0.2294706 
  } else if (sp%in%c("pine","p","furu","f","2")){
    b1 <-  0.91123006  
    b2 <-  0.90083073  
    b3 <-  1.00007881  
    b4 <-  0.43370086 
    b5 <- -0.10404003
    b6 <-  0.28934664 
    b7 <- -0.02398272  
    b8 <-  0.07465469 
    p  <-  0.51986140 
  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                    "lauv","l","3")){
    b1 <-  0.8903574  
    b2 <-  0.9914279  
    b3 <-  0.9934046  
    b4 <-  0.9666948 
    b5 <- -0.1884422  
    b6 <-  0.9323787 
    b7 <- -0.4233551  
    b8 <-  0.1741121 
    p  <-  0.3379576 
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
    b<-barkNOR(d = d,h = h,dbh = dbh,h_top = h_top,sp = sp)
    d_ub<-d-b/10
    d_ub[d_ub<0]<-0
    return(d_ub)
  }


}

