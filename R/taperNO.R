# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

taperNO <- function(h,dbh,h_top,sp="spruce"){

  if(as.character(sp)%in%c("spruce","s","gran","g","1")){
    b1 <-  1.0329481
    b2 <-  0.9573736
    b3 <-  0.9981911
    b4 <-  1.7415408
    b5 <- -0.37813666
    b6 <-  2.9698804
    b7 <- -1.57204939
    b8 <-  0.18871109
    p  <-  0.2538893
  } else if (as.character(sp)%in%c("pine","p","furu","f","2")){
    b1 <-  0.9119251
    b2 <-  0.8646666
    b3 <-  1.0018507
    b4 <-  0.3266430
    b5 <- -0.08145126
    b6 <-  0.2050435
    b7 <-  0.05112187
    b8 <-  0.07639823
    p  <-  0.5737896
  }else if (as.character(sp)%in%c("birch","b","bjork","bj",
                                  "lauv","l","3")){
    b1 <-  1.0667671
    b2 <-  0.8741271
    b3 <-  1.0007140
    b4 <-  0.8204556
    b5 <- -0.14835312
    b6 <-  0.7178395
    b7 <- -0.28814575
    b8 <-  0.17402111
    p  <-  0.3540946
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


  return(d)
}

taperNO(1:10,30,30)

