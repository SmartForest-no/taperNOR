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



volume<-function(dbh,h_top){

  if(length(dbh)!=length(h_top)) {
    error("dbh and h_top must have the same length!")
  }

  taper_integr<-unlist(apply( cbind(dbh,h_top),
         MARGIN = 1,
         FUN= function(x){
           integrate(function(h,dbh,h_top)(taperNO(h,dbh,h_top)/2)^2,
                     dbh=x[1],
                     h_top=x[2],
                     lower = 0,
                     upper = x[2])$value
         }
  ))

  return(p*taper_integr)
}
