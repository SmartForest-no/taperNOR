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

hd_taperNO<-function(x,h,d,sp){
  sqrt(mean((d-taperNO(h=h,dbh=x[2],h_top=x[1],sp=sp))^2))
}

hfromd<-function(d,h,sp="spruce",output="H"){

  result<-optim(c(50,0.2),hd_taperNO,h=h,d=d,sp=sp)
  if(output=="H") {
    return(result$par[1])
  } else if(output=="all"){
    return(result)
  } else {
    return(result$par)
  }
}
