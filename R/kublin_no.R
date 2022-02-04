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

kublin_no <- function(Hx,Hm,Dm,mHt,sp=1) {

  if(sp==1){
    par_lme<-kublin_par_lme_spruce
  } else if(sp==2){
    par_lme<-kublin_par_lme_spruce
  } else if(sp==3){
    par_lme<-kublin_par_lme_spruce
  }
    TapeR::E_DHx_HmDm_HT.f(Hx = Hx,Hm = Hm,Dm = Dm,mHt,par.lme = par_lme)

}
