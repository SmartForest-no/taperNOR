#' Taper model for spruce, pine, and birch for Norway with dynamic diameter input
#'
#' Allows the use of multiple diameters measured at various points along the stem to estimate taper.
#'
#' @param Hx height where to return diameters
#' @param Hm height of measured diameters
#' @param Dm measured diameters
#' @param mHt measured tree height
#' @param sp species
#' @return Timber volume in m.
#' @export

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
