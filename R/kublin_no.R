#' Taper model for spruce, pine, and birch for Norway with dynamic diameter input
#'
#' Allows the use of multiple diameters measured at various points along the stem to estimate taper.
#'
#' @param Hx height where to return diameters (m)
#' @param Hm height of measured diameters (m)
#' @param Dm measured diameters (cm)
#' @param mHt measured tree height (m)
#' @param sp species
#' @param ... parameters handed over to E_DHx_HmDm_HT.f or E_HDx_HmDm_HT.f
#' @return When Hx is given: diameters at Hx (cm). When Dx is given: heights where d=Dx (m).
#' @export

kublin_no <- function(Hx=NULL,Hm,Dm,mHt,sp=1,Dx=NULL,...) {

  if(sp==1){
    par_lme<-kublin_par_lme_spruce
  } else if(sp==2){
    par_lme<-kublin_par_lme_spruce
  } else if(sp==3){
    par_lme<-kublin_par_lme_spruce
  }
  if(is.null(Hx)&!is.null(Dx)){
    TapeR::E_HDx_HmDm_HT.f(Dx = Dx,Hm = Hm,Dm = Dm,mHt,par.lme = par_lme,...)
  } else if ((!is.null(Hx))&is.null(Dx)){
    TapeR::E_DHx_HmDm_HT.f(Hx = Hx,Hm = Hm,Dm = Dm,mHt,par.lme = par_lme,...)
  } else {
    stop("Either Hx or Dx must be given")
  }

}
