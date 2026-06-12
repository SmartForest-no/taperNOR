#' Taper model for spruce, pine, and birch for Norway with dynamic diameter input
#'
#' Based on Kublin et al. 2013. A flexible stem taper and volume prediction method based on mixed-effects B-spline regression. Eur J For Res. 132(5-6):983–997
#' and code in Kublin & Breidenbach (https://CRAN.R-project.org/package=TapeR).
#'
#' Allows the use of multiple diameters measured at various points along the stem to estimate taper.
#'
#' @param Hx height above ground where to return diameters (m)
#' @param Dx or diameters (cm) for which to return a height
#' @param Hm height above ground of measured diameters (m)
#' @param Dm measured diameters (cm)
#' @param mHt measured tree height (m) above ground
#' @param sp species. \strong{Only spruce is currently supported}
#'   ("spruce", "s", "gran", "g", "1"); the Kublin (TapeR) mixed-effects model
#'   has only been fitted for spruce in this package. Pine and birch raise an
#'   error - use \code{\link{taperNOR}} for those species, or supply a fitted
#'   \code{par.lme}.
#' @param ... parameters handed over to E_DHx_HmDm_HT.f or E_HDx_HmDm_HT.f
#' @return When Hx is given: diameters at Hx (cm). When Dx is given: heights where d=Dx (m).
#'   \code{TapeR::E_HDx_HmDm_HT.f()} only root-finds a single height per call, so when
#'   \code{Dx} has length > 1 it is called once per element of \code{Dx} and the
#'   resulting heights are combined into a vector of the same length as \code{Dx}.
#' @examples
#' # Spruce stem with diameters of 30 cm at 1.3 m and 22 cm at 5 m above
#' # ground, and a total height of 25 m. (Only spruce is currently supported.)
#'
#' # Height(s) (m) at which the stem reaches 25 and 15 cm in diameter:
#' kublin_nor(Dx = c(25, 15), Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = "spruce")
#'
#' # Diameters (cm) predicted at 1.3, 5 and 10 m above ground:
#' kublin_nor(Hx = c(1.3, 5, 10), Hm = c(1.3, 5), Dm = c(30, 22), mHt = 25, sp = "spruce")$DHx
#' @export

kublin_nor <- function(Hx=NULL,Hm,Dm,mHt,sp=1,Dx=NULL,...) {

  sp_norm<-tolower(as.character(sp))
  if(sp_norm%in%c("spruce","s","gran","g","1")){
    par_lme<-kublin_par_lme_spruce
  } else if(sp_norm%in%c("pine","p","furu","f","2",
                         "birch","b","bj\u00f8rk","bjork","bj","lauv","l","3")){
    stop("kublin_nor() currently only supports spruce. The Kublin (TapeR) model has not been fitted for pine or birch in this package; use taperNOR() for those species, or supply a fitted par.lme.")
  } else {
    stop("sp must indicate spruce, the only species currently supported by kublin_nor().")
  }
  if(is.null(Hx)&!is.null(Dx)){
    if(length(Dx)>1){
      vapply(Dx, function(Dx_i){
        TapeR::E_HDx_HmDm_HT.f(Dx = Dx_i,Hm = Hm,Dm = Dm,mHt,par.lme = par_lme,...)
      }, FUN.VALUE = numeric(1))
    } else {
      TapeR::E_HDx_HmDm_HT.f(Dx = Dx,Hm = Hm,Dm = Dm,mHt,par.lme = par_lme,...)
    }
  } else if ((!is.null(Hx))&is.null(Dx)){
    TapeR::E_DHx_HmDm_HT.f(Hx = Hx,Hm = Hm,Dm = Dm,mHt,par.lme = par_lme,...)
  } else {
    stop("Either Hx or Dx must be given")
  }

}
