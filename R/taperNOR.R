#' Taper model for spruce, pine, and birch in Norway
#'
#' Computes stem diameters (taper) along the bole of spruce, pine, and birch in Norway.
#' The function implements the taper equations described in Hansen et al. (2023),
#' based on Kozak’s (1988) variable-exponent taper equation.
#' The original article by Hansen et al. (2023) contains minor errata
#' (e.g., missing brackets, the use of `Log` instead of `ln`, and a sign error at `b7`
#' in the pine taper model), but these do not affect the present implementation.
#'
#' @references
#' Hansen, E., Rahlf, J., Astrup, R., & Gobakken, T. (2023).
#' Taper, volume, and bark thickness models for spruce, pine, and birch in Norway.
#' \emph{Scandinavian Journal of Forest Research}, \doi{10.1080/02827581.2023.2243821}.
#'
#' Kozak, A. (1988). A variable-exponent taper equation.
#' \emph{Canadian Journal of Forest Research}, 18(11), 1363–1368.
#'
#' @param h A numeric vector of heights above ground (in meters) at which to return diameters.
#' @param dbh Diameter at breast height (1.3 m above ground), over bark (in centimeters).
#' @param h_top Total tree height (in meters).
#' @param sp Character or numeric value indicating the species; recognized inputs include:
#'   \itemize{
#'     \item \strong{spruce}: "spruce", "s", "gran", "g", "1"
#'     \item \strong{pine}: "pine", "p", "furu", "f", "2"
#'     \item \strong{birch}: "birch", "b", "bjørk", "bjork", "bj", "lauv", "l", "3"
#'   }
#'   Defaults to \code{"spruce"}.
#' @param with_bark Logical. If \code{TRUE} (the default), returns diameter over bark.
#'   If \code{FALSE}, diameter under bark is computed by subtracting bark thickness via
#'   \code{\link{barkNOR}}.
#'
#' @return A numeric vector of diameters (in centimeters) at each height in \code{h}.
#'
#' @seealso
#'   \code{\link{barkNOR}} for details on bark thickness subtraction.
#'
#' @examples
#' # Example usage:
#' # Calculate taper for pine, at heights 1:30 m, dbh = 20 cm, total height = 30 m:
#' taperNOR(h = 1:30, dbh = 20, h_top = 30, sp = "pine", with_bark = TRUE)
#'
#' # To get diameters under bark:
#' taperNOR(h = 1:30, dbh = 20, h_top = 30, sp = "pine", with_bark = FALSE)
#'
#' @export


taperNOR <- function(h,dbh,h_top,sp="spruce",with_bark=TRUE){


  if(sum(!c(class(h),class(dbh),class(h_top))%in%c("numeric","integer"))>0){
    stop("h, dbh, and h_top must be numeric.")
  }

  sp<-tolower(as.character(sp))

  if(sp%in%c("spruce","s","gran","g","1")){
    b1 <-  1.0625010
    b2 <-  0.9590684
    b3 <-  0.9982461
    b4 <-  2.2909135
    b5 <-  -0.5201230
    b6 <-  3.8808849
    b7 <-  -2.1078922
    b8 <-  0.1695809
  } else if (sp%in%c("pine","p","furu","f","2")){
    b1 <-  1.14798036
    b2 <-  0.90295964
    b3 <-  1.00118665
    b4 <-  0.24116857
    b5 <-  -0.09667025
    b6 <-  -0.50359177
    b7 <-  0.32132441
    b8 <-  0.05546691
  }else if (sp%in%c("birch","b","bj\u00f8rk","bjork","bj",
                    "lauv","l","3")){
    b1 <-  0.9810885
    b2 <-  0.9936293
    b3 <-  0.9941538
    b4 <-  0.8526987
    b5 <-  -0.1819791
    b6 <-  0.4687623
    b7 <-  -0.2198294
    b8 <-  0.1591102
  } else{
    stop("sp must be in c(\"spruce\",\"pine\",\"birch\")")
  }


  d <-
    b1 *
    dbh^b2 *
    b3^dbh *
    ((1 - sqrt(h/h_top))/(1 - sqrt(0.2)))^
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

