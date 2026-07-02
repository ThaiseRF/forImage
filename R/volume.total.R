#' Volumetric calculation
#'
#' @description The function calculates foraminifera test volume or cell volume of similar organisms based on geometric approximation.
#'
#'
#' @param data data frame containing size data. Size data parameters may vary according to chosen model, see Details.
#' @param model geometric model to calculate volume, the models options are listed below:
#' \itemize{
#'   \item \code{'1hl'} : sphere
#'   \item \code{'2sl'} : half-sphere
#'   \item \code{'3hl'} : prolate spheroid
#'   \item \code{'4hl'} : cone or double cone
#'   \item \code{'6fs'} : paraboloid
#'   \item \code{'7fs'} : dome
#'   \item \code{'8hl'} : cylinder
#'   \item \code{'10hl'} : ellipsoid
#'   \item \code{'11fs'} : elliptic cone
#'   \item \code{'12v'} : cone + half ellipsoid
#'   \item \code{'13hlsl'} : gomphonemoid
#'   \item \code{'14hl'} : prism on elliptic base/elliptic cylinder
#'   \item \code{'15hl'} : half elliptic prism
#'   \item \code{'17fs'} : triangular dypyramid
#'   \item \code{'ahx'} : area x height
#'
#' }
#' @param ... other parameters.
#' @details These geometric models applied in this function are based and adapted from microalgae models developed by Hillebrand et al. (1999) - \code{('.hl')}, Sun and Liu (2003) - \code{('.sl')} and Vadrucci, Cabrini and Basset (2007) - \code{('.v')}, plus other adapted models to benthic foraminifera \code{('.fs')}.
#' The models can be a variable in \code{data} if specified as \code{model}.The size data parameters should follow the specified measures determined by each model, where \eqn{d_one} is minor diameter, \eqn{d_two} is major diameter and \eqn{h} is height.
#' \tabular{ll}{
#'   \code{'1hl'}
#'   \tab \eqn{V = (pi * (d_one^3))/6}
#'   \cr
#'   \code{'2sl'}
#'   \tab \eqn{V = (pi * (d_one^3))/12}
#'   \cr
#'   \code{'3hl'}
#'   \tab \eqn{V = (pi * h * (d_one^2))/6}
#'   \cr
#'   \code{'4hl'}
#'   \tab \eqn{V = (pi * h * (d_one^2))/12}
#'   \cr
#'   \code{'6fs'}
#'   \tab \eqn{V = (pi * hx * (d_one^2))/8}
#'   \cr
#'   \tab where \eqn{hx} is a function of test height for trochamminids.
#'   \cr
#'   \code{'7fs'}
#'   \tab \eqn{V = (pi * h * (4 * (h^2) + 3 * (d_one^2)))/24}
#'   \cr
#'   \code{'8hl'}
#'   \tab \eqn{V = (pi * h * (d_one^2))/4}
#'   \cr
#'   \code{'10hl'}
#'   \tab \eqn{V = (pi * h * d_one * d_two)/6}
#'   \cr
#'   \code{'11fs'}
#'   \tab \eqn{V = (pi * h * d_one * d_two)/12}
#'   \cr
#'   \code{'12v'}
#'   \tab \eqn{V = (pi * h * d_one * d_two)/12}
#'   \cr
#'   \code{'13hlsl'}
#'   \tab \eqn{V = ((d_one * d_two)/4) * (d_one + ((pi/4) - 1) * d_two) * asin(h/(2*d_one))}
#'   \cr
#'   \code{'14hl'}
#'   \tab \eqn{V = (pi * h * d_one * d_two)/4}
#'   \cr
#'   \code{'15hl'}
#'   \tab \eqn{V = pi * h * d_one * d_two)/4}
#'   \cr
#'   \code{'17fs'}
#'   \tab \eqn{V = ((length * width)/2) * h)/3}
#'   \cr
#'
#' }
#'
#' @return A `data.frame` or numeric object, consisting of calculated individual total volume.
#' @author Thaise Ricardo de Freitas \email{thaisericardo.freitas@@gmail.com}
#' @references
#' \itemize{
#'   \item Hillebrand, H., Dürselen, C.D., Kirschtel, D., Pollingher, U., & Zohary, T. (1999). Biovolume calculation for pelagic and benthic microalgae. \emph{Journal of Phycology}, 35(2), 403–424. \emph{doi:10.1046/j.1529-8817.1999.3520403.x}
#'   \item Sun, J., & Liu, D. (2003). Geometric models for calculating cell biovolume and surface area for phytoplankton. \emph{Journal of Plankton Research}, 25(11), 1331–1346. \emph{doi:10.1093/plankt/fbg096}
#'   \item Vadrucci, M. R., Cabrini, M., & Basset, A. (2007). Biovolume determination of phytoplankton guilds in transitional water ecosystems of Mediterranean Ecoregion. \emph{Transitional Waters Bulletin}, 2, 83–102. \emph{doi:10.1285/i1825229Xv1n2p83}
#'   \item Ricardo de Freitas, T., Bacalhau, E. T., & Disaró, S. T. (2021). Biovolume method for foraminiferal biomass assessment: evaluation of geometric models and incorporation of species mean cell occupancy. \emph{J. Foramin. Res.} 51, 249–266. \emph{doi: 10.2113/gsjfr.51.4.249}

#' }
#'
#' @seealso \code{\link{measure}}
#' @seealso \code{\link{bio.volume}}
#' @importFrom magrittr %>%
#' @examples
#' #Ammonia size data
#' data("ammonia")
#'
#' #calculate test volume
#' volume.total(ammonia, model = "10hl")
#'
#'
#' @export volume.total
#' @rdname volume

volume.total <- function(data, model, ...) {

  x <- data.frame(data)

  if ("model" %in% colnames(x)) {
    model <- x$model
  }


  MODELS <- c("1hl", "2sl", "3hl", "4hl", "5hl",
             "6fs", "7fs","8hl", "10hl", "11fs",
             "12v", "13hlsl", "14hl", "15hl", "17fs", "axh")


  model <- match.arg(model, MODELS, several.ok = TRUE)


  # model dispatch table: each entry is a function of the row
  dispatch <- list(
    "1hl"   = function(r) sphere(r$d_one),
    "2sl"   = function(r) half_sphere(r$d_one),
    "3hl"   = function(r) spheroid(r$h, r$d_one),
    "4hl"   = function(r) cone(r$h, r$d_one),
    "5hl"   = function(r) cone(r$h, r$d_one),
    "6fs"   = function(r) paraboloid(r$h, r$d_one),
    "7fs"   = function(r) dome(r$h, r$d_one),
    "8hl"   = function(r) cylinder(r$h, r$d_one),
    "10hl"  = function(r) ellipsoid(r$h, r$d_one, r$d_two),
    "11fs"  = function(r) elliptic_cone(r$h, r$d_one, r$d_two),
    "12v"   = function(r) elliptic_cone(r$h, r$d_one, r$d_two),
    "13hlsl"= function(r) gomphonemoid(r$h, r$d_one, r$d_two),
    "14hl"  = function(r) elliptic_prism(r$h, r$d_one, r$d_two),
    "15hl"  = function(r) elliptic_prism(r$h, r$d_one, r$d_two),
    "17fs"  = function(r) dypyramid(r$h, r$length, r$width),
    "axh"   = function(r) axh(r$area, r$h)
  )

  vols <- vapply(seq_len(nrow(x)), function(i) {
                   m <- if (length(model) == 1) model else model[i]
                   if (!m %in% names(dispatch)) stop("Unknown model: ", m)
                   dispatch[[m]](x[i, ])
                 }, numeric(1))

  result <- tibble::as_tibble(x) |>
    dplyr::mutate(vol = vols)


  return(result)


}
