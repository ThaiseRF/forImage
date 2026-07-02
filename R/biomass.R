#' Biomass estimative
#'
#' @description
#' The function estimates biomass through biovolume data and applies different cell density values as conversion methods.
#' See details \sQuote{Details}:
#'
#' @usage biomass(biovolume, method = "michaels")
#' @param biovolume numeric value, object or data.frame with cell living volume values in micrometers (µm³), as returned by \code{\link{bio.volume}}.
#' @param method The conversion method from biovolume to biomass. Default is \code{'michaels'}. Available options:
#' \itemize{
#'   \item \code{'saidova'} : wet mass density of 1.027 g/cm³;
#'   \item \code{'strathmann'} : carbon:volume ratio of 0.110 pgC[org]/µm³;
#'   \item \code{'turley'} : carbon:volume ratio of 0.132 pgC[org]/µm³;
#'   \item \code{'putt'} : carbon:volume ratio of 0.140 pgC[org]/µm³;
#'   \item \code{'gerlach'} : wet mass of 1.13 g/cm³ ;
#'   \item \code{'michaels'} : carbon:volume ratio of 0.089 pgC[org]/µm³.
#'
#'
#' }
#' @details For biomass estimates based on biovolume is usual the application of a cell density value, to retrieve the amount of organic carbon in the organism.
#' The function made available distinct options of conversion factor which are based in several authors.
#' These factors have been applied to a wide diversity of nano, micro, and macro-organisms, some applied to foraminifera and other groups.
#'
#' The Saidova and Gerlach factors are originally reported as wet mass densities (g/cm³). The soft body wet mass are converted to organic carbon equivalent by a 10:1 ratio.
#'
#'
#' @return An added column in a \code{data.frame} or a numeric object, consisting of calculated biomass in µgC[org]/individual.
#'
#' @author Thaise Ricardo de Freitas \email{thaisericardo.freitas@@gmail.com}
#' @references
#' \itemize{
#'   \item Saidova, K. (1966). The biomass and quantitative distribution of live foraminifera in the Kurile-Kamchatka trench area. \emph{DOKLADY AKAD. NAUK SSSR}, 174(1), 216–217.
#'   \item Strathmann, R. (1967). Estimating the Organic Carbon Content of Phytoplankton from Cell Volume or Plasma Volume. \emph{Limnology and Oceanography}, 12, 411–418. \emph{doi:10.4319/lo.1967.12.3.0411}
#'   \item Turley, C., Newell, R., & Robins, D. (1986). Survival Strategies of 2 Small Marine Ciliates and Their Role in Regulating Bacterial Community Structure Under Experimental Conditions. \emph{Marine Ecology Progress Series}, 33(1), 59–70. \emph{doi:10.3354/meps033059}
#'   \item Putt, M., & Stoecker, D. K. (1989). An experimentally determined carbon : volume ratio for marine ‘oligotrichous’ ciliates from estuarine and coastal waters. \emph{Limnology and Oceanography}, 34(6), 1097–1103. \emph{doi:10.4319/lo.1989.34.6.1097}
#'   \item Gerlach, S. A., Hahn, A., & Schrage, M. (1985). Size spectra of benthic biomass and metabolism . \emph{Marine Ecology Progress Series}, 26, 161–173. \emph{doi:10.3354/meps026161}
#'   \item Michaels, A. F., Caron, D. A., Swanberg, N. R., Howse, F. A., & Michaels, C. M. (1995). Planktonic sarcodines (Acantaria, Radiolaria, Foraminifera) in surface waters near Bermuda: abundance, biomass and vertical flux. \emph{Journal of Plankton Research}, 17(0), 131–163. \emph{doi:10.1093/plankt/17.1.131}
#' }
#'
#' @seealso \code{\link{bio.volume}}, \code{\link{volume.total}}
#' @importFrom magrittr %>%
#' @export
#' @examples
#' #Ammonia biomass calculation
#' data(ammonia)
#'
#' #calculate test volume and biovolume
#' df <- bio.volume(data = ammonia, genus = "ammonia")
#' df
#'
#' #calculate individual biomass with choosen method
#' res <- biomass(df, method = 'michaels')
#' res
#'
#'

biomass <- function(biovolume, method = "michaels"){

  x <- data.frame(biovolume)

  if (is.null(method)) {
    method <- "michaels"
  }

  # All factors are in pgC[org]/µm³.
  # Saidova: 1.027 g/cm³ wet mass × 10% OC = 0.1027 g OC/cm³
  #          1 g/cm³ = 1 pgC/µm³, so 0.1027 g/cm³ = 0.1027 pgC/µm³
  # Gerlach: 1.13 g/cm³ wet mass × 10% OC = 0.113 pgC/µm³
  # All others are measured or estimated carbon densities already in pgC/µm³.

  factors <- c(
    saidova    = 0.1027,
    strathmann = 0.110,
    gerlach    = 0.113,
    turley     = 0.132,
    putt       = 0.140,
    michaels   = 0.089
  )

  method <- tolower(method)

  if (!method %in% names(factors)) {
    stop(
      "Unknown method '", method, "'. Choose one of: ",
      paste(names(factors), collapse = ", "), "."
    )
  }

  # biomass = µm³ × pgC/µm³
  b <- x$biovol * factors[[method]]

  result <- x |>
    tibble::as_tibble() |>
    dplyr::mutate(biomass = b / 1e6) #pgC ÷ 1e6 = µgC

  return(result)

}

