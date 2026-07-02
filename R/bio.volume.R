#' Biovolume calculation
#'
#' @description
#' This function calculates foraminifera biovolume based on geometric approximation.
#' To compute others organisms cell volume use \code{\link{volume.total}} function
#'
#' @param data a numeric vector or data frame with size data.
#' Size data parameters by model see \code{\link{volume.total}} details.
#' @param genus (optional) character informing foraminifera genus to calculate individual biovolume.
#' See all genera available in \code{\link{data_pco}}
#' @param pco (optional) vector informing percent of cell occupancy in the test.
#' Default value set for specific genus in \code{\link{data_pco}}.
#' @param model (optional if genus unknown) character informing geometric model to calculate volume.
#' See all models available in \code{\link{volume.total}}
#'
#' @return A `data.frame` or numeric object, consisting of calculated individual volume (if not available), biovolume and model (if \code{genus} is informed).
#'
#' @details
#' The function calculates the biovolume of different individuals from the available genera.
#'
#' @author Thaise Ricardo de Freitas \email{thaisericardo.freitas@@gmail.com}
#'
#' @seealso \code{\link{volume.total}}
#' @seealso \code{\link{biomass}}
#' @seealso \code{\link{measure}}
#' @importFrom magrittr %>%
#' @examples
#' # Calculate biovolume for different genera
#' #Ammonia size data
#' data("ammonia")
#'
#' bio.volume(ammonia, genus= "ammonia")
#'
#' # Calculate biovolume for unknown genus
#' df <- data.frame(h = 10, d_one = 10,
#' d_two = 10, area = 10, width = 10, length = 10)
#' bio.volume(df, model = "10hl", pco = 0.76)
#'
#' @export

bio.volume <- function(data, pco = NULL, genus = NULL, model = NULL){

  x <- data.frame(data)


  if ("genus" %in% colnames(x)) {
    genus <- x$genus
  }

  if ("model" %in% colnames(x) && is.null(genus)) {
    model <- x$model
  }

  if (is.null(genus) && is.null(model)) {
    stop("Please inform genus or model to be applied.")
  }

  d_pco <- forImage::data_pco

  # Resolve model from genus lookup
  if (is.null(model) && !is.null(genus)) {
    if (any(genus == d_pco$genera)) {
      x$model <- d_pco[match(genus, d_pco$genera), ]$model
    }
  }

  # Resolve pco: column in data takes priority, then genus lookup, then default = 0.76
  if ("pco" %in% colnames(x)) {
    pco <- x$pco
  } else if (!is.null(genus) && any(genus == d_pco$genera)) {
    pco <- d_pco[match(genus, d_pco$genera), ]$mean / 100
  } else if (is.null(pco)) {
    pco <- 0.76
  }



  v <- forImage::volume.total(x, model = model)$vol

  bv <- v * pco


  result <- x |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    tibble::add_column(vol = v, biovol = bv)


  return(result)
}






