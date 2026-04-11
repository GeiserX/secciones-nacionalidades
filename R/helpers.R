# Pure helper functions that do not depend on shiny/rgdal/raster/sp.
# These can be tested in isolation without heavy geospatial dependencies.

#' Check whether all population values in a layer's data are identical.
#'
#' When all census sections have the same population count, the map
#' should render a uniform yellow instead of a green-to-red gradient.
#'
#' @param layer_data A data.frame containing a `numPoblacionElegida` column
#'   (the selected population count per census section). In production this
#'   is `capa_sp@data`; for testing a plain data.frame works.
#' @return `TRUE` if max - min == 0 (i.e. all values are equal),
#'   `FALSE` otherwise.
samePopulationPrintYellow <- function(layer_data) {
  values <- layer_data$numPoblacionElegida
  return(max(values, na.rm = TRUE) - min(values, na.rm = TRUE) == 0)
}

#' Format a municipality code to 5-digit zero-padded string.
#'
#' @param code A numeric or character municipality code.
#' @return A 5-character zero-padded string.
formatMunicipioCode <- function(code) {
  sprintf("%05d", as.integer(code))
}

#' Build a census section identifier from its components.
#'
#' @param cumun Character vector of municipality codes.
#' @param cdis Character vector of district codes.
#' @param csec Character vector of section codes.
#' @return Character vector of concatenated census section IDs.
buildSeccionCensal <- function(cumun, cdis, csec) {
  paste0(cumun, cdis, csec)
}
