# Pure helper functions that do not depend on shiny/rgdal/raster/sp.
# These can be tested in isolation without heavy geospatial dependencies.

#' Check whether all population values in a spatial layer are identical.
#'
#' When all census sections have the same population count, the map
#' should render a uniform yellow instead of a green-to-red gradient.
#'
#' @param capa_sp A spatial object whose `@data` slot contains a
#'   `numPoblacionElegida` column (the selected population count
#'   per census section).
#' @return `TRUE` if max - min == 0 (i.e. all values are equal),
#'   `FALSE` otherwise.
samePopulationPrintYellow <- function(capa_sp) {
  values <- capa_sp@data$numPoblacionElegida
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
