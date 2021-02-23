#' Open a pandan sheet
#'
#' @inheritParams pandan_dat
#'
#' @export

pandan_tracker <- function(gs_url = Sys.getenv("PANDAN_MS")) {
  googlesheets4::gs4_browse(gs_url)
}
