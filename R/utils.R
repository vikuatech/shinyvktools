#' utils
#'
#' @description
#' Utils
#'
#' @param file_name str
#' @param module_name str
#'
#' @noRd
name_download <- function(file_name, module_name) {
  file_ext <- stringr::str_remove(file_name, '.csv')
  print(file_ext)
  paste0(
    file_ext,
    module_name,
    floor(as.numeric(Sys.time())),
    ".csv"
  )
}
