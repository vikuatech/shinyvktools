#' bq_get
#'
#' @description wrapper helpers to communicate with Bigquery
#'
#' @param query SQL query to execute
#' @param dataset str name of the dataset that will perform the job
#' @param location str name of the wkt column to convert to sf
#'
#' @return invisible.
#'
#' @export
bq_get <- function(query, dataset = NULL, location = NULL){

  res <- bigrquery::bq_project_query(dataset, query) %>%
    bigrquery::bq_table_download()

  if(!is.null(location)){

    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("sf required: install that first") # nocov
    }

    ret <- res %>%
      sf::st_as_sf(wkt = location, crs = 'WGS84')
  }
  else{
    ret <- tibble::as_tibble(res)
  }

  return(ret)
}
