#' publish assert
#'
#' @description
#' Publish an assert error message in a shinyWidgets::show_alert module
#'
#' @param assert_report final expression returned by a assertr::chain_end
#' @param show_alert boolean. use shinyWidgets::show_alert with the error message
#' @param session Internal parameters for {shiny}
#' @param df dataframe
#'
#' @noRd
publish_assert_vk <- function(assert_report, show_alert = F, session = NULL){

  # If error exist
  if(is.list(assert_report)){

    error_description <- assert_report %>% purrr::map(~.x$description) %>% sprintf('**%s.**', .)

    error_index <- assert_report %>%
      purrr::map(
        ~.x$error_df %>%
          dplyr::filter(verb == 'assert') %>%
          head(10) %>%
          dplyr::pull(index) %>%
          paste0(collapse = ', ') %>%
          paste0(' rows: ', ., ', ...\n')
      )

    error_column <- assert_report %>%
      purrr::map(
        ~.x$error_df %>%
          dplyr::distinct(column) %>%
          dplyr::pull() %>%
          paste0(' column: ', .)
      )

    assert_result <- list(
      is_defect = T,
      error_message = paste0(error_description, error_column, error_index, collapse = '- ') %>%
        stringr::str_remove(' column: NA rows: , ...') %>%
        sprintf('- %s', .)
    )

  }
  # Else Report success
  else{
    assert_result <- list(is_defect = F, error_message = NULL)
  }

  # Show Alert in shiny
  if(assert_result$is_defect & show_alert){
    shinyWidgets::show_alert(
      session = session,
      title = "Error uploading dataset!!",
      text = shiny::markdown(assert_result$error_message),
      type = "error",
      html = T,
      btn_labels = NA,
      showCloseButton = T
    )
  }

  return(assert_result)

}

#'
#' @noRd
assert_geocode_dataset_vk <- function(df){
  df %>%
    assertr::chain_start(store_success=TRUE) %>%
    assertr::verify(assertr::has_all_names("address"), description = 'Missing mandatory columns') %>%
    assertr::verify(!assertr::has_all_names("row_id"), description = 'Restricted column name: row_id') %>%
    assertr::assert(assertr::not_na, tidyselect::any_of('address'), description = 'Missing values on address column') %>%
    assertr::assert(is.character, tidyselect::any_of('address'), description = 'address column is not character') %>%
    assertr::assert(is.numeric, tidyselect::any_of(c('lng', 'lat')), description = 'lat or lng columns no numeric') %>%
    assertr::assert(assertr::within_bounds(-180, 180), tidyselect::any_of('lng'), description = 'Wrong lng value') %>%
    assertr::assert(assertr::within_bounds(-85.05113, 85.05113), tidyselect::any_of('lat'), description = 'Wrong lat value') %>%
    assertr::chain_end(success_fun = assertr::success_logical, error_fun = assertr::error_return)
}
