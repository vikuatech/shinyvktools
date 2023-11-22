#' utils
#'
#' @description
#' Utils
#'
#' @param file_name str
#' @param module_name str
#' @param width passed to bs4Dash::column
#' @param title str Title for the output
#' @param button button widget to show next to title
#' @param ... output to show in column
#' @param output_vector output of shinyWidget::AirDatePicker
#'
#' @export
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

#' @export
#' @rdname name_download
column_card <- function(width, title, ...){
  bs4Dash::column(
    width,
    shiny::h3(title),
    div( class = 'card', ... )
  )
}

#' @export
#' @rdname name_download
column_card_button <- function(width, title, button, ...){
  bs4Dash::column(
    width,
    fluidRow(
      shiny::h3(title),
      button
    ),
    div( class = 'card', ... )
  )
}

#' @export
#' @rdname name_download
expand_air_month_picker_output <- function(output_vector){
  if(length(output_vector) == 1){
    position_ <- 1
  } else{
    position_ <- 2
  }

  left_selected <- output_vector[1]
  right_selected <- trunc(output_vector[position_]+33, 'month')-1

  return(c(left_selected, right_selected))

}
