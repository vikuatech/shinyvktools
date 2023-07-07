#' example_dataset UI Function
#'
#' @description
#' A shiny Module consisting of an actionBttn that displays a markdown example_text and example_dataset
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param example_text str. in markdown format explainning what columns does the dataset need
#' @param example_dataset str. name of the .csv file to be read and display via DT::datatable
#' @param package_name str. name of the shiny-package
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_example_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::actionBttn(
      ns('example'),
      label = 'Example Dataset',
      icon = fontawesome::fa_i('circle-info'),
      size = 'sm',
      color = 'primary'
    )
  )
}

#' example_dataset Server Functions
#'
#' @noRd
mod_example_dataset_server <- function(id, example_text, example_dataset, package_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(
      input$example,
      {

        example_dataset_ <- system.file('extdata', example_dataset, package = package_name) %>%
          readr::read_csv(show_col_types = F)

        output$example_table <- DT::renderDataTable({

          example_dataset_ %>%
            head(10) %>%
            DT::datatable(
              rownames = F,
              options = list(
                ordering = F,
                searching = F,
                paging = F,
                info = F
              )
            )

        })

        output$download_example <- downloadHandler(
          filename = example_dataset,
          content = function(file) {
            example_dataset_ %>%
              readr::write_csv(file)
          })

        showModal(
          modalDialog(

            fluidRow(
              column(12, markdown(example_text))
            ),
            br(),
            fluidRow(
              column(12, DT::dataTableOutput(ns('example_table')))
            ),
            br(),
            markdown('Other dataset columns will be omited at evaluation, but will be included at result download'),

            title = "Example Dataset",
            size = 'xl',
            footer = shinyWidgets::downloadBttn(ns('download_example'), 'Download example dataset'),
            easyClose = TRUE
          )
        )
      }
    )

  })
}

## To be copied in the UI
# mod_example_dataset_ui("example_dataset_1")

## To be copied in the server
# mod_example_dataset_server("example_dataset_1")
