#' geocoding UI Function
#'
#' @description A shiny Module that allows to geocode addresses.
#' The user `fileInput`s a .csv file with the column *address* and press the bttn to geocode via geocode_apply.
#' A leaflet map displays with the marker locations.
#' If any address can`t be geocoded, a DT::datatable displays below the map
#' User can manually locate addresses or modify them in the map via mod_geocoding_manual or by dragging markers
#' User can download the same fileInput .csv with new columns lat, lng and place_id
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param example_dataset str. name of the .csv file to be read and display via DT::datatable
#' @param package_name str. name of the shiny-package
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geocoding_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        bs4Dash::column(
          2,
          shiny::fileInput(ns("upload_addresses"), NULL, accept = c(".csv"))
        ),
        bs4Dash::column(
          2,
          mod_example_dataset_ui(ns("example_geocode"))
        ),


        bs4Dash::column(
          3,
          shinyjs::hidden(
            shinyWidgets::actionBttn(
              ns('geocode'),
              label = 'Geocode',
              icon = fontawesome::fa_i('map-location-dot')
            )
          )
        ),
        bs4Dash::column(
          3,
          shinyjs::hidden(
            shinyWidgets::downloadBttn(ns('download_addresses_bttn'), 'Download Geocoded Addresses')
          )
        )

      ),

      br(),

      fluidRow(
        bs4Dash::column(
          12,
          leaflet::leafletOutput(ns('address_map'), height = 500)
        )
      ),

      br(),

      fluidRow(
        bs4Dash::column(
          12,
          DT::dataTableOutput(ns('missing_table') )
        )
      )
    )
  )
}

#' geocoding Server Functions
#'
#' @noRd
mod_geocoding_server <- function(id, example_dataset, package_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rvalues <- reactiveValues()
    rvalues$count=0

    # Issue with downloadBttn and shinyJS
    shinyjs::addCssClass(id = "download_addresses_bttn", class = "btn btn-xs")

    ############# Geocode #############

    # Read Uploaded Dataset
    observeEvent(input$upload_addresses, {

      directions <- readr::read_csv(input$upload_addresses$datapath,
                                    show_col_types = FALSE,
                                    col_types = list(address = 'c', lat = 'd', lng = 'd'))

      check_result <- directions %>%
        assert_geocode_dataset_vk() %>%
        publish_assert_vk(show_alert = T, session = session)

      validate(
        need(!check_result$is_defect, 'Please upload dataset')
      )

      rvalues$addresses <- directions %>%
        tibble::rownames_to_column('row_id')

      shinyjs::show('geocode')

    })

    # Action Bttn: Geocode
    observeEvent(input$geocode, {

      if(is.null(rvalues$addresses)){
        shinyWidgets::show_alert(
          title = "No dataset uploaded!",
          text = 'Please upload a dataset with "address" column to proceed',
          type = "warning"
        )

        return(NULL)
      }

      if(nrow(rvalues$addresses) > 500){

        shinyWidgets::show_alert(
          title = "Too many records to geocode!",
          text = 'This module use googlemaps API resources, please contact roserrano@vikua.com to geolocate more than 500 records',
          type = "error"
        )

        return(NULL)
      }

      waiter::waiter_show( # show the waiter
        html = waiter::spin_double_bounce() # use a spinner
      )

      rvalues$addresses_geocoded <- geocode_apply(rvalues$addresses)

      shinyjs::show('download_addresses_bttn')
      shinyjs::show('download_addresses_bttn_bttn')

      waiter::waiter_hide()

    })

    # Leaflet Map
    output$address_map <- leaflet::renderLeaflet({

      req(rvalues$addresses_geocoded)

      leaflet::leaflet(rvalues$addresses_geocoded) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
        leaflet::addLayersControl(
          baseGroups = c("CartoDB Positron", "Esri World Imagery")
        ) %>%
        leaflet::addMarkers(
          layerId = ~row_id,
          lng = ~lng,
          lat = ~lat,
          label = ~address,
          options = leaflet::markerOptions(draggable = TRUE)
        )

    })

    ############# Manually Geocode: Marker-Drag #############
    observeEvent(input$address_map_marker_dragend, {

      dragged_marker <- input$address_map_marker_dragend
      cat('Dragged stop', dragged_marker$id, '\n')

      rvalues$addresses_geocoded <- geocode_update(
        rvalues$addresses_geocoded,
        id = dragged_marker$id,
        lat = dragged_marker$lat,
        lng = dragged_marker$lng,
        place_id = NA_character_
      )

    })

    ############# Manually Geocode: Marker-Click #############

    # Modal PopUp
    observeEvent(
      input$address_map_marker_click, {

        cat('clicked stop ', input$address_map_marker_click$id, '\n')
        missing_address_selected_marker <- rvalues$addresses_geocoded %>% dplyr::filter(row_id == input$address_map_marker_click$id)

        # https://stackoverflow.com/questions/64504517/nested-modules-and-observeevents-r-shiny
        rvalues$count <- rvalues$count + 1
        mod_geocoding_manual_server(
          paste0('address_marker_click', rvalues$count),
          rvalues,
          missing_address_selected_marker
        )
        mod_geocoding_manual_ui(paste0(ns("address_marker_click"), rvalues$count))

      }
    )

    ############# Manually Geocode: Selected Table-Row #############

    missing_addresses <- reactive({
      req(rvalues$addresses_geocoded)

      rvalues$addresses_geocoded %>%
        dplyr::filter(is.na(lat))
    })

    output$missing_table <- DT::renderDataTable({

      req(missing_addresses())

      validate(
        need(nrow(missing_addresses()) > 0, 'All records have been geocoded')
      )

      missing_addresses() %>%
        dplyr::select(address) %>%
        DT::datatable(
          selection = 'single',
          colnames = c('Address' = 'address')
        )

    })

    # Modal PopUp
    observeEvent(
      input$missing_table_rows_selected, {
        missing_table_selected_row <- missing_addresses() %>% dplyr::slice(input$missing_table_rows_selected)

        rvalues$count <- rvalues$count + 1
        mod_geocoding_manual_server(
          paste0('missing_table_selected', rvalues$count),
          rvalues,
          missing_table_selected_row
        )
        mod_geocoding_manual_ui(paste0(ns("missing_table_selected"), rvalues$count)) }
    )

    ############# Download Geocoded Data #############

    output$download_addresses_bttn <- downloadHandler(
      filename = function(){ name_download(input$upload_addresses$name, '-geocoded-') },
      content = function(file) {
        rvalues$addresses_geocoded %>%
          dplyr::select(-response) %>%
          readr::write_csv(file)
      })

    ############# Example Dataset #############
    example_text_str <- "
      - **address**: `Required.` string. Must have city and country defined
      - **lat**, **lng**: `Optional.` numeric double. If present, addresses will not be geocoded
      "
    mod_example_dataset_server(
      "example_geocode",
      example_text = example_text_str,
      example_dataset = example_dataset,
      package = package_name
    )

  })
}

## To be copied in the UI
# mod_geocoding_ui("geocoding_1")

## To be copied in the server
# mod_geocoding_server("geocoding_1")
