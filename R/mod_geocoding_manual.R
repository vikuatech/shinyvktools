#' geocoding_manual UI Function
#'
#' @description
#' Giving a one-row df *selected_address* this shiny Module allows geocoding by adding a marker to a map,
#' or by manually writting address text and applying geocode_google_safe.
#' The module saves the geocoded address to a element called *addresses_geocoded* in a reactiveValue passed as argument *geocode_rvalues*.
#' This module aims to be a inner wrapper module in *mod_geocoding*
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param geocode_rvalues reactive values passed from outter module, must have *addresses_geocoded* df element
#' @param selected_address one-row df with address and row_id column
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_geocoding_manual_ui <- function(id){
  ns <- NS(id)
  tagList(
    showModal(
      modalDialog(
        fluidRow(
          bs4Dash::column(
            12,
            shiny::textOutput(ns('missing_selected_address'))
          ),

        ),

        br(),

        fluidRow(
          bs4Dash::column(
            4,
            shiny::textInput(ns('missing_manual_address_text'), 'Try with another address text:')
          ),

          br(),

          bs4Dash::column(
            4,
            shinyWidgets::actionBttn(
              ns('geocode_missing_bttn'),
              'Geocode',
              icon = fontawesome::fa_i('map-location-dot')
            )
          )
        ),
        fluidRow(
          bs4Dash::column(
            12,
            strong('Or click over the map to add/update the location'),
            leaflet::leafletOutput(ns('missing_map'))
          )
        ),

        br(),

        fluidRow(
          bs4Dash::column(
            1, offset = 11,
            shinyWidgets::actionBttn(
              ns('save_missing_geocoded'),
              label = 'Save',
              size = 'md',
              style = "pill",
              color = 'success'
            )
          )
        ),

        title = "Geocode address",
        size = 'xl',
        footer = NULL,
        easyClose = TRUE
      )
    )
  )
}

#' geocoding_manual Server Functions
#'
#' @noRd
mod_geocoding_manual_server <- function(id, geocode_rvalues, selected_address){

  stopifnot(all(c('address', 'row_id') %in% names(selected_address)))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rvalues_manualgeo <- reactiveValues()

    cat('Save bttn state', input$save_missing_geocoded, '\n')
    cat('Map click state \n')
    print(input$missing_map_click)

    observe({
      if(is.null(input$missing_map_click)) {
        shinyjs::hide("save_missing_geocoded")
      } else {
        shinyjs::show("save_missing_geocoded")
      }
    })

    # Plain leaflet map to show on modal
    output$missing_map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>% leaflet::addTiles()
    })

    # Text address of row-selection
    output$missing_selected_address <- shiny::renderText({
      selected_address %>% dplyr::pull(address)
    })

    # Verbose new selected location
    observeEvent(input$missing_map_click, {

      new_geopoint <- input$missing_map_click

      leaflet::leafletProxy('missing_map') %>%
        leaflet::clearMarkers() %>%
        leaflet::addMarkers(layerId = 'manual_marker', lat = new_geopoint$lat, lng = new_geopoint$lng)

      rvalues_manualgeo$coords <- list(
        lat = new_geopoint$lat,
        lng = new_geopoint$lng
      )

    })

    observeEvent(input$geocode_missing_bttn, {

      cat('Geocoding alternative address \n')

      missing_manual_address_text <- input$missing_manual_address_text

      if(missing_manual_address_text == '' | is.null(missing_manual_address_text)){
        return(NULL)
      }

      missin_geocode <- vktools::geocode_google_safe(missing_manual_address_text)
      lat_ <- missin_geocode$results$geometry$location$lat
      lng_ <- missin_geocode$results$geometry$location$lng

      if(!is.null(lat_)){

        leaflet::leafletProxy('missing_map') %>%
          leaflet::clearMarkers() %>%
          leaflet::addMarkers(layerId = 'manual_marker', lat = lat_, lng = lng_)

        rvalues_manualgeo$coords <- list(
          lat = lat_,
          lng = lng_
        )

        shinyjs::show('save_missing_geocoded')

      }

    })

    # Action button to save manual-pointed new location
    observeEvent(input$save_missing_geocoded, {

      if(is.null(rvalues_manualgeo$coords)){
        removeModal()
        return(NULL)
      }

      selected_row_id <- selected_address %>%
        dplyr::pull(row_id)

      cat('Reactive Save bttn \n')

      geocode_rvalues$addresses_geocoded <- vktools::geocode_update(
        geocode_rvalues$addresses_geocoded,
        id = selected_row_id,
        lat = rvalues_manualgeo$coords$lat,
        lng = rvalues_manualgeo$coords$lng,
        place_id = NA_character_
      )

      leaflet::leafletProxy('missing_map') %>%
        leaflet::clearMarkers()

      removeModal()

    })

    return(NULL)

  })
}

## To be copied in the UI
# mod_geocoding_manual_ui("geocoding_manual_1")

## To be copied in the server
# mod_geocoding_manual_server("geocoding_manual_1")
