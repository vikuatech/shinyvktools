#' chat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param user_id Vikua User ID to log the conversation
#' @param assistant_id OpenAI assistant ID to use in the conversation
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chat_ui <- function(id, height = '600px'){
  ns <- NS(id)

#   js <- '
# $(document).keyup(function(event) {
#   if ($("#user_message").is(":focus") && (event.keyCode == 13)) {
#       $("#send_message").click();
#   }
# });
# '

  tagList(
    fluidPage(

      # Only for development
      shinyjs::useShinyjs(),
      waiter::useWaiter(),

      # tags$head(tags$script(HTML(js))),

      tags$hr(),
      fluidRow(
        bs4Dash::column(
          width = 12,
          id="mensajes",

          div(
            style='height:800px;overflow-y: scroll;',
            uiOutput(ns("chat_history"))
          )
        )
      ),

      tags$hr(),

      fluidRow(
        bs4Dash::column(
          6,
          textAreaInput(
            inputId = ns("user_message"),
            placeholder = "Mensaje a VikuaBot",
            label = NULL,
            width = "100%"
          )
        ),
        column(
          2,
          shinyWidgets::actionBttn(
            ns("send_message"),
            label = NULL,
            icon = fontawesome::fa_i("paper-plane"),
            color = 'primary',
            block = T
          )
        )
      )

    )
  )
}

#' chat Server Functions
#'
#' @noRd
mod_chat_server <- function(id, user_id, assistant_id, project, dataset){

  # Only for Development
  bigrquery::bq_auth(path = ".secrets/tributo-app-manager_sat.json")
  openai_key <- Sys.getenv("OPENAI_API_KEY")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    w <- waiter::Waiter$new(
      id = ns("chat_history"),
      html = waiter::spin_loaders(id = 3, color = "black", style = NULL),
      color = waiter::transparent(.5)
    )

    # Initialize Thread ID and dataframe store object
    rv <- reactiveValues(
      chat_data = { data.frame(source = 'VikuaBot', message = 'Hola! En que puedo ayudarte?') },
      thread_id = {
        # thread <- create_thread_openai(openai_key)
        # thread %>% purrr::pluck('id')

        Sys.getenv("DEV_THREAD_TAX_EXPERT")
      }
    )

    # Log Chat Config in BQ
    # observeEvent(rv$thread_id, {
    #   chat_config <- tibble::tibble(
    #     assistant_id = assistant_id,
    #     user_id = user_id,
    #     thread_id = rv$thread_id,
    #     created_at = Sys.time()
    #   )
    #
    #   chat_config %>%
    #     vktools::bq_post('reporting-338116', 'vikua_platform', 'chat_config', write_disposition = 'WRITE_APPEND')
    #
    # })

    observeEvent(input$send_message, {

      w$show()

      if(input$user_message == ""){
        NULL
      }
      else{

        shinyjs::disable(id = "send_message")
        shinyjs::disable(id = "user_message")

        new_data <- data.frame(source = "User", message = input$user_message)
        rv$chat_data <- rbind(rv$chat_data, new_data)

        # Sys.sleep(5)
        # assistant_response <- 'Respuesta ChatGPT!!'

        assistant_response <- vkchat::ask_openai(
          input$user_message,
          rv$thread_id,
          assistant_id,
          openai_key,
          project_id = project,
          dataset_id = dataset
        ) %>%
          vkchat::clean_response()

        if(!is.null(assistant_response)) {
          gpt_data <- data.frame(source = "VikuaBot", message = assistant_response)
          rv$chat_data <- rbind(rv$chat_data, gpt_data)
        }

        shinyjs::enable(id = "send_message")
        shinyjs::enable(id = "user_message")
        updateTextInput(session, "user_message", value = "")
      }

    })

    output$chat_history <- renderUI({

      chat_data <- rv$chat_data
      n_messages <- nrow(chat_data)
      chatBox <- purrr::map(1:n_messages, ~compile_chat(.x, chat_data))

      do.call(tagList, chatBox)

    })

  })
}

## To be copied in the UI
# mod_chat_ui("chat_1")

## To be copied in the server
# mod_chat_server("chat_1")