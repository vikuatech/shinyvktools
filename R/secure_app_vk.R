#' secure_app_vk
#'
#' @description login module for vikua shiny apps
#'
#' @param app_ui function application user-interface
#' @param prod boolean identifying if the app communicates with BQ to ask for credentials and save logs
#' @param tags_vk named list with the top, bottom and img strings to include in login module
#' @param gcp_project,bq_dataset,bq_table string names of GCP resources to retrieve and write
#' @param fields string vector of fields to return from `bq_table`. These will be available in auth reactive
#' @param app_name string to include in logs
#'
#' @return invisible.
#'
#' @export
secure_app_vk <- function(app_ui, prod = T, tags_vk){

  if(prod){
    shinymanager::secure_app(
      app_ui,
      id = "auth",
      tags_top =
        tags$div(
          tags$style(HTML("
               @import url('https://storage.googleapis.com/vikua-styles/logos/loginTwo.css');
          ")
          ),
          tags$img(
            id="logo_vikua-fondo",
            src = "https://storage.googleapis.com/vikua-styles/logos/background_blue_login.png",
            alt = 'logo_vikua', width = '150'
          ),
          tags$img(
            id="logo_vikua",
            src = tags_vk$img,
            alt = 'logo_vikua', width = '150'
          ),
          tags$img(
            id="logo_vikua-white",
            src = "https://storage.googleapis.com/vikua-styles/logos/logo_negative_vikua.png",
            alt = 'logo_vikua', width = '150'
          ),
          tags$h3(tags_vk$top, style = "align:center", id="form-title"),
          tags$h3("La vida, mejor", style = "align:center", id="title-one"),
          tags$div(
            id="title-two",
            tags$h3("Toda tu data", style = "align:center", id="title-two-text"),
            tags$h3("en un solo lugar", style = "align:center", id="title-two-text"),
          )
        ),
      tags_bottom = tagList(
        tags$div(id = "placeholder-loading"),
      )
    )
  }
  else{
    app_ui
  }
}

#' @export
#' @rdname secure_app_vk
check_credentials_vk <- function(prod = T, gcp_project, bq_dataset, bq_table = 'credentials', fields = c('permission', 'company')){

  function(user, password){
    user_ <- user
    password_ <- password

    # Aviso de Espera mientras se procesa login
    # https://github.com/datastorm-open/shinymanager/issues/131
    shiny::insertUI(
      selector = "#placeholder-loading",
      immediate = TRUE,
      ui = tagList(
        tags$div(
          id = "loading-ui",
          tags$div(
            style = "text-align: center;",
            "Authentication in progress, please wait..."
          )
        )
      )
    )

    if(prod){
      parse_fields <- fields %>% paste0(collapse = ', ')
      .q <- sprintf('select user, %s from %s.%s where user = "%s" and password = "%s"',
                    parse_fields, bq_dataset, bq_table, user_, password_)
      res <- bigrquery::bq_project_query(gcp_project, .q) %>%
        bigrquery::bq_table_download()

    } else{

      local_creds <- readr::read_csv('.secrets/credentials_temp.csv', show_col_types = FALSE)

      res <- local_creds %>%
        dplyr::filter(user == user_) %>%
        dplyr::filter(password == password_)

    }

    # Remove loading-ui text
    shiny::removeUI(
      selector = "#loading-ui",
      immediate = TRUE
    )

    if (nrow(res) > 0) {
      list(result = TRUE, user_info = as.list(res))
    } else {
      list(result = FALSE)
    }

  }

}

#' @export
#' @rdname secure_app_vk
log_store_custom_vk <- function(app_name, gcp_project, bq_dataset, bq_table, prod){
  if(prod){
    shinylogs::track_usage(
      app_name = app_name,
      storage_mode = shinylogs::store_custom(
        function(logs) {

          df <- tibble::tibble(logs = jsonlite::toJSON(logs), datetime = Sys.time())
          bigrquery::bq_table(project = gcp_project, dataset = bq_dataset, table = bq_table) %>%
            bigrquery::bq_table_upload(
              df,
              create_disposition = 'CREATE_IF_NEEDED',
              write_disposition = 'WRITE_APPEND',
              quiet = F,
              fields = bigrquery::as_bq_fields(df)
            )
        }
      )
    )
  }
}
