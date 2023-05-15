#' secure_app_vk
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
secure_app_vk <- function(app_ui, prod = T){

  if(prod){
    shinymanager::secure_app(
      app_ui,
      id = "auth",
      tags_top =
        tags$div(
          tags$h3("Vikua Platform: La Wawa Quoting App", style = "align:center"),
          tags$img(
            src = "https://storage.googleapis.com/vikua-styles/logos/logo_vikua_es_dark.png",
            alt = 'logo_vikua', width = '150'
          )
        ),
      tags_bottom = tagList(
        tags$div(id = "placeholder-loading"),
        tags$div(
          tags$p("Specialized interactive platform for La Wawa made by Vikua Corp")
        )
      ),
      background  = "linear-gradient(#272c30, #272c30);"
    )
  }
  else{
    app_ui
  }
}
