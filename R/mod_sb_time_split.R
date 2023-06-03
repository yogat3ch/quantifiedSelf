#' sb_time_split UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sb_time_split_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("split"),
      label = "Split By:",
      choices = list(
        Year = "year",
        Quarter = "quarter",
        Month = "month",
        Week = "week"
      )
    )
  )
}

#' sb_time_split Server Functions
#'
#' @noRd
mod_sb_time_split_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$split, {
      session$userData$log$data_split(input$split)
    }, ignoreNULL = TRUE)
  })
}

## To be copied in the UI
# mod_sb_time_split_ui("sb_time_split_1")

## To be copied in the server
# mod_sb_time_split_server("sb_time_split_1")
