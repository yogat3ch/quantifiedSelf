#' sb_time_ranger UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sb_time_ranger_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::dateRangeInput(
      inputId = ns("timeline"),
      label = "Time range considered:"
    )
  )
}

#' sb_time_ranger Server Functions
#'
#' @noRd
mod_sb_time_ranger_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(session$userData$log$o_data(), {
      r <- range(session$userData$log$o_data()$start)
      shiny::updateDateRangeInput(
        inputId = "timeline",
        start = r[1],
        end = r[2],
        min = r[1],
        max = r[2]
      )
    })

    observeEvent(input$timeline, {
      tl <- input$timeline
      session$userData$log$data_filter(start = tl[1], end = tl[2])
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
  })
}

## To be copied in the UI
# mod_sb_time_ranger_ui("sb_time_ranger_1")

## To be copied in the server
# mod_sb_time_ranger_server("sb_time_ranger_1")
