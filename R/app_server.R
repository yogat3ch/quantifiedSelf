#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # Build Sass ----
  # Sat May 27 14:36:27 2023
  sass::sass(
    input = sass::sass_file(dirs$css("custom", ext = "scss")),
    output = dirs$css("custom.min", ext = "css"),
    options = sass::sass_options(output_style = "compressed")
  )
  e <- environment()
  ud <- session$userData
  assign("log", ct_data(), envir = session$userData)
  assign("state", shinyVirga::rv(), envir = session$userData)

  observeEvent(input$file, {
    shinyjs::hideElement(asis = TRUE, id = "ex_data")
    d <- try(log_read(input$file$datapath))
    if (UU::is_error(d)) {
      shinyjs::showElement(id = "ex_data")
      shinyjs::removeClass("ex_data", class = "text-success")
      shinyjs::addClass("ex_data", class = "text-danger")
      shinyjs::html(id = "ex_data", "Not a valid Insight Timer CSV Export.")
      req(NULL)
    }
    ud$log$data_update(d, o_data = TRUE)
  }, priority = 5)

  observeEvent(input$minimize_sb, {
    shinyjs::toggle(selector = ".sb")
    shinyjs::toggleClass("sb", class = "w-5 bg-transparent", asis = TRUE)
    shinyjs::toggleClass("main", class = "w-100", asis = TRUE)
    shinyjs::runjs("$('.plotly').resize()")
  })
  observeEvent(input$plot_type, {
    ud$log$plot_type(input$plot_type)
  }, priority = 2)

  observeEvent(ud$state$render, {
    graphs <- graph_gen(ud$log)
    ui <- list()
    for (i in seq_along(graphs)) {
      id <- paste0("graph_",i)
      output[[id]] <- NULL
      output[[id]] <- plotly::renderPlotly(rlang::expr({graphs[[!!i]]}), quoted = TRUE)
      ui[[id]] <- plotly::plotlyOutput(id)
    }
    g_len <- length(graphs)
    if (g_len >= 2) {
      rows <- g_len %/% 2
      ui_split <- split(ui, rep(1:rows, each = 2))
      out <- tagList()
      for (el in ui_split) {
        out <- htmltools::tagAppendChildren(out, do.call(fluidRow, list(shiny::column(6, el[[1]]), shiny::column(6, el[[2]]))))
      }
    } else {
      out <- fluidRow(ui[[1]])
    }

    output$charts <- renderUI({out})
  }, priority = 1)

  mod_sb_data_prep_server("prep")
}



