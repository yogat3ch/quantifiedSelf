#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),
      titlePanel(title = tagList(tags$a(href = "https://insighttimer.com/", target = "_blank", class = "it_icon", tags$img(class = "img-thumbnail", height = "50px", width = "50px", src = shinyVirga::path_strip_shiny(dirs$img("insight-timer", ext = "jpeg")))), "Insight Timer Summary", tags$a(href = "https://github.com/yogat3ch/quantifiedSelf", class = "float-right", target = "_blank", tags$img(src = "www/img/github-mark-white.svg", height = "50px", width = "50px")))),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          h2("Controls", class = "sb", style = "display:inline-block;"),
          div(
            class = "sb",
            shiny::fileInput("file",
                             "Upload your Insight Timer Export CSV",
                             accept = "csv",
                             placeholder = "example-data.csv"),
            tags$em(class = 'text-success', id = "ex_data", 'Example data loaded, explore it with the controls below.'),
            radioButtons(
              inputId = "plot_type",
              label = "Chart Type:",
              choices = list(Timeseries = "time",
                             Histogram = "hist")
            ),
            mod_sb_data_prep_ui("prep")
          ),
          width = 3
        ) |>
          htmltools::tagAppendAttributes(id = "sb") |>
          htmltools::tagInsertChildren(
            actionButton(
              style = "z-index: 50;position:relative;height:26px;margin-left:-100px",
              "minimize_sb",
              label = NULL,
              icon = shiny::icon("arrows-left-right-to-line"),
              class = "d-inline-block float-right"
            ),
            after = 0
          ),
        mainPanel =
          mainPanel(uiOutput("charts")) |>
          tagAppendAttributes(id = "main")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    shinyjs::useShinyjs(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Insight Timer Summary"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
