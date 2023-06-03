#' sb_data_prep UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sb_data_prep_ui <- function(id){
  ns <- NS(id)
  r <- range(.log$start)
  tagList(
    shiny::dateRangeInput(
      inputId = ns("timeline"),
      label = "Time range considered:",
      start = r[1],
      end = r[2],
      min = r[1],
      max = r[2]
    ),
    selectInput(
      inputId = ns("split"),
      label = "Split By:",
      choices = rlang::list2(
        `No Split` = "none",
        !!!UU::time_aggregates
      )
    ),
    selectInput(
      ns("agg"),
      label = "Aggregate By:",
      choices = UU::time_aggregates
    ),
    div(
      class = "d-flex justify-content-end",
      actionButton(
        class = "btn-info",
        inputId = ns("render"),
        label = "Render",
        icon = shiny::icon("chart-simple")
      )
    )
  )
}

#' sb_data_prep Server Functions
#'
#' @noRd
mod_sb_data_prep_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ud <- session$userData
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("split", ~ {
      if (!is.null(input$agg)) {
       if (identical(., "year") && identical(input$agg, "season")) "Split: Year & Aggregate: Season are incompatible as seasons span the change of years"
      }
    })

    iv$add_rule("split", ~ {
      if (!is.null(input$agg) && !identical(., "none")) {
        x <- UU::time_factor(c(., input$agg))
        if (!x[1] > x[2]) "The split timespan must be greater than the aggregation timespan"
      }
    })
    iv$add_rule("agg", ~ {
      ud$log$data_timespan_check(., "The aggregation timespan must be less than the total time range", timeline = input$timeline)
    })
    iv$add_rule("split", ~ {
      if (!identical(., "none")) {
        ud$log$data_timespan_check(., "The split timespan must be less than the total time range", timeline = input$timeline)
      }
    })
    iv$add_rule("timeline", ~{
      if (!identical(.[1], .[2])) {
        if (!is.null(input$split) && !identical(input$split, "none")) {
          ud$log$data_timespan_check(input$split, "The time range must be greater than the split timespan", timeline = input$timeline)
        }
        if (!is.null(input$agg)) {
          ud$log$data_timespan_check(input$agg, "The time range must be greater than the aggregation timespan", timeline = input$timeline)
        }
      }
    })
    iv$enable()
    # Time Range ----
    # Sat May 27 05:08:34 2023
    observeEvent(ud$log$new_data(), {
      r <- range(ud$log$o_data()$start)
      # Update the timeline
      shiny::updateDateRangeInput(
        inputId = "timeline",
        start = r[1],
        end = r[2],
        min = r[1],
        max = r[2]
      )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$timeline, {
      tl <- input$timeline
      # Check if split or aggregate need to be updated
      d <- lubridate::as.duration(difftime(tl[2], tl[1]))
      # Split/Agg levels that would be out of bounds
      oob <- unlist(UU::time_difftimes) > as.numeric(d)
      if (any(oob)) {
        # indexes of time aggregates to remove
        i <- 1:which.min(which(oob))
        choices <- UU::time_aggregates[-i]
        shiny::updateSelectInput(
          inputId = "split",
          choices = choices,
          selected = choices[1]
        )
      }
      shinyjs::toggleClass("render", class = "animated")
    }, priority = 2)

    observeEvent(c(input$agg, input$split), {
      if (!identical(input$split, "none")) {
        r <- UU::time_factor(c(input$split, input$agg))
        # How many to shave off
        l <- length(length(UU::time_aggregates):as.numeric(r[1]))
        choices <- UU::time_aggregates[-c(1:l)]
        shiny::updateSelectInput(
          inputId = "agg",
          choices = choices,
          selected = choices[ifelse(choices[[1]] == "season" && input$split == "year", 2, 1)]
        )
      }

      shinyjs::toggleClass("render", class = "animated")
    }, priority = 1)
    observeEvent(input$render, {
      req(iv$is_valid())
      tl <- input$timeline
      # Filter
      ud$log$data_filter(start = tl[1], end = tl[2])
      # Split and aggregate
      ud$log$split(input$split)
      ud$log$agg(input$agg)
      ud$state$render <- input$render
      shinyjs::removeClass("render", class = "animated")
    }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = 3)



  })
}

## To be copied in the UI
# mod_sb_data_prep_ui("sb_data_prep_1")

## To be copied in the server
# mod_sb_data_prep_server("sb_data_prep_1")
