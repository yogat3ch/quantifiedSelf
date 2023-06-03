#' echarts
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
graph_gen <- function(.data, agg = NULL, split = NULL, plot_type = NULL) {
  .x <- if (crosstalk::is.SharedData(.data)) {
    plot_type <- .data$plot_type()
    split <- .data$split(chr = TRUE)
    agg <-  .data$agg()
    .data$data()
  } else
    .data





  is_scatter <- !identical(plot_type, "hist")
  agg_f_chr <- paste0("f", agg)
  agg_sym <- rlang::sym(if (is_scatter) agg else agg_f_chr)
  agg_title <- stringr::str_to_title(agg)



  # Grouping ----
  # Sat May 27 20:14:13 2023
  do_split <- !identical(split, "none")
  s <- c(agg)
  if (do_split) {
    spl_sym <- rlang::sym(split)
    s <- c(s, split)
  }
  if (!is_scatter)
    s <- c(s, agg_f_chr)
  gb <- rlang::syms(s)



  # Summarize ----
  # Sat May 27 16:38:31 2023
  summed <- dplyr::group_by(.x, !!!gb) |>
    dplyr::summarise(hours = sum(hours, na.rm = FALSE),
                     duration = sum(lubridate::seconds(duration), na.rm = TRUE))





  # Split ----
  # Sat May 27 18:00:52 2023
  summed <- if (do_split) {
    if (split %in% c("quarter", "month", "week")) {
      f <- getFromNamespace(paste0("year", split), ns = "tsibble")
      summed <- dplyr::mutate(summed, !!spl_sym := f(!!spl_sym))
    }

    .x <- dplyr::group_by(summed, !!spl_sym)
    .x <- rlang::set_names(dplyr::group_split(.x), dplyr::group_keys(.x)[[1]])
  } else {
    summed
  }


  .x <- if (!identical(agg, "year") && !is_scatter) {
    e <- rlang::expr({
      dplyr::group_by(.x, !!agg_sym) |>
        dplyr::summarize(dplyr::across(dplyr::any_of(c("hours", "duration")), .fns = mean))
    })

    .x <- if (do_split) {
      purrr::imap(.x, rlang::new_function(rlang::pairlist2(.x = , .y = ), body = e))
    } else {
      .x <- summed
      rlang::eval_tidy(e)
    }
  } else {
    summed
  }


  #  Plot ----
  # Sat May 27 20:26:36 2023
  # Color and type
  colors <- RColorBrewer::brewer.pal(11, "Spectral")
  type <- switch(plot_type,
         hist = "bar",
         time = "scatter"
  )
  # Plotly
  graph_exp <- rlang::expr({
    d <- dplyr::mutate(.x, duration = lubridate::as.duration(duration),
                        col = UU::color_cycle(!!colors, n = dplyr::n()))


    type <- !!type
    is_scatter <- identical(type, "scatter")
    if (is_scatter)
      d <- dplyr::mutate(d, l = suppressWarnings(purrr::possibly(loess, otherwise = NA)(hours)))

    args <- purrr::compact(list(
      d,
      type = type,
      name = "Hours",
      mode = if (is_scatter)
        "lines+markers",
      x = ~ !!agg_sym,
      y = ~ hours,
      text = ~ UU::duration_print(duration),
      hoverinfo = "text",
      marker = list(color = ~ col)
    ))
    p <- do.call(plotly::plot_ly, args)
    if (is_scatter && !anyNA(d$l))
      p <- p |>
      plotly::add_lines(
        name = "loess average",
        x = ~ !!agg_sym,
        y = ~ l,
        mode = "lines",
        color = I("maroon"),
        marker = NULL
      )

    p <- plotly::layout(
      p,
      plot_bgcolor = "#434343",
      paper_bgcolor = "#434343",
      margin = list(t = -5, b = -5),
      showlegend = is_scatter,
      legend = list(
        font = list(
          color = "#fff"
        )
      ),
      title = list(

        pad = list(b = 10, t = 10),
        text = paste("Average Hours by", agg_title, if (do_split) paste("|", stringr::str_to_title(split),":", .y) else ""),
        font = list(
          color = "#fff"
        )
      ),
      xaxis = list(
        title = list(
          text = agg_title,
          color = "#fff"
        ),
        color = "#fff",
        gridcolor = "#e9f1f1"
      ),
      yaxis = list(
        title = list(
          text = "Hours",
          color = "#fff"
        ),
        color = "#fff",
        gridcolor = "#e9f1f1"
      )
    )

    p
  })


  # Render graph(s) ----
  # Sat May 27 18:01:31 2023
  out <- if (!is.data.frame(.x)) {
    purrr::imap(.x, rlang::new_function(rlang::pairlist2(.x = , .y = ), body = graph_exp))
  } else {
    list(rlang::eval_tidy(graph_exp))
  }

  return(out)
}


line_duration <- function(log, split_on = "year") {
  plot_data <- dplyr::group_by(log, !!!rlang::syms(split_on)) |>
    dplyr::group_split() |>
    rlang::set_names(rev(unique(log$year)))
  out <- list()
  for (i in seq_along(plot_data)) {
    out[[i]] <- plotly::plot_ly(
      plot_data[[i]],
      x = ~ start,
      y = ~ hours,
      text = ~ as.character(duration),
      name = "Duration",
      mode = "lines"
    ) |>
      plotly::layout(
        hovermode = "x unified",
        xaxis = list(
          type = "date",
          # TODO this isn't forming months on the x-axis as intended
          # https://stackoverflow.com/questions/71823559/how-to-set-custom-date-ticks-breaks-in-plotly
          tickvals = as.numeric(lubridate::make_date(year = unique(plot_data[[i]]$year), month = 1:12, day = 1)),
          tickformat = "%b"
        ))
  }
  rlang::exec(plotly::subplot, !!!out,  nrows = length(out), shareY = TRUE)

}

loess <- function(hours) {
  len <- list(hours = length(hours))
  d <- tibble::tibble(hours, start = 1:len$hours)
  out <- predict(stats::loess(hours ~ start, data = d), data = d)
  len <- append(len, list(out = length(out)))
  if (!do.call(identical, unname(len)))
    out <- c(out, rep(out[len$out], len$hours - len$out))
  out
}
year_fix <- function(data) {
  ind <- which(lubridate::year(data$week) != data$year)
  hd <- c("hours", "duration")
  data[ind, hd] <- data[ind, hd] + data[ind + 1, hd]
  data[-ind, ]
}

plot_years_by <- function(log, agg_by = "day") {
  by <- switch(agg_by,
         day = "day",
         week = "week",
         month = "month")
  hours <- switch(agg_by,
                  day = rlang::expr(loess(hours)),
                  week = rlang::expr(hours),
                  month = rlang::expr(hours))
  eng_by <- snakecase::to_sentence_case(by)
  sym_by <- rlang::sym(by)
  y <- lubridate::year(lubridate::today())



  data <- log |>
    dplyr::group_by(year, !!sym_by) |>
    dplyr::summarise(hours = sum(hours, na.rm = TRUE), .groups = "drop_last",
                     duration = lubridate::as.period(sum(lubridate::seconds(duration), na.rm = TRUE))) |>
    dplyr::mutate(start = lubridate:::`year<-`(!!sym_by, y),
                  hours = !!hours) |>
    dplyr::distinct()
    if (agg_by == "week") {
      data <- year_fix(data)
    }
  smoothing <- ifelse(agg_by == "day", "<br><sup>Gaussian smoothing applied</sup>", "")
    plotly::plot_ly(
      data,
      type = "scatter",
      x = ~ start,
      y = ~ hours,
      name = ~ year,
      text = ~ as.character(duration),
      mode = "lines"
    ) |>
    plotly::layout(
      title = list(
        text = glue::glue("Total Hours by {eng_by} across Years{smoothing}")
      ),
      hovermode = "x unified",
      xaxis = list(
        title = list(
          text = "Months"
        ),
        type = "date",
        dtick = "M1",
        tickformat = "%b"
      ))
}

