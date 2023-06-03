#' @include utils_dir_fns.R
col_specs <- list(
  `Started At` = readr::col_datetime(format = "%m/%d/%Y %H:%M:%S"),
  `Duration` = readr::col_character(),
  `Preset` = readr::col_logical(),
  `Activity` = readr::col_character()
)

log_munge <- function(.data) {
  .data |>
    dplyr::distinct(`Started At`, .keep_all = TRUE) |>
    dplyr::mutate(
      start = `Started At`,
      # Durations
      duration = lubridate::hms(Duration),
      hours = lubridate::hour(duration) + lubridate::minute(duration) / 60 + lubridate::second(duration) / 3600,
      # Floor dates for timeseries
      !!!purrr::map(rlang::set_names(UU::time_aggregates), \(.x) rlang::expr(lubridate::floor_date(start, !!.x))),
      # Factor aggregates for histograms
      year = lubridate::year(start),
      fyear = factor(year),
      fseason = UU::season_factor(start, label = TRUE, abbr = FALSE),
      fquarter = lubridate::quarter(start),
      fmonth = lubridate::month(start, label = TRUE),
      fweek = lubridate::week(start),
      fday = lubridate::wday(start),
      fhour = lubridate::hour(start),
      # Category
      activity = stringr::str_remove(Activity, "^PracticeType\\."),
      # Remove original columns
      `Started At` = NULL,
      `Duration` = NULL,
      `Preset` = NULL,
      `Activity` = NULL
    )
}

log_read <- function(file = dirs$extdata("Insight Timer Logs", ext = "csv")) {
  readr::read_csv(
    file,
    col_types = col_specs
  ) |>
    log_munge()
}
.log <- log_read()


ct_data <- function(.data = .log, key = NULL, group = "logs") {
  ct <- crosstalk::SharedData$new(reactiveVal(.data), key = key, group = group)
  ct$addMethods(
    o_data = reactiveVal(.data), # Keep track of the original date
    new_data = function() { # reactive counter for when new data is uploaded
      private$.new_data()
    },
    private_assign = function(...) {
      rlang::env_unlock(private)
      dots <- rlang::dots_list(...)
      dot_nms <- names(dots)
      for (i in seq_along(dots)) {
        assign(envir = private, dot_nms[i], dots[[i]])
      }
      rlang::env_lock(private)
      self
    },
    data_filter = function(start, end) {
      d <- dplyr::filter(
        self$o_data(),
        dplyr::between(.data$start, .env$start, end + 1) # +1 so it's inclusive since Date starts at midnight
      )
      self$data_update(d)
      self$data()
    },
    plot_type = function(x) {
      if (!missing(x))
        private$.pt(x)
      else
        private$.pt()
    },
    timespan = function() {
      private$.timespan()
    },
    split = function(x, chr = FALSE) {
      if (!missing(x))
        private$.split(x)
      if (chr)
        private$.split()
      else
        private$split()
    },
    agg = function(x) {
      if (!missing(x))
        private$.agg(x)
      else
        private$.agg()
    },
    data_update = function(.data, o_data = FALSE) {
      private$.data(.data)
      if (o_data) {
        self$o_data(.data)
        private$.new_data(private$.new_data() + 1)
      }

      self
    },
    data_timespan = function(level = FALSE) {
      d <- self$data()
      out <- UU::timespan(d$start)
      if (level)
        out <- UU::duration_print(out)
      return(out)
    },
    data_timespan_check = function(x, timeline = NULL, msg, fn = `<`) {
      if (identical(timeline[1], timeline[2]))
        return(NULL)
      ts1 <- if (is.null(timeline))
        self$data_timespan()
      else
        UU::timespan(timeline)

      ts2 <- UU::timespan(x)

      if (fn(ts1, ts2))
        msg
      else
        NULL
    }
  )
  ct$private_assign(.split = reactiveVal(), split = reactiveVal(), .agg = reactiveVal(), .pt = reactiveVal(), .new_data = reactiveVal(0))
}

