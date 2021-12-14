#' Compare a Marker channel in fEMG data to a sequence of markers in a session log file
#'
#' @param femg_data data frame or tibble
#' @param data_marker_channel the name of the column in femg_data that corresponds to the marker channel
#' @param data_time_channel the name of the column in femg_data that gives the time in seconds
#' @param session_log data frame or tibble
#' @param log_marker_channel the name of the column in session_log that corresponds to the marker channel
#' @param log_start_time the name of the column in session_log that has the start times of the markers
#' @param log_end_time The name of the column in session_log that has the end times of the markers. Must be provided if log_duration is NULL.
#' @param log_duration  The name of the column in session_log that has the durations of the markers. Must be provided if log_end_time is NULL.
#' @param fill_marker_values set to TRUE if you want to fill marker values in femg_data based on the duration in the session log
#'
#' @return a list:
#'
#' session_log_aligned
#'
#' synced
#'
#' start_offsets
#'
#' end_offsets
#'
#' comparison_plot
#'
#' femg_data_filled_markers (if fill_marker_values = TRUE)
#'
#' filled_plot (if fill_marker_values = TRUE)
#'
#' @export
#'
#' @examples
compare_session_log <- function(
  femg_data,
  data_marker_channel,
  data_time_channel,
  session_log,
  log_marker_channel,
  log_start_time,
  log_end_time = NULL,
  log_duration = NULL,
  fill_marker_values = FALSE
  ) {

  # read in stim file
  session_log <- session_log %>%
    dplyr::mutate(
    !!log_duration := dplyr::if_else(
      is.null(log_duration),
      .data[[log_end_time]] - .data[[log_start_time]],
      .data[[log_duration]]
      ),
    !!log_end_time := dplyr::if_else(
      is.null(log_end_time),
      .data[[log_start_time]] + .data[[log_duration]],
      .data[[log_end_time]]
      )
    )

  # get time of first stimulus in stimulus file
  log_first_marker_start <- session_log %>%
    dplyr::filter(.data[[log_marker_channel]] != OFF_MARKER_VALUE) %>%
    dplyr::summarise(start = min(.data[[log_start_time]])) %>%
    dplyr::pull(start)

  # get time of first stimulus in femg data
  femg_data <- femg_data %>%
    add_transitions(data_marker_channel)

  data_first_marker_start <- femg_data %>%
    dplyr::filter(.data$transition.start & .data[[data_marker_channel]] != OFF_MARKER_VALUE) %>%
    dplyr::summarise(start = min(.data[[data_time_channel]])) %>%
    dplyr::pull(.data$start)

  # time offset between data and log
  offset <- data_first_marker_start - log_first_marker_start

  # align log to first marker in femg data
  log_aligned <- session_log %>%
    dplyr::select(c(log_marker_channel, log_start_time, log_duration, log_end_time)) %>%
    dplyr::mutate(
      log_start_time = log_start_time + offset,
      log_end_time = log_end_time + offset
      )

  # get the sequence of markers in the femg data
  data_marker_sequence <- femg_data %>%
    dplyr::filter(.data$transition.start & .data[[data_marker_channel]] != OFF_MARKER_VALUE) %>%
    dplyr::pull(.data[[data_marker_channel]])

  # is the sequence in the femg data  the same as in the log?
  synced <- length(session_log[[log_marker_channel]]) == length(data_marker_sequence)
  if (synced) synced <- sum(abs(session_log[[log_marker_channel]] - data_marker_sequence)) == 0

  # output list initiated
  output <- list(
    'session_log_aligned' = log_aligned,
    'synced' = synced
    )

  # start times for all markers in femg data
  data_start_time <- femg_data %>%
    dplyr::filter(.data$transition.start & .data[[data_marker_channel]] != OFF_MARKER_VALUE) %>%
    dplyr::pull(.data[[data_time_channel]])

  # end times for all markers in femg data
  data_end_time <- femg_data %>%
    dplyr::filter(.data$transition.end & .data[[data_marker_channel]] != OFF_MARKER_VALUE) %>%
    dplyr::pull(.data[[data_time_channel]])

  if (synced) {
    # does the timing of the start of the markers match?
    start_offsets <- log_aligned$log_start_time - data_start_time
    output$start_offsets <- start_offsets

    # does the timing of the end of the markers match?
    end_offsets <- log_aligned$log_end_time - data_end_time
    output$end_offsets <- end_offsets
  }

  comparison_plot <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = dplyr::filter(femg_data, .data$transition.start | .data$transition.end),
      ggplot2::aes(x = .data[[data_time_channel]], y = .data[[data_marker_channel]])
      ) +
    ggplot2::geom_text(
      data = dplyr::filter(femg_data, .data$transition.start & .data[[data_marker_channel]] != OFF_MARKER_VALUE),
      ggplot2::aes(
        x = .data[[data_time_channel]],
        y = .data[[data_marker_channel]] + 40,
        label = .data[[data_marker_channel]]
        ),
      colour = 'black'
      ) +
    ggplot2::geom_point(
      data = log_aligned,
      ggplot2::aes(x = .data[[log_start_time]], y = .data[[log_marker_channel]]),
      colour = 'blue',
      shape = 3
      ) +
    ggplot2::geom_text(
      data = log_aligned,
      ggplot2::aes(x = .data[[log_start_time]], y = .data[[log_marker_channel]] + 20, label = .data[[log_marker_channel]]),
      colour = 'blue'
      ) +
    ggplot2::geom_point(
      data = log_aligned,ggplot2::aes(x = log_end_time, y = OFF_MARKER_VALUE),
      colour = 'blue',
      shape = 4
      ) +
    ggplot2::labs(
      title = 'Black: marker values in femg file; Blue: marker values in session log file',
      y = data_marker_channel
      )

  # add plot to output
  output$comparison_plot <- plotly::ggplotly(comparison_plot)

  name_log_marker_channel_filled = paste(log_marker_channel, "filled", sep = "_")

  if (fill_marker_values) {

    femg_data_filled_markers <- femg_data %>%
      dplyr::mutate(
        !!name_log_marker_channel_filled := .data[[data_marker_channel]]
        )

    for (n in seq_along(log_aligned[[log_marker_channel]])) {
      tofill <- which(
        femg_data[[data_time_channel]] > data_start_time[n] &
          femg_data[[data_time_channel]] <= data_start_time[n] + log_aligned[[log_duration]][n]
      )
      femg_data_filled_markers[tofill, name_log_marker_channel_filled] <- log_aligned[[log_marker_channel]][n]
    }

    femg_data_filled_markers <- femg_data_filled_markers %>%
      add_transitions(name_log_marker_channel_filled)

    filled_plot <- ggplot2::ggplot() +
      ggplot2::geom_path(
        data = dplyr::filter(femg_data_filled_markers, .data$transition.start | .data$transition.end),
        ggplot2::aes(x = .data[[data_time_channel]], y = .data[[name_log_marker_channel_filled]]),
        colour = 'darkgreen'
        ) +
      ggplot2::geom_text(
        data = dplyr::filter(
          femg_data_filled_markers, .data$transition.start & .data[[name_log_marker_channel_filled]] != OFF_MARKER_VALUE
          ),
        ggplot2::aes(
          x = .data[[data_time_channel]],
          y = .data[[name_log_marker_channel_filled]] + 40,
          label = .data[[name_log_marker_channel_filled]]
          ),
        colour = 'darkgreen'
        ) +
      ggplot2::geom_point(
        data = log_aligned,
        ggplot2::aes(x = .data[[log_start_time]], y = log_marker_channel),
        colour = 'blue',
        shape = 3
        ) +
      ggplot2::geom_text(
        data = log_aligned,
        ggplot2::aes(x = .data[[log_start_time]], y = log_marker_channel + 20, label = log_marker_channel),
        colour = 'blue'
        ) +
      ggplot2::geom_point(
        data = log_aligned,
        ggplot2::aes(x = log_end_time, y = OFF_MARKER_VALUE),
        colour = 'blue',
        shape = 4
        ) +
      ggplot2::labs(
        title = 'Green: stim codes in femg file (duration filled from stim file); Blue: stim codes in stim file',
        y = data_marker_channel
        )

    output$femg_data_filled_markers <- femg_data_filled_markers
    output$filled_plot <- plotly::ggplotly(filled_plot)

  } else {
    start_median <- stats::median(start_offsets)
    end_median <- stats::median(end_offsets)
    if (end_median > 3*start_median) {
      warning(paste('compare_session_log()\n',
                    'offsets for the end of the stimulus are between',
                    prettyNum(min(end_offsets)), 'and', prettyNum(max(end_offsets)),
                    'seconds (median =', prettyNum(end_median),
                    'seconds). \nDo you want \'fill_marker_values = TRUE\'?'))
    }
  }

  return(output)

}
