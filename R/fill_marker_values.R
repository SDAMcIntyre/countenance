#' Title
#'
#' @param femg_data data frame or tibble
#' @param fill_duration duration to fill in the marker channel for each marker
#' @param marker_channel the name of the column in femg_data that corresponds to the marker channel
#' @param time_channel the name of the column in femg_data that gives the time in seconds
#'
#' @return
#' @export
#'
#' @examples
fill_marker_values <- function(femg_data, fill_duration, marker_channel, time_channel = "time_sec") {

  starts <- femg_data %>%
    add_transitions(marker_channel) %>%
    dplyr::filter(.data$transition_start & .data[[marker_channel]] != OFF_MARKER_VALUE) %>%
    dplyr::select(c(.data[[time_channel]], dplyr::all_of(marker_channel)))

  name_marker_channel_filled <- paste(marker_channel, "filled", sep = "_")

  femg_data <- femg_data %>%
    dplyr::mutate(!!name_marker_channel_filled := .data[[marker_channel]])

  for (n in seq_along(starts[[time_channel]])) {
    tofill <- which(
      femg_data[[time_channel]] > starts[[time_channel]][n] &
        femg_data[[time_channel]] <= starts[[time_channel]][n] + fill_duration
    )
    femg_data[tofill,name_marker_channel_filled] <- starts[n, marker_channel] %>% dplyr::pull()
  }

  femg_data

}
