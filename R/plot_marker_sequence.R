#' Plot the sequence of marker values
#'
#' Create a ggplot object, plotting the sequence of transitions found in a marker channel, and highlighting unexpected values in red.
#'
#' @param femg_data data frame or tibble
#' @param marker_channel the name of the column in femg_data that corresponds to the marker channel
#' @param unexpected_channel the name of the column in femg_data that is a boolean indicating if the channel was unexpected
#' @param time_channel the name of the column in femg_data that gives the time in seconds
#'
#' @return a ggplot object
#' @export
#'
#' @examples
plot_marker_sequence <- function(femg_data, marker_channel, unexpected_channel = "unexpected", time_channel = 'time_sec') {

  unexpected_markers <- femg_data %>%
    dplyr::filter(.data[[unexpected_channel]]) %>%
    .[[marker_channel]] %>%
    unique()

  femg_data %>%
    add_transitions(marker_channel) %>%
    dplyr::filter(.data$transition_start | .data$transition_end) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[time_channel]], y = .data[[marker_channel]])) +
    ggplot2::geom_path() +
    ggplot2::geom_point(ggplot2::aes(colour = .data[[unexpected_channel]])) +
    ggplot2::scale_colour_manual(values = c('grey','red')) +
    ggplot2::labs(
      title = paste(unexpected_markers, collapse = ", "),
         y = 'Marker Value',
         x = 'Time (sec)'
      )
}
