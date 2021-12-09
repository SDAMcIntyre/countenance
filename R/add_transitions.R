diff_from_prev <- function(x) {
  c(TRUE, diff(x) != 0)
}

diff_from_next <- function(x) {
  c(diff(x) != 0, TRUE)
  }

#' Add a column to a data frame indicating transitions in a marker channel
#'
#' @param femg_data data frame or tibble
#' @param marker_channel the name of the column in femg_data that corresponds to the marker channel
#'
#' @return femg_data with extra columns "transition_start" and "transition_end"
#' @export
#'
#' @examples
add_transitions <- function(femg_data, marker_channel) {
  femg_data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      transition_start = diff_from_prev(.data[[marker_channel]]),
      transition_end = diff_from_next(.data[[marker_channel]])
    )
}
