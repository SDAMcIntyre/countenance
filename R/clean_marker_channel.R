
#' Denoise
#'
#' Internal function called by clean_marker_channel().
#'
#' @param marker_channel a vector, the marker channel to denoise
#' @param denoise_width an integer or "auto". The number of samples for which to take the median as a method to remove individual samples with unexpected values.
#' @param values_to_ignore a vector of values in marker_channel that should not be subject to denoising
#'
#' @return a list of flagged indices, and the full cleaned marker channel
#' @export
#'
#' @examples
denoise <- function(marker_channel, denoise_width = 'auto', values_to_ignore = c(0)) {

  # automatically determine the denoise width
  if (denoise_width == 'auto') {
    # look at the data to find out how long the stimulus signal is
    runs <- rle(marker_channel)
    raw_marker_runs <- dplyr::tibble(
      lengths = runs$lengths,
      marker_values = runs$values
      )
    med_n_samples_per_value <- raw_marker_runs %>%
      dplyr::filter(.data$marker_values %in% values_to_ignore == FALSE) %>%
      dplyr::pull(lengths) %>% stats::median()
    denoise_width = ceiling(med_n_samples_per_value/2)
  }

  # look forwards
  #get the median of the last window in the dataset
  last_win_median <- stats::median(marker_channel[(length(marker_channel)-denoise_width):length(marker_channel)])

  # get the rolling median of the data, left-aligned, pad it with the
  # median of the last window so that it is the same length as the data
  next_n_med <- RcppRoll::roll_medianl(marker_channel, denoise_width, fill = last_win_median)

  # shift the medians back by one position so that each value in the
  # dataset aligns with the median of the window of data ahead of it ("looking ahead")
  next_n_med <- c(next_n_med[-1], next_n_med[length(next_n_med)])

  # look backwards (do the same as above but right aligned medians)
  first_win_median <- stats::median(marker_channel[1:denoise_width])
  prev_n_med <- RcppRoll::roll_medianr(marker_channel, denoise_width, fill = first_win_median)
  #shift it one forwards so it doesn't include itself
  prev_n_med <- c(prev_n_med[1], prev_n_med[-length(prev_n_med)])

  # check if it's both different from the median of the previous n samples and
  #from the median of the next n samples
  flagged <- marker_channel != prev_n_med & marker_channel != next_n_med

  cleaned <- marker_channel
  cleaned[flagged] <- next_n_med[flagged]

  return(list('flagged' = flagged, 'cleaned' = cleaned))

}

#' Clean the marker channel of an fEMG recording
#'
#' @param femg_data data frame or tibble
#' @param marker_channel the name of the column in femg_data that corresponds to the marker channel
#' @param off_marker_value the value of the marker channel indicating no stimulus/task (default = 0)
#' @param valid_marker_values valid marker values to be found in marker_channel that are used to indicate the stimulus/task
#' @param invalid_marker_values invalid marker values that can safely be set to off_marker_value
#' @param denoise_width an integer or "auto". The number of samples for which to take the median as a method to remove individual samples with unexpected values.
#'
#' @return femg_data with additional column (marker_channel)_cleaned
#' @export
#'
#' @examples
#' clean_marker_channel(
#'     femg_raw_data_sample,
#'     marker_channel = "Marker",
#'     valid_marker_values = c(54)
#'     )
clean_marker_channel <- function(
  femg_data,
  marker_channel,
  off_marker_value = 0,
  valid_marker_values,
  invalid_marker_values = c(),
  denoise_width = 'auto'
  ) {

  denoised <- denoise(
    dplyr::pull(femg_data[marker_channel]),
    denoise_width = denoise_width,
    values_to_ignore = c(off_marker_value, invalid_marker_values)
    )

  name_marker_channel_cleaned <- paste(marker_channel, "cleaned", sep = "_")

  femg_data %>%
    dplyr::mutate(
      # try to flag and correct unknown noise/debounce errors:
      Stim.flag.noise = denoised$flagged,
      !!name_marker_channel_cleaned := denoised$cleaned) %>%
    dplyr::mutate(
    #   #set known noise codes to 0
      !!name_marker_channel_cleaned := replace(
        .data[[name_marker_channel_cleaned]], # vector to find values to replace
        .data[[name_marker_channel_cleaned]] %in% invalid_marker_values, # condition
        off_marker_value), # replace with
      # is the stim code an unexpected one?
      unexpected = .data[[name_marker_channel_cleaned]] %in%  c(off_marker_value, valid_marker_values) == FALSE
    )
}

