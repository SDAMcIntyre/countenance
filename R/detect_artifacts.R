roll_range <- function(x, ...) {
  Rcpp::roll_max(x, ...) - Rcpp::roll_min(x, ...)
}

#' Detect artifacts in fEMG
#'
#' Artifact rejection procedure as described in Künecke et. al. (2014). Data samples are flagged if the range within a sliding 50ms window exceeds three times the standard deviation of the full session recording.
#'
#' Künecke, J., Hildebrandt, A., Recio, G., Sommer, W., & Wilhelm, O. (2014). Facial EMG Responses to Emotional Expressions Are Related to Emotion Perception Ability. PLoS ONE, 9(1), e84053. https://doi.org/10.1371/journal.pone.0084053
#'
#'
#' @param femg_data data frame or tibble
#' @param muscle_channels A vector with the names of the channels in the data that contain EMG data to be checked for artifacts.
#' @param win_sec 0.05 sec by default. The duration of the sliding window in which to look for extreme values.
#' @param flag_threshold 3 by default. The multiple of SD beyond which the range of data in win_sec is considered a likely artifact.
#'
#' @return data frame or tibble with additional variables:
#'
#' (muscle_channel)_z: z transformation of the muscle data
#'
#' (muscle_channel)_zrange: range of the window of duration win_sec centred on this sample
#'
#' (muscle_channel)_flagged: boolean indicating if the window centered on this sample contains artifactual data
#'
#' (muscle_channel)_fixed: same data as muscle_channel but flagged samples are set to NA
#'
#' (muscle_channel)_zfixed: same data as muscle_channel_z but flagged samples are set to NA
#'
#' @export
#'
#' @examples
detect_artifacts <- function(femg_data, muscle_channels, win_sec = 0.05, flag_threshold = 3) {

  sample_duration <- diff(femg_data$stimTime.sec[1:2])

  n_samples <- win_sec/sample_duration

  for (muscle in muscle_channels) {

    raw_var <- names(femg_data) %>% stringr::str_subset(muscle)
    name_z <- paste0(muscle,'_z')
    name_zrange <- paste0(name_z,'_zrange')
    name_flagged <- paste0(muscle, '_flagged')
    name_rawfixed <- paste0(muscle, '_fixed')
    name_zfixed <- paste0(muscle, '_zfixed')

    femg_data <- femg_data %>%
      dplyr::mutate(!!name_z := scale(.[[raw_var]])[,1]) %>%
      dplyr::mutate(!!name_zrange := roll_range(.[[name_z]], n = n_samples, fill = NA)) %>%
      dplyr::mutate(!!name_flagged := abs(.[[name_zrange]]) > flag_threshold) %>%
      dplyr::mutate(!!name_flagged := tidyr::replace_na(.[[name_flagged]], FALSE)) %>%

      dplyr::mutate(!!name_rawfixed := dplyr::if_else(
        .[[name_flagged]],
        NA_real_,
        .[[raw_var]]
        )) %>%

      dplyr::mutate(!!name_zfixed := dplyr::if_else(
        .[[name_flagged]],
        NA_real_,
        .[[name_z]]
        ))

  }
  return(femg_data)
}
