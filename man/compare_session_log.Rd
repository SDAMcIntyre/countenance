% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_session_log.R
\name{compare_session_log}
\alias{compare_session_log}
\title{Compare a Marker channel in fEMG data to a sequence of markers in a session log file}
\usage{
compare_session_log(
  femg_data,
  data_marker_channel,
  data_time_channel,
  session_log,
  log_marker_channel,
  log_start_time,
  log_end_time = NULL,
  log_duration = NULL,
  fill_marker_values = FALSE
)
}
\arguments{
\item{femg_data}{data frame or tibble}

\item{data_marker_channel}{the name of the column in femg_data that corresponds to the marker channel}

\item{data_time_channel}{the name of the column in femg_data that gives the time in seconds}

\item{session_log}{data frame or tibble}

\item{log_marker_channel}{the name of the column in session_log that corresponds to the marker channel}

\item{log_start_time}{the name of the column in session_log that has the start times of the markers}

\item{log_end_time}{The name of the column in session_log that has the end times of the markers. Must be provided if log_duration is NULL.}

\item{log_duration}{The name of the column in session_log that has the durations of the markers. Must be provided if log_end_time is NULL.}

\item{fill_marker_values}{set to TRUE if you want to fill marker values in femg_data based on the duration in the session log}
}
\value{
a list:

session_log_aligned

synced

start_offsets

end_offsets

comparison_plot

femg_data_filled_markers (if fill_marker_values = TRUE)

filled_plot (if fill_marker_values = TRUE)
}
\description{
Compare a Marker channel in fEMG data to a sequence of markers in a session log file
}
