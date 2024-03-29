% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_transitions.R
\name{add_transitions}
\alias{add_transitions}
\title{Add a column to a data frame indicating transitions in a marker channel}
\usage{
add_transitions(femg_data, marker_channel)
}
\arguments{
\item{femg_data}{data frame or tibble}

\item{marker_channel}{the name of the column in femg_data that corresponds to the marker channel}
}
\value{
femg_data with extra columns "transition_start" and "transition_end"
}
\description{
Add a column to a data frame indicating transitions in a marker channel
}
