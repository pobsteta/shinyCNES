% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_s2_list.R
\name{s2_list}
\alias{s2_list}
\title{s2_list}
\usage{
s2_list(
  session,
  spatial_extent = NULL,
  tiles = NULL,
  orbit = NULL,
  time_interval = NULL,
  time_period = "full",
  level = "L2A",
  platform = "SENTINEL2A",
  maxcloud = 101,
  collection = "SENTINEL",
  path_to_download = "~",
  download = TRUE,
  project_name = NULL,
  progress_bar = TRUE
)
}
\arguments{
\item{spatial_extent}{Spatial extent}

\item{orbit}{Orbit}

\item{time_interval}{Time interval}

\item{time_period}{Time period}

\item{level}{Level}

\item{platform}{Plateform}

\item{maxcloud}{Max cloud cover}

\item{collection}{Collection}

\item{path_to_download}{Path to the project}

\item{tile}{Tiles}
}
\value{
List of products
}
\description{
s2_list
}
