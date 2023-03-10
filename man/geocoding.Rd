% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocoding.R
\name{get_coordinate}
\alias{get_coordinate}
\alias{get_address}
\title{Geocoding and it's reverse}
\usage{
get_coordinate(x, ..., api = NULL)

get_address(lng, lat, ..., api = NULL)
}
\arguments{
\item{x}{names of location.}

\item{...}{other parameters passing to API.}

\item{api}{Baidu Map API key.}

\item{lng, lat}{longitude and latitude.}
}
\value{
\code{get_coordinate()} return a data frame, and \code{get_address()}
return a character vector.
}
\description{
Call Baidu map API to obtain geocoding and it's reverse.
}
