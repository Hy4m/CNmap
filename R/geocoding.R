#' @title Geocoding and it's reverse
#' @description Call Baidu map API to obtain geocoding and it's reverse.
#' @param x names of location.
#' @param lng,lat longitude and latitude.
#' @param ... other parameters passing to API.
#' @param api Baidu Map API key.
#' @return \code{get_coordinate()} return a data frame, and \code{get_address()}
#' return a character vector.
#' @rdname geocoding
#' @export
get_coordinate <- function(x, ..., api = NULL) {
  if (is.null(api)) {
    stop("Please provide your own Baidu Map API key.", call. = FALSE)
  }

  x <- as.character(x)
  out <- data.frame(name = character(0),
                    lng = numeric(0),
                    lat = numeric(0))
  if (length(x) == 0) {
    return(out)
  }

  URL <- "https://api.map.baidu.com/geocoding/v3"
  query <- rlang::list2(...)
  for (ii in x) {
    if (is.na(ii)) {
      out <- rbind(out, data.frame(name = ii,
                                   lng = NA_real_,
                                   lat = NA_real_))
    } else {
      q <- c(list(address = ii, output = "json", ak = api), query)
      url <- httr::modify_url(url = URL, query = q)
      html <- suppressMessages(httr::content(httr::GET(url = url),
                                             type = "application/json"))
      if (html$status != 0) {
        out <- rbind(out, data.frame(name = ii,
                                     lng = NA_real_,
                                     lat = NA_real_))
      } else {
        out <- rbind(out, data.frame(name = ii,
                                     lng = html$result$location$lng,
                                     lat = html$result$location$lat))
      }
    }
  }
  out
}

#' @rdname geocoding
#' @export
get_address <- function(lng, lat, ..., api = NULL) {
  if (is.null(api)) {
    stop("Please provide your own Baidu Map API key.", call. = FALSE)
  }

  n <- max(length(lng), length(lat))
  if (n == 0) {
    return(character(0))
  }

  lng <- rep_len(lng, n)
  lat <- rep_len(lat, n)
  URL <- "https://api.map.baidu.com/reverse_geocoding/v3"
  query <- rlang::list2(...)

  out <- character(0)
  for (ii in seq_len(n)) {
    q <- c(list(location = paste0(lat[ii], "," ,lng[ii]),
                output = "json",
                ak = api),
           query)
    url <- httr::modify_url(url = URL, query = q)
    html <- suppressMessages(httr::content(httr::GET(url = url),
                                           type = "application/json"))
    if (html$status != 0) {
      out <- c(out, NA_character_)
    } else {
      out <- c(out, html$result$formatted_address)
    }
  }
  out
}
