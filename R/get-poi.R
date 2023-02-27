#' @title Get POI
#' @description Location search service.
#' @param query key words of query.
#' @param region region of query.
#' @param ... other parameters passing to API.
#' @param api Baidu Map API key.
#' @return a data frame.
#' @rdname get_poi
#' @export
get_poi <- function(query, region = 340, ..., api = NULL) {
  if (is.null(api)) {
    stop("Please provide your own Baidu Map API key.", call. = FALSE)
  }

  query <- paste0(query, collapse = ",")
  region <- paste0(region, collapse = ",")

  URL <- "https://api.map.baidu.com/place/v2/search"
  query <- c(list(query = query,
                  region = region,
                  output = "json",
                  ak = api),
             rlang::list2(...))
  url <- httr::modify_url(URL, query = query)
  html <- suppressMessages(httr::content(httr::GET(url = url),
                                         type = "application/json"))

  if (html$status != 0) {
    out <- data.frame()
  } else {
    r <- html$results
    out <- data.frame(name = vapply(r, function(x) x$name, character(1)),
                      lng = vapply(r, function(x) x$location$lng, numeric(1)),
                      lat = vapply(r, function(x) x$location$lat, numeric(1)),
                      address = vapply(r, function(x) x$address, character(1)))
  }
  out
}
