### All the code in this section is modified from GITHUB: wandergis/coordtransform
### Source URL: https://github.com/wandergis/coordtransform/blob/master/index.js

#' @title Transform coordinate reference system
#' @description A set of tools to convert points to points with another
#' coordinate reference system, and it's useful for Chinese users only.
#' @param lng numeric vector of longitude.
#' @param lat numeric vector of latitude.
#' @return a new data frame of longitude and latitude.
#' @rdname transform
#' @export
bd09_to_gcj02 <- function(lng, lat) {
  n <- max(length(lng), length(lat))
  lng <- rep_len(lng, n)
  lat <- rep_len(lat, n)
  id <- need_trans(lng, lat)
  out <- data.frame(lng = lng, lat = lat)
  if (sum(id) == 0) {
    return(out)
  }

  lng <- lng[id] - 0.0065
  lat <- lat[id] - 0.006
  z <- sqrt(lng^2 + lat^2) - 0.00002 * sin(lat * CONST$PI)
  theta <- atan2(lat, lng) - 0.000003 * cos(lng * CONST$PI)
  new <- data.frame(lng = z * cos(theta),
                    lat = z * sin(theta))
  out[id, ] <- new
  out
}

#' @rdname transform
#' @export
gcj02_to_bd09 <- function(lng, lat) {
  n <- max(length(lng), length(lat))
  lng <- rep_len(lng, n)
  lat <- rep_len(lat, n)
  id <- need_trans(lng, lat)
  out <- data.frame(lng = lng, lat = lat)
  if (sum(id) == 0) {
    return(out)
  }

  lng <- lng[id]
  lat <- lat[id]
  z <- sqrt(lng^2 + lat^2) + 0.00002 * sin(lat * CONST$PI)
  theta <- atan2(lat, lng) + 0.000003 * cos(lng * CONST$PI)
  new <- data.frame(lng = z * cos(theta) + 0.0065,
                    lat = z * sin(theta) + 0.006)
  out[id, ] <- new
  out
}

#' @rdname transform
#' @export
wgs84_to_gcj02 <- function(lng, lat) {
  n <- max(length(lng), length(lat))
  lng <- rep_len(lng, n)
  lat <- rep_len(lat, n)
  id <- need_trans(lng, lat)
  out <- data.frame(lng = lng, lat = lat)
  if (sum(id) == 0) {
    return(out)
  }

  lng <- lng[id]
  lat <- lat[id]

  lng2 <- trans_lng(lng - 105, lat - 35)
  lat2 <- trans_lat(lng - 105, lat - 35)
  radian_lat <- lat/180*pi
  m <- 1 - CONST$E * sin(radian_lat)^2
  sqrt_m <- sqrt(m)
  new <- data.frame(lng = lng + (lng2*180)/(CONST$A/sqrt_m*cos(radian_lat)*pi),
                    lat = lat + (lat2*180)/((CONST$A*(1 - CONST$E))/(m*sqrt_m)*pi))
  out[id, ] <- new
  out
}

#' @rdname transform
#' @export
gcj02_to_wgs84 <- function(lng, lat) {
  n <- max(length(lng), length(lat))
  lng <- rep_len(lng, n)
  lat <- rep_len(lat, n)
  id <- need_trans(lng, lat)
  out <- data.frame(lng = lng, lat = lat)
  if (sum(id) == 0) {
    return(out)
  }

  lng <- lng[id]
  lat <- lat[id]

  lng2 <- trans_lng(lng - 105, lat - 35)
  lat2 <- trans_lat(lng - 105, lat - 35)
  radian_lat <- lat/180*pi
  m <- 1 - CONST$E * sin(radian_lat)^2
  sqrt_m <- sqrt(m)
  lat2 <- (lat2*180)/((CONST$A*(1 - CONST$E))/(m*sqrt_m)*pi)
  lng2 <- (lng2*180)/(CONST$A/sqrt_m*cos(radian_lat)*pi)
  new <- data.frame(lng = lng - lng2,
                    lat = lat - lat2)
  out[id, ] <- new
  out
}

#' @noRd
CONST <- list(PI = pi * 3000 / 180,
              A = 6378245,
              E = 0.00669342162296594323)

#' @noRd
need_trans <- function(lng, lat) {
  id <- (lat >= 3.86 & lat <= 53.55) & (lng >= 73.66 & lng <= 135.05)
  vapply(id, isTRUE, logical(1))
}

#' @noRd
trans_lat <- function(lng, lat) {
  ret <- -100 + 2*lng + 3*lat + 0.2*lat^2 + 0.1*lng*lat + 0.2*sqrt(abs(lng))
  ret <- ret + (20*sin(6*lng*pi) + 20*sin(2*lng*pi))*2/3
  ret <- ret + (20*sin(lat*pi) + 40*sin(lat/3*pi))*2/3
  ret <- ret + (160*sin(lat/12*pi) + 320*sin(lat*pi/30))*2/3
  ret
}

#' @noRd
trans_lng <- function(lng, lat) {
  ret <- 300 + lng + 2*lat + 0.1*lng^2 + 0.1*lng*lat + 0.1*sqrt(abs(lng))
  ret <- ret + (20*sin(6*lng*pi) + 20*sin(2*lng*pi))*2/3
  ret <- (20*sin(lng*pi) + 40*sin(lng/3*pi))*2/3
  ret <- (150*sin(lng/12*pi) + 300*sin(lng/30*pi))*2/3
  ret
}

