#' City Level Map of China
#'
#' City level map of China, and only contain province level map for Beijing,
#' Shanghai, Tianjin, Chongqing, Hongkong, Macau or Taiwan.
#'
#' @format ## `CNmap_city`
#' A sf tibble with 370 rows and 4 columns:
#' \describe{
#'   \item{ID}{Geocoding of each city.}
#'   \item{name}{City name in Chinese.}
#'   \item{level}{`city` level.}
#'   \item{geometry}{list column of simple feature.}
#' }
#' @source <https://github.com/Civitasv/DataV_GeoJSON>
"CNmap_city"
