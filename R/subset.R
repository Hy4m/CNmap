#' @title Subset Map Data
#' @description A set of functions to subset of CNmap.
#' @param map map data in `CNmap` package.
#' @param ... other parameters passing to `*_id()` function.
#' @return a sf object.
#' @rdname extract
#' @author Hou Yun
#' @export
extract_province <- function(map, ...) {
  id <- province_id(..., na.rm = TRUE)
  map[map$ID %in% id, , drop = FALSE]
}

#' @rdname extract
#' @export
extract_city <- function(map, ...) {
  id <- city_id(..., na.rm = TRUE)
  map[map$ID %in% id, , drop = FALSE]
}

#' @rdname extract
#' @export
extract_county <- function(map, ...) {
  id <- county_id(..., na.rm = TRUE)
  map[map$ID %in% id, , drop = FALSE]
}


#' @title Extract ID
#' @description Helper functions to extract ID of CNmap by numeric-id or name-id.
#' @param ... valid numeric id or name.
#' @param output 'num' means return numeric id, and 'chr' means return name id.
#' @param grep logical, if `TRUE` (default) will use regular expressions.
#' @param na.rm logical, if `TRUE` will remove `NA` value.
#' @return character vector.
#' @rdname map_id
#' @author Hou Yun
#' @export
province_id <- function(...,
                        output = c("num", "chr"),
                        grep = TRUE,
                        na.rm = TRUE) {
  id <- unlist(list(...))
  if (length(id) < 1L) {
    return(character(0))
  }
  output <- match.arg(output)

  out <- rep_len(NA_character_, length(id))
  for (ii in seq_along(id)) {
    x <- id[ii]
    x <- suppressWarnings(
      if (is.na(as.integer(x))) as.character(x) else as.integer(x)
    )
    if (is.na(x)) next
    if (is.character(x)) {
      nm <- CNmap_province$name
      valid <- if (isTRUE(grep)) {
        grepl(x, nm)
      } else {
        nm == x
      }
      x <- as.integer(CNmap_province$ID[which(valid)])
    }
    x <- get_id(x, "province", na.rm = na.rm)
    if (length(x) < 1L) next

    if (output == "chr") {
      out[ii] <- CNmap_province$name[which(CNmap_province$ID == as.character(x))]
    } else {
      out[ii] <- x
    }
  }

  out
}

#' @rdname map_id
#' @export
city_id <- function(...,
                    province = NULL,
                    output = c("num", "chr"),
                    grep = TRUE,
                    na.rm = TRUE) {
  id <- unlist(list(...))
  if (length(id) < 1L && is.null(province)) {
    return(character(0))
  }
  output <- match.arg(output)

  map <- CNmap_city
  pid <- FALSE
  if (!is.null(province)) {
    pid <- vapply(province, province_id, character(1),
                  na.rm = na.rm, USE.NAMES = FALSE)
    if (length(pid) >= 1L) {
      pid <- get_id(map$ID, "province") %in% pid
    } else {
      pid <- FALSE
    }
  }

  cid <- FALSE
  for (ii in seq_along(id)) {
    x <- id[ii]
    x <- suppressWarnings(
      if (is.na(as.integer(x))) as.character(x) else as.integer(x)
    )

    if (is.na(x)) next
    if (is.integer(x)) {
      temp <- get_id(map$ID, "city") %in% get_id(x, "city")
      cid <- cid | temp
    } else {
      temp <- if (isTRUE(grep)) {
        grepl(x, map$name)
      } else {
        map$name == x
      }
      cid <- cid | temp
    }
  }

  if (output == "num") {
    map$ID[pid | cid]
  } else {
    map$name[pid | cid]
  }
}

#' @rdname map_id
#' @export
county_id <- function(...,
                      province = NULL,
                      city = NULL,
                      output = c("num", "chr"),
                      grep = TRUE,
                      na.rm = TRUE) {
  id <- unlist(list(...))
  if (length(id) < 1L && is.null(province) && is.null(city)) {
    return(character(0))
  }
  output <- match.arg(output)

  map <- CNmap_county
  pid <- FALSE
  if (!is.null(province)) {
    pid <- vapply(province, province_id, character(1),
                  na.rm = na.rm, USE.NAMES = FALSE)
    if (length(pid) >= 1L) {
      pid <- get_id(map$ID, "province") %in% pid
    } else {
      pid <- FALSE
    }
  }

  cid <- FALSE
  if (!is.null(city)) {
    cid <- vapply(city, city_id, character(1),
                  na.rm = na.rm, USE.NAMES = FALSE)
    if (length(cid) >= 1L) {
      cid <- get_id(map$ID, "city") %in% cid
    } else {
      cid <- FALSE
    }
  }

  xid <- FALSE
  for (ii in seq_along(id)) {
    x <- id[ii]
    x <- suppressWarnings(
      if (is.na(as.integer(x))) as.character(x) else as.integer(x)
    )

    if (is.na(x)) next
    if (is.integer(x)) {
      temp <- get_id(map$ID, "county") %in% get_id(x, "county")
      xid <- xid | temp
    } else {
      temp <- if (isTRUE(grep)) {
        grepl(x, map$name)
      } else {
        map$name == x
      }
      xid <- xid | temp
    }
  }

  if (output == "num") {
    map$ID[pid | cid | xid]
  } else {
    map$name[pid | cid | xid]
  }
}

#' @noRd
get_id <- function(id,
                   level = c("province", "city", "county"),
                   na.rm = TRUE) {
  id <- as.integer(id)
  level <- match.arg(level)

  id <- vapply(id, function(x) {
    if (level == "province") {
      if (is.na(x)) {
        NA_character_
      } else if (x > 10000L) {
        x <- as.character((as.integer(x) %/% 10000L) * 10000L)
      } else if (x > 1000L) {
        x <- as.character((as.integer(x) %/% 100L) * 10000L)
      } else {
        x <- as.character(as.integer(x) * 10000L)
      }
      x <- if (x %in% CNmap_province$ID) x else NA_character_
    } else if (level == "city") {
      if (is.na(x)) {
        NA_character_
      } else if (x > 10000L) {
        x <- as.character((as.integer(x) %/% 100L) * 100L)
      } else {
        x <- as.character(as.integer(x) * 100L)
      }
      x <- if (x %in% CNmap_city$ID) x else NA_character_
    } else {
      x <- as.character(x)

      if (x %in% CNmap_county$ID) x else NA_character_
    }
  }, character(1), USE.NAMES = FALSE)

  if (isTRUE(na.rm)) {
    id[!is.na(id)]
  } else {
    id
  }
}
