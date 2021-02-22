#' Title
#'
#' @return
#'
#'
#' @examples
assert <- function(year,volume,number,silence,full_text) {
  tryCatch(
    assertthat::assert_that(is.numeric(year)),
    error = function(e) {
      stop("'year' must be double", call. = FALSE)
    }
  )

  tryCatch(
    assertthat::assert_that(is.numeric(volume)),
    error = function(e) {
      stop("'volume' must be numeric", call. = FALSE)
    }
  )

  tryCatch(
    assertthat::assert_that(is.numeric(number)),
    error = function(e) {
      stop("'number' must be numeric", call. = FALSE)
    }
  )

  tryCatch(
    assertthat::assert_that(is.logical(silence)),
    error = function(e) {
      stop("'silence' must be logical", call. = FALSE)
    }
  )

  tryCatch(
    assertthat::assert_that(is.logical(full_text)),
    error = function(e) {
      stop("'full_text' must be logical", call. = FALSE)
    }
  )
}
