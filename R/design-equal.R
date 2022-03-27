

#' Test if two designs are equivalent
#'
#' Given two data frames that contain the design of experiment output, test
#' to see if the design is equivalent.
#'
#' @param x,y A data frame with the design of experiment table
#' @param ... A series of name-value pair specifying which columns in `x` and
#'   `y` are equivalent. The name should be the name of the column in `x` and
#'   value should be the name of column in `y`.
#'
#' @examples
#' \dontrun{
#' library(edibble)
#' des1 <- design() %>%
#'   set_units(plot = 30) %>%
#'   set_trts(trt = 5) %>%
#'   allot_table(trt ~ plot)
#'
#' des2 <- design() %>%
#'   set_units(subject = 30) %>%
#'   set_trts(vaccine = 5) %>%
#'   allot_table(vaccine ~ subject)
#'
#' design_equal(des1, des2,
#'              plot = subject, trt = vaccine)
#' }
#' @export
design_equal <- function(x, y, ...) {
  dots <- ensyms(...)
  xcols <- names(dots)
  ycols <- unname(vapply(dots, rlang::as_string, character(1)))
  for(i in seq_along(xcols)) {
    xcount <- table(x[[xcols[i]]])
    ycount <- table(y[[ycols[i]]])
    testthat::expect_true(any(xcount[order(xcount)]==ycount[order(ycount)]))
  }
}
