#' Internal Function
#'
#' This is an internal function, not for users.
#'
#' @param dataVct DataVct
#' @param value Value
#' @param nth Nth
#' @param reverse Reverse
#'
#' @return loc
#'
#' @templateVar author liu
#' @template auth
#'
#' @keywords internal

nthOccurance <- function(dataVct, value, nth = NA, reverse = FALSE) {
    if(reverse) {
        dataVct <- rev(dataVct)
    }
    if(is.na(value)) {
        value <- "NA"
        dataVct[is.na(dataVct)] <- "NA"
    }
    loc <- which(dataVct == value)
    if(!is.na(nth)) {
        loc <- loc[nth]
    }
    if(reverse) {
        loc <- length(dataVct) - loc + 1
    }
    if(all(is.na(loc))) {
        loc <- 0
    }
    loc
}
