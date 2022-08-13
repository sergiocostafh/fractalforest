#' Check if character is a branch symbol
#'
#' @param x an R object to be tested.
#'
#' @export
is_branch_symbol <- function(x) {!x %in% c('[',']','+','-','(',')')}
