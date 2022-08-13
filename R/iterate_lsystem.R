#' Rewrite an Axiom Using Production Rules to Give a String Ready for Turtle Graphics
#'
#' @param init A character string giving variables (symbols) to use as the initial string, also known as the axiom.
#' @param rules A data frame containing columns "inp" and "out". These contain the input variables and the corresponding replacement string.
#' @param n An integer giving the number of cycles or iterations desired.
#' @param verbose An integer giving the level of information desired as the calculation proceeds. verbose = 1L gives basic information at each cycle. Any value greater than 1 gives much more detail. Supress messages by using a value less than 1.
#'
#' @export
iterate_lsystem <- function(init = NULL, rules = NULL, n = 5,
                            verbose = 0L) {

  nc <- nchar(rules$inp)
  if (any(nc > 1)) stop("Input variables must be a single character")

  if (verbose == 1L) cat("\nCycle 0 string has length ", nchar(init), "\n", sep = "")
  if (verbose == 1L) cat("Cycle 0:", init, "\n")
  curr <- init
  out <- rep(NA_character_, n+1) # save init and all output
  out[1] <- init

  for (j in 1:n) {
    # apply all the rules simultaneously to the current string
    # this is different than any built-in string processing
    # I can find in R

    RR <- vector("list") # save the rule output here
    for (i in 1:nrow(rules)) {
      rr <- stringr::str_locate_all(curr, rules[i,1])
      if (verbose > 1L) cat("Processing rule", i, "\n")
      if (dim(rr[[1]])[1] == 0) {
        if (verbose > 1L) cat("\tRule", i, "was not needed\n")
        next
      }
      RR[i] <- rr
    }

    # reformat RR into something more useful
    if (verbose >= 1L) print(RR)


    for (i in 1:length(RR)) {
      tmp <- as.data.frame(RR[i])
      if (is.null(tmp[1,1])) {

        next # empty output as rule didn't apply
      }

      rule_i <- rules[i,2]
      if(grepl(',', rule_i)){
        rules_i <- stringr::str_split(rule_i, ',', simplify = T)
        rule_i <- sample(rules_i, 1)
      }

      tmp$insert <- rule_i
      RR[[i]] <- tmp
    }

    if (verbose >= 1L) print(RR)

    # RR is still a list, unlist to a data frame

    RRdf <- as.data.frame(RR[1])
    for (i in 2:length(RR)) {
      RRdf <- rbind(RRdf, as.data.frame(RR[i]))
    }

    if (verbose > 1L) print(RRdf)

    # assemble a new string using the rules

    curr <- unlist(strsplit(curr, ""))
    curr[RRdf$start] <- RRdf$insert
    curr <- paste0(curr, collapse = "")
    out[j+1] <- curr
    if (verbose == 1L) cat("\nCycle ", j, " string has length ", nchar(curr), "\n", sep = "")
    if (verbose == 1L) cat("Cycle ", j, ": ", curr, "\n", sep = "")
  }

  curr
}
