#' Build a 2D L-System Tree from Pre-defined Tree Templates
#'
#' @param tree_model The integer or the name (character) that refers to the desired tree model. Check details.
#' @param n_iter Integer. The number of iterations to build the tree. Each tree model has its own default value.
#'
#' @details There are 10 pre-implemented tree templates in this function, as follows (default `n_iter` between parentheses):
#' - 1 or "binary_tree" (6);
#' - 2 or "alternate_tree" (5);
#' - 3 or "arrow_weed" (5);
#' - 4 or "twiggy_weed" (5);
#' - 5 or "stochastic_fuzzy_weed" (4);
#' - 6 or "crooked_binary_tree" (6);
#' - 7 or "crooked_alternate_tree" (5);
#' - 8 or "crooked_arrow_weed" (5);
#' - 9 or "crooked_twiggy_weed" (5);
#' - 10 or "crooked_stochastic_fuzzy_weed" (4).
#'
#' @export
fractal_tree_model <- function(tree_model, n_iter){

  if(tree_model %in% list(1,"binary_tree")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]+(0)", "1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 6}

  }

  else if(tree_model %in% list(2,"alternate_tree")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]1[+(0)](0),1[+(0)]1[-(0)](0)", "1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(3,"arrow_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[+(0)][-(0)](10)", "1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(4,"twiggy_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]1[-(0)]+(0)", "1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(5,"stochastic_fuzzy_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1-[[(0)]+(0)]+1[+(10)]-(0),1+[[(0)]-(0)]-1[-(10)]+(0)","1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 4}

  }

  if(tree_model %in% list(6,"crooked_binary_tree")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]+(0)", "+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 6}

  }

  else if(tree_model %in% list(7,"crooked_alternate_tree")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]1[+(0)](0),1[+(0)]1[-(0)](0)", "+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(8,"crooked_arrow_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[+(0)][-(0)](10)", "+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(9,"crooked_twiggy_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1[-(0)]1[-(0)]+(0)", "+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 5}

  }

  else if(tree_model %in% list(10,"crooked_stochastic_fuzzy_weed")){
    rules <- data.frame(
      inp = c("0", "1"),
      out = c("1-[[(0)]+(0)]+1[+(10)]-(0),1+[[(0)]-(0)]-1[-(10)]+(0)","+-1"),
      stringsAsFactors = FALSE
    )

    if(missing(n_iter)){n_iter <- 4}

  }

  fractalforest::iterate_lsystem(init = '0', rules = rules, n_iter = n_iter)
}
