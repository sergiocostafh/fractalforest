#' Build a 2D L-System Tree Data Frame Using Turtle Graphics
#'
#' @param string A character vector giving the strings containing the turtle graphics instructions. Created by iterate_lsystem function.
#' @param height Total height of the plant.
#' @param diameter Base diameter of the plant.
#' @param h_reduction Numeric. Reduction rate to be applied to the length of the branches whenever the symbols '(' and ')' appear in the string. Default is the golden ratio.
#' @param d_reduction Numeric. Reduction rate to be applied to the diameter of the branches whenever the symbols '(' and ')' appear in the string. Default is the golden ratio.
#' @param angle Numeric. The angle in degrees when a change in direction is requested
#' @param randomness Logical. Should randomness be applied to the angle of the branches?
#' @param angle_cv Numerical. The coefficient of variation of the angle, used when randomness = TRUE. Default is 0.1,
#' @param length_cv Numerical. The coefficient of variation of the length, used when randomness = TRUE. Default is 0.1,
#' @param leaf_size Size of leaves (branches from the ends of the plant). Default is NULL (follows the applied d_reduction value).
#'
#' @importFrom magrittr %>%
#' @importFrom stats rnorm
#' @importFrom purrr detect_index
#'
#' @export
build_tree <- function (string = NULL, angle = 15,
                        height = NULL, diameter = NULL,
                        h_reduction = (1+sqrt(5))/2-1, d_reduction = (1+sqrt(5))/2-1,
                        randomness = FALSE, angle_cv = .1, length_cv = .1,leaf_size = NULL) {

  uniques <- strsplit(string, "") %>% unlist() %>% unique()

  Fs <- uniques[which(!uniques %in% c("+", "-", "[", "]", "(", ")"))]

  drules <- data.frame(sym = uniques) %>%
    dplyr::mutate(action = dplyr::if_else(sym %in% Fs, "F", sym))

  sring <- unlist(strsplit(string, ""))
  for (i in 1:nrow(drules)) {
    for (j in 1:length(sring)) {
      if (sring[[j]] == drules$sym[i])
        sring[[j]] <- drules$action[i]
    }
  }
  st <- c(0, 0, 90)
  cp <- st
  ch <- st[3]

  fifo <- vector("list")
  prop_red <- vector("list")
  ns <- 0L
  pr <- c(1, 1)

  angle_sd <- angle * angle_cv

  n <- 0

  sring <- sring %>% paste0(collapse='') %>%
    stringi::stri_replace_all_fixed('[',';[;') %>%
    stringi::stri_replace_all_fixed(']',';];') %>%
    stringi::stri_replace_all_fixed('+',';+;') %>%
    stringi::stri_replace_all_fixed('-',';-;') %>%
    stringi::stri_replace_all_fixed('(',';(;') %>%
    stringi::stri_replace_all_fixed(')',';);') %>%
    stringr::str_split(pattern = ';') %>%
    unlist() %>%
    lapply(.,
           function(x){
             if(!x %in% c('[',']','+','-','(',')','')){
               return(nchar(x))
             }
             else{x}
           }
    ) %>%
    paste0(collapse = '') %>%
    stringi::stri_replace_all_fixed('[',';[;') %>%
    stringi::stri_replace_all_fixed(']',';];') %>%
    stringi::stri_replace_all_fixed('+',';+;') %>%
    stringi::stri_replace_all_fixed('-',';-;') %>%
    stringi::stri_replace_all_fixed('(',';(;') %>%
    stringi::stri_replace_all_fixed(')',';);') %>%
    stringr::str_split(pattern = ';') %>%
    unlist()

  sring <- sring[-which(sring == '')] %>% as.list()
  output <- vector("list", length(which(!is.na(as.numeric(sring)))))

  if(is.null(height)){
    height <- 1
  }
  if(is.null(diameter)){
    diameter <- 1
  }

  for (j in 1:length(sring)) {

    if (!is.na(as.numeric(sring[[j]]))) {
      n <- n+1

      step <- as.numeric(sring[[j]]) * pr[1]

      if(randomness){ step <- stats::rnorm(1, step, step * length_cv)}

      x <- cp[1] + step * cos(ch * pi/180)
      y <- cp[2] + step * sin(ch * pi/180)

      d <- diameter * pr[2]


      if(
        j == length(sring) # if is the last element, is a leaf
      ){
        tp <- 'leaf'
        if(!is.null(leaf_size)){
          d <- leaf_size
        }
      }
      else if(
        purrr::detect_index(sring[(j+1):length(sring)], fractalforest::is_pop_symbol) == 0 | #if there is no bracket forward, is a leaf
        sring[(j+1):length(sring)] %>% purrr::detect_index(fractalforest::is_branch_symbol) == 0 | # if there is no segment forward, is a leaf
        (sring[(j+1):length(sring)] %>% purrr::detect_index(is_pop_symbol) < sring[(j+1):length(sring)] %>% purrr::detect_index(fractalforest::is_branch_symbol)) # if the next bracket is comes before the next segment, is a leaf
      ){
        tp <- 'leaf'
        if(!is.null(leaf_size)){
          d <- leaf_size
        }
      }
      else{
        tp <- 'branch'
      }

      output[[n]] <- list(from_x = cp[1], to_x = x, from_y = cp[2], to_y = y, diameter = d, type = tp)
      cp <- c(x, y)
    }

    else if (sring[[j]] == "[") {

      ns <- ns + 1
      fifo[[ns]] <- c(cp, ch)

    }

    else if (sring[[j]] == "]") {
      cp <- fifo[[ns]][1:2]
      ch <- fifo[[ns]][3]

      ns <- ns - 1

    }

    else if (sring[[j]] == "(") {

      pr[1] <- pr[1] * h_reduction
      pr[2] <- pr[2] * d_reduction

    }

    else if (sring[[j]] == ")") {

      pr[1] <- pr[1] / h_reduction
      pr[2] <- pr[2] / d_reduction

    }

    else if (sring[[j]] == "-"){
      if(randomness){ang_j <- stats::rnorm(1, angle, angle_sd)}
      else{ang_j <- angle}
      ch = ch - ang_j}

    else if (sring[[j]] == "+") {
      if(randomness){ang_j <- rnorm(1, angle, angle_sd)}
      else{ang_j <- angle}
      ch = ch + angle
    }

  }

  df <- output %>% dplyr::bind_rows()

  hfac <- (max(df$to_y)-min(df$from_y))/height
  df[,1:4] <- df[,1:4]/hfac

  if(is.null(diameter)){
    df <- df[,-5]
  }

  return(df)

}
