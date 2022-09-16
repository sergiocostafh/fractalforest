#' Build a 2D Forest Profile of L-System Trees Based on Inventory Data
#'
#' @param data A data frame that represents field or simulated forest inventory data.
#' @param height Name of the column containing the heights of the trees.
#' @param diameter Name of the column containing the dbh or base diameters of the trees.
#' @param label A column with the labels that identifies the trees (scientific names or other taxonomic identification).
#' @param string The string or the column of the strings that contains the turtle graphics instructions, created by `iterate_lsystem` function. If NULL (default), the `tree_model` argument will be used.
#' @param tree_model The tree model to be used when `string` = NULL. Can be an integer or the name (character) that refers to the desired tree model. Check the `fractal_tree_model` function help for more details. Default is "binary_tree".
#' @param n_iter Integer. The number of iterations to build the selected `tree_model` when `string` = NULL (default). Default value = NULL means it depends on the selected `tree_model`. Check the `fractal_tree_model` function help for more details.
#' @param angle Numeric. The angle in degrees when a change in direction is requested.
#' @param crown_diameter Crown diameter of the plant.
#' @param h_reduction Numeric. Reduction rate to be applied to the length of the branches whenever the symbols '(' and ')' appear in the string. Default is the golden ratio.
#' @param d_reduction Numeric. Reduction rate to be applied to the diameter of the branches whenever the symbols '(' and ')' appear in the string. Default is the golden ratio.
#' @param randomness Logical. Should randomness be applied to the angle of the branches?
#' @param angle_cv Numerical. The coefficient of variation of the angle, used when `randomness` = TRUE. Default is 0.1.
#' @param length_cv Numerical. The coefficient of variation of the length, used when `randomness` = TRUE. Default is 0.1.
#' @param leaf_size Size of leaves (branches from the ends of the plant). Default is NULL (follows the applied d_reduction value).
#' @param sample Logical. Whether to sample rows (individuals) from the `data`. If FALSE, the entire `data` will be used. Default to TRUE.
#' @param n_trees Integer. The number of trees to be sampled from `data` when `sample` = TRUE.
#' @param dist Numerical. The average linear distance between the trees.
#' @param dist_cv Numerical. The coefficient of variation of the `dist`argument. Default is .3.
#'
#' @importFrom magrittr %>%
#' @importFrom stats rnorm
#' @importFrom purrr detect_index
#'
#' @details A column of `data` can be declared in the `string` argument. In this case the column must contain for each line (tree) the turtle graphics string that builds the tree.
#'
#' @export
build_forest_profile <- function(data,
                         height, diameter, label,
                         string = NULL, tree_model = "binary_tree", n_iter = NULL,
                         angle = 15, crown_diameter = NULL,
                         h_reduction = (1+sqrt(5))/2-1, d_reduction = (1+sqrt(5))/2-1,
                         randomness = FALSE, angle_cv = .1, length_cv = .1, leaf_size = NULL,
                         sample = FALSE, n_trees,
                         dist = 3, dist_cv = .3){

  if(sample){
    smp <- sample(1:nrow(data), n_trees, replace = T)

    full_df <- data[smp, ]
  }
  else{
    full_df <- data

    n_trees <- nrow(full_df)
  }

  # required columns
  diameter <- rlang::ensym(diameter)
  height <- rlang::ensym(height)
  label <- rlang::ensym(label)

  # null, char or column
  string <- rlang::enquo(string)

  if(rlang::quo_is_null(string)){

    if(is.null(n_iter)){
      strings <- replicate(fractalforest::fractal_tree_model(tree_model), n = n_trees)
    }
    else{
      strings <- replicate(fractalforest::fractal_tree_model(tree_model, n_iter = n_iter), n = n_trees)
    }

    full_df <- full_df %>%
      dplyr::mutate(..string.. = strings)

  }
  else if(rlang::quo_is_symbol(string)){
    string <- rlang::ensym(string)
    full_df <- full_df %>%
      dplyr::mutate(..string.. = !!string)
  }
  else{
    string <- rlang::as_name(string)
    full_df <- full_df %>%
      dplyr::mutate(..string.. = string)
  }

  # null, integer or column
  leaf_size <- rlang::enquo(leaf_size)
  if(rlang::quo_is_null(leaf_size)){
    full_df <- full_df %>%
      dplyr::mutate(..leaf_size.. = NA)
  }
  else if(rlang::quo_is_symbol(leaf_size)){
    leaf_size <- rlang::ensym(leaf_size)
    full_df <- full_df %>%
      dplyr::mutate(..leaf_size.. = !!leaf_size)
  }
  else{
    leaf_size <- rlang::quo_squash(leaf_size)
    if(!is.numeric(leaf_size)){stop('`leaf_size` must be numeric.')}
    full_df <- full_df %>%
      dplyr::mutate(..leaf_size.. = leaf_size)
  }

  # null or column
  crown_diameter <- rlang::enquo(crown_diameter)
  if(rlang::quo_is_null(crown_diameter)){
    full_df <- full_df %>%
      dplyr::mutate(..crown_diameter.. = NA)
  }
  else{
    crown_diameter <- rlang::ensym(crown_diameter)
    full_df <- full_df %>%
      dplyr::mutate(..crown_diameter.. = !!crown_diameter)
  }

  # logical or column
  randomness <- rlang::enquo(randomness)
  if(rlang::quo_is_symbol(randomness)){
    randomness <- rlang::ensym(randomness)
    full_df <- full_df %>%
      dplyr::mutate(..randomness.. = !!randomness)

    if(!is.logical(full_df$..randomness..)){stop('`randomness` must be logical.')}

  }
  else{

    randomness <- rlang::quo_squash(randomness)

    if(!is.logical(randomness)){stop('`randomness` must be logical.')}

    full_df <- full_df %>%
      dplyr::mutate(..randomness.. = randomness)
  }

  # numeric or column
  angle <- rlang::enquo(angle)
  if(rlang::quo_is_symbol(angle)){
    angle <- rlang::ensym(angle)
    full_df <- full_df %>%
      dplyr::mutate(..angle.. = !!angle)

    if(!is.numeric(full_df$..angle..)){stop('`angle` must be numeric.')}

  }
  else{
    angle <- rlang::quo_squash(angle)

    if(!is.numeric(angle)){stop('`angle` must be numeric.')}

    full_df <- full_df %>%
      dplyr::mutate(..angle.. = angle)
  }

  h_reduction <- h_reduction
  h_reduction <- rlang::enquo(h_reduction)
  if(rlang::quo_is_symbol(h_reduction)){
    h_reduction <- rlang::ensym(h_reduction)
    full_df <- full_df %>%
      dplyr::mutate(..h_reduction.. = !!h_reduction)

    if(!is.numeric(full_df$..h_reduction..)){stop('`h_reduction` must be numeric.')}

  }
  else{
    h_reduction <- rlang::quo_squash(h_reduction)

    if(!is.numeric(h_reduction)){stop('`h_reduction` must be numeric.')}

    full_df <- full_df %>%
      dplyr::mutate(..h_reduction.. = h_reduction)
  }

  d_reduction <- d_reduction
  d_reduction <- rlang::enquo(d_reduction)
  if(rlang::quo_is_symbol(d_reduction)){
    d_reduction <- rlang::ensym(d_reduction)
    full_df <- full_df %>%
      dplyr::mutate(..d_reduction.. = !!d_reduction)

    if(!is.numeric(full_df$..d_reduction..)){stop('`d_reduction` must be numeric.')}

  }
  else{
    d_reduction <- rlang::quo_squash(d_reduction)

    if(!is.numeric(d_reduction)){stop('`d_reduction` must be numeric.')}

    full_df <- full_df %>%
      dplyr::mutate(..d_reduction.. = d_reduction)
  }

  angle_cv <- rlang::enquo(angle_cv)
  if(rlang::quo_is_symbol(angle_cv)){
    angle_cv <- rlang::ensym(angle_cv)
    full_df <- full_df %>%
      dplyr::mutate(..angle_cv.. = !!angle_cv)

    if(!is.numeric(full_df$..angle_cv..)){stop('`angle_cv` must be numeric.')}

  }
  else{
    angle_cv <- rlang::quo_squash(angle_cv)

    if(!is.numeric(angle_cv)){stop('`angle_cv` must be numeric.')}

    full_df <- full_df %>%
      dplyr::mutate(..angle_cv.. = angle_cv)
  }

  length_cv <- rlang::enquo(length_cv)
  if(rlang::quo_is_symbol(length_cv)){
    length_cv <- rlang::ensym(length_cv)
    full_df <- full_df %>%
      dplyr::mutate(..length_cv.. = !!length_cv)

    if(!is.numeric(full_df$..length_cv..)){stop('`length_cv` must be numeric.')}

  }
  else{
    length_cv <- rlang::quo_squash(length_cv)

    if(!is.numeric(length_cv)){stop('`length_cv` must be numeric.')}

    full_df <- full_df %>%
      dplyr::mutate(..length_cv.. = length_cv)
  }

    df <- full_df %>%
      dplyr::select(
        !!diameter,
        !!height,
        !!label,
        ..string..,
        ..angle..,
        ..crown_diameter..,
        ..h_reduction..,
        ..d_reduction..,
        ..randomness..,
        ..angle_cv..,
        ..length_cv..,
        ..leaf_size..
      )

  ac_dist <- 0
  nx <- 0
  forest_df <- df %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    lapply(function(x){
      nx <<- nx + 1

      xd <- x %>% dplyr::pull(!!diameter)
      xh <- x %>% dplyr::pull(!!height)
      xl <- x %>% dplyr::pull(!!label)

      lsp <- x %>% dplyr::pull(..string..)

      xang <- x %>% dplyr::pull(..angle..)
      xcrdiam <- x %>% dplyr::pull(..crown_diameter..)
      xhred <- x %>% dplyr::pull(..h_reduction..)
      xdred <- x %>% dplyr::pull(..d_reduction..)
      xrand <- x %>% dplyr::pull(..randomness..)
      xangcv <- x %>% dplyr::pull(..angle_cv..)
      xlencv <- x %>% dplyr::pull(..length_cv..)
      xlfsz <-  x %>% dplyr::pull(..leaf_size..)

      if(is.na(xlfsz)){xlfsz <- NULL}
      if(is.na(xcrdiam)){xcrdiam <- NULL}

      tree_df <- fractalforest::build_tree(string = lsp, height = xh, diameter = xd, angle = xang,
                            randomness = xrand, crown_diameter = xcrdiam, h_reduction = xhred,
                            d_reduction = xdred, angle_cv = xangcv, length_cv = xlencv,leaf_size = xlfsz)

      tree_df <- tree_df %>%
        dplyr::mutate(!!label := xl,
               from_x = from_x + ac_dist,
               to_x = to_x + ac_dist,
               tree_id = nx)

      ac_dist <<- ac_dist + rnorm(1, dist, dist*dist_cv)

      cat("\rBuilding tree", nx, "of", n_trees)
      flush.console()
      return(tree_df)

    }) %>%
    dplyr::bind_rows()

  return(forest_df)

}


