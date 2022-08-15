#' Extract the fractal tree matrix from a fractal tree data frame
#'
#' @param tree_df The resulting data.frame from the `build_tree` function.
#'
#' @export
#'
get_tree_matrix <- function(tree_df){
  tree_df %>%
    dplyr::select(x = from_x, y = from_y) %>%
    dplyr::bind_rows(
      tree_df %>%
        dplyr::select(x = to_x, y = to_y)
    ) %>%
    dplyr::distinct() %>%
    as.matrix() %>%
    unname()
}
