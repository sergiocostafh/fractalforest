#' Visualize a fractal tree
#'
#' @param tree_df The resulting data.frame from the `build_tree` function.
#' @param d_col Name of the column that contains the diameters along the branches. Usually 'diameter'.
#' @param d_factor Numeric. Multiplication factor to harmonize the visualization of the diameters.
#' @param branch_color Character. Color of the tree branches
#' @param leaf_color Character. Color of the tree leaves.
#'
#' @export
#'
draw_tree <- function(tree_df, d_col = NULL, d_factor = 1, branch_color = 'black', leaf_color = 'black'){

  if(is.null(d_col)){
    tree_df <- tree_df %>%
      dplyr::mutate(diameter = .8)
  }
  else{
    d_col <- ggplot2::sym(d_col)
    tree_df <- tree_df %>%
    dplyr::rename(diameter = {{d_col}})}

  ggplot2::ggplot()+
    ggmap::geom_leg(ggplot2::aes(x = from_x, y = from_y, xend = to_x, yend = to_y, size = diameter * d_factor, color = type),
                 show.legend = F, data = tree_df)+
    ggplot2::scale_color_manual(values = c(branch_color, leaf_color))+
    ggplot2::scale_size_identity()+
    ggplot2::coord_equal()+
    ggplot2::theme_void()

}

