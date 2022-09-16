#' Visualize a fractal tree
#'
#' @param tree_df The resulting data.frame from the `build_tree` function.
#' @param d_col Name of the column that contains the diameters along the branches. Usually 'diameter'.
#' @param branch_color Character. Color of the tree branches.
#' @param leaf_color Character. Color of the tree leaves.
#' @param simplify Logical. Whether to simplify geometries of the tree. Default to FALSE.
#' @param dTolerance Numeric. If simplify = TRUE, the tolerance parameter to be considered. Default to .15.
#'
#' @export
plot_tree <- function(tree_df, d_col = NULL, branch_color = 'black', leaf_color = 'black', simplify = F, dTolerance = .15){

  d_col <- rlang::enquo(d_col)

  if(rlang::quo_is_null(d_col)){
    tree_df <- tree_df %>%
      dplyr::mutate(diameter = .8)
  }
  else{
    d_col <- rlang::ensym(d_col)
    tree_df <- tree_df %>%
    dplyr::rename(diameter = {{d_col}})}

  df_geom <- tree_df[1:4] %>%
    dplyr::mutate(ID = dplyr::row_number()) %>%
    dplyr::select(5,1,3,2,4)

  rows <- df_geom %>%
    split(., seq(nrow(.)))

  lines <- lapply(rows, function(row) {
    lmat <- matrix(unlist(row[2:5]), ncol = 2, byrow = TRUE)
    sf::st_linestring(lmat)
  })
  lines <- sf::st_sfc(lines)
  lines_sf <- sf::st_sf('ID' = df_geom$ID, 'geometry' = lines)

  plot_df <- tree_df %>%
    dplyr::mutate(geometry = sf::st_buffer(lines_sf, dist = tree_df$diameter/100, endCapStyle = 'ROUND') %>% dplyr::pull(geometry)) %>%
    sf::st_as_sf() %>%
    dplyr::group_by(type) %>%
    dplyr::summarise()

  if(simplify){
    tol <- dplyr::if_else(plot_df$type == 'branch', dTolerance, 0)
    plot_df <- plot_df %>%
      sf::st_simplify(dTolerance = tol)
  }

  ggplot2::ggplot()+
    ggplot2::geom_sf(ggplot2::aes(fill = type, color = type),
                 show.legend = F, data = plot_df)+
    ggplot2::scale_fill_manual(values = c(branch_color, leaf_color))+
    ggplot2::scale_color_manual(values = c(branch_color, leaf_color))+
    ggplot2::theme_void()

}

