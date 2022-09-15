#' Visualize the 2D profile of a fractal trees forest
#'
#' @param tree_df The resulting data.frame from the `build_forest_profile` function.
#' @param d_col Name of the column that contains the diameters along the branches. Usually 'diameter'.
#' @param label A column with the labels that identifies the trees (scientific names or other taxonomic identification).
#' @param branch_color Character. Color of the tree branches.
#' @param leaf_color Character. Color of the tree leaves.
#' @param simplify Logical. Whether to simplify geometries of the tree. Default to FALSE.
#' @param dTolerance Numeric. If simplify = TRUE, the tolerance parameter to be considered. Default to .15.
#'
#' @export
plot_forest_profile <- function(tree_df, d_col = NULL, label = NULL, branch_color = 'black', leaf_color = 'black', simplify = F, dTolerance = .15){

  d_col <- rlang::enquo(d_col)
  label <- rlang::enquo(label)
  branch_color <- rlang::enquo(branch_color)
  leaf_color <- rlang::enquo(leaf_color)

  if(rlang::quo_is_null(d_col)){
    tree_df <- tree_df %>%
      dplyr::mutate(diameter = .8)
  }
  else{
    d_col <- rlang::ensym(d_col)
    tree_df <- tree_df %>%
      dplyr::rename(diameter = {{d_col}})}

  if(rlang::quo_is_symbol(branch_color)){

    branch_color <- rlang::ensym(branch_color)

    tree_df <- tree_df %>%
      dplyr::mutate(bc = !!branch_color)
  }
  else{

    branch_color <- branch_color %>% rlang::as_name()

    tree_df <- tree_df %>%
      dplyr::mutate(bc = branch_color)
  }

  if(rlang::quo_is_symbol(leaf_color)){

    leaf_color <- rlang::ensym(leaf_color)

    tree_df <- tree_df %>%
      dplyr::mutate(lc = !!leaf_color)
  }
  else{

    leaf_color <- leaf_color %>% rlang::as_name()

    tree_df <- tree_df %>%
      dplyr::mutate(lc = leaf_color)

  }

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
    dplyr::mutate(geometry = sf::st_buffer(lines_sf, dist = tree_df$diameter/100, endCapStyle = 'FLAT') %>% dplyr::pull(geometry)) %>%
    sf::st_as_sf() %>%
    dplyr::group_by(type, lc, bc) %>%
    dplyr::summarise()

  if(simplify){
    tol <- dplyr::if_else(plot_df$type == 'branch', dTolerance, 0)
    plot_df <- plot_df %>%
      sf::st_simplify(dTolerance = tol)
  }

  ggplot2::ggplot()+
    ggplot2::geom_sf(ggplot2::aes(fill = bc, color = bc),
                     show.legend = F, data = plot_df %>% dplyr::filter(type == 'branch'))+
    ggplot2::geom_sf(ggplot2::aes(fill = lc, color = lc),
                     show.legend = F, data = plot_df %>% dplyr::filter(type == 'leaf'))+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_color_identity()+
    ggplot2::labs(y = 'Height (m)')+
    ggplot2::scale_y_continuous(limits=function(x){c(0,max(x)*1.1)}, expand = c(0,0), breaks = seq(0,100,5))+
    ggplot2::scale_x_continuous(breaks = -1000)+
    cowplot::theme_minimal_hgrid()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          panel.grid = ggplot2::element_line(linetype = 'dashed'),
          axis.text.x = ggplot2::element_blank())

}
