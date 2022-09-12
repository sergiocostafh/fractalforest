#' Generate a point cloud from 2D trees data frame
#'
#' @param tree_df The resulting data.frame from the `build_tree` or `build_forest_profile` functions.
#' @param pt_distance Numeric. Distance between points in the point cloud.
#'
#' @export
generate_point_cloud <- function(tree_df, pt_distance = .3){
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
    dplyr::mutate(geometry = sf::st_buffer(lines_sf, dist = tree_df$diameter/100, endCapStyle = 'FLAT') %>% dplyr::pull(geometry),
                  h_lin = sqrt((from_x-to_x)^2+(from_y-to_y)^2)) %>%
    sf::st_as_sf() %>%
    dplyr::group_by(tree_id) %>%
    dplyr::summarise(h_lin = sum(h_lin),
                     d = max(diameter),
                     h = max(to_y)) %>%
    dplyr::mutate(n_pts = round(h_lin*15))


  grid <- sf::st_make_grid(plot_df, cellsize = pt_distance, square = T)
  pts <- grid %>% sf::st_cast('POINT')
  pts_itsct <- sf::st_intersects(pts, plot_df, sparse = F)
  point_cloud <- pts[rowSums(pts_itsct)>=1]
  pt_cloud_xy <- sf::st_coordinates(point_cloud)
  return(pt_cloud_xy)

}
