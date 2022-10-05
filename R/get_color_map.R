#' Apply color map to continuous values
#'
#' @param x data
#' @param n_colors number of colors
#' @param color_palette_function callable: color palette function
#'
#' @return color codes for data `x`
#' @export
#'
apply_color_map = function(x, n_colors = 1000, color_palette_function = viridis::viridis){
  color_palette = color_palette_function(n_colors)
  cuts = seq(
    min(x, na.rm=TRUE),
    max(x, na.rm=TRUE),
    length.out = n_colors
  )
  color_indices =
    unlist(
      lapply(x,
             function(z) {
               idxs = which(z <= cuts)
               return(idxs[1])
             }
      )) # calculate color indices (as array)
  hex_colors = color_palette[color_indices]
  hex_colors = stringr::str_sub(hex_colors, 1, nchar(hex_colors)-2)
  return (hex_colors)
}
