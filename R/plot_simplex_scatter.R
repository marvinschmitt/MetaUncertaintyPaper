#' Plot scatter within a simplex from barycentric coordinates
#'
#' @param P matrix of barycentric coordinates
#' @param ... further arguments passed to `points()`
#'
#' @export
plot_simplex_scatter <- function(P, ...){
  B = matrix(
    c(0, 0,
      1, 0,
      0.5, sqrt(3)/2),
    byrow = TRUE,
    ncol = 2
  )

  graphics::plot.new()
  graphics::par(pty="s", xpd=NA, mar=c(0,0,0,0))
  graphics::plot.window(xlim=c(-0.15, 1.15), ylim=c(-0.15, 1.15))


  graphics::points(P %*% B, pch = 16, ...)


  graphics::text(diag(3) %*% B,                          # label edges
       c(expression(M[1]), expression(M[2]), expression(M[3])),
       pos=c(2, 4, 3),
       cex=3)
  graphics::polygon(B[, 1], B[, 2], lwd=3)

}
