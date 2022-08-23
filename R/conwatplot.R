#' @title BarPlots with ggplot2
#'
#' @description Making plots for conservation priority index outputs for waterbodies
#'
#' @param data Dataset for analysis
#' @param y values on y the Y-axis
#' @param x values on the x axis
#'
#' @return plot
#'
#' @examples
#' conwatplot(gbif, waterbody, Area)
#'
#' @export
conwatplot <- function(data, x, y){
  x  <- dplyr::enquo(x)
  y  <- dplyr::enquo(y)
  ggplot2::ggplot(data, ggplot2::aes(!!x, !!y))
}
#' @keywords conservation priority index
NULL
