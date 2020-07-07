#' Paired violin plot
#'
#' Create a paired violin plot (useful for visualizing difference between experimental conditions tested on the same subjects or items).
#'
#' Adopted from the geom_violinhalf() source code from the {see} package
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_violin
#' @examples
#' library(ggplot2)
#'
#' @seealso https://github.com/easystats/see/blob/master/R/geom_violinhalf.R
#'
#' @import ggplot2
#' @export
geom_paired_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                            position = "dodge", trim = TRUE, scale = "area",
                            show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPairedViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}


#' GeomPairedViolin
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @importFrom dplyr mutate group_by arrange desc
#' @importFrom rlang .data
#' @keywords internal
GeomPairedViolin <-
  ggproto("GeomViolinHalf", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # Break if the grouping isn't a pair
            if(length(unique(data$group)) != 2){
              stop("geom_paired_violin is only useful for visualizing groupings of length 2.
                   Check out packages {vioplot} and {see} for alternative ways of plotting split violins", call. = FALSE)
            }
            
            object <<- data
            
            object_split <<- split(data, data$group)
            
            data <- do.call(rbind, lapply(split(data, data$group), function(.group) {
              .group$ymin <- min(.group$y)
              .group$ymax <- max(.group$y)
              .group$xmin <- .group$x
              # flips the first group (negative width)
              .group$xmax <- .group$x + .group$width / ifelse(.group$group[1] == 1, -2, 2)
              .group
            }))

          },
          
          draw_group = function(data, panel_scales, coord) {
            data$xminv <- data$x
            data$xmaxv <- data$x + data$violinwidth * (data$xmax - data$x)
            
            newdata <- rbind(
              dplyr::arrange(dplyr::mutate(data, x = .data$xminv), .data$y),
              dplyr::arrange(dplyr::mutate(data, x = .data$xmaxv), dplyr::desc(.data$y))
            )
            
            newdata <- rbind(newdata, newdata[1,])
            
            .grobName("geom_violinhalf", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


#' @keywords internal
"%||%" <- function(a, b) if (!is.null(a)) a else b


#' @keywords internal
.grobName <- function(prefix, grob) {
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' required for this function to work. Please install it.", call. = FALSE)
  }
  grob$name <- grid::grobName(grob, prefix)
  grob
}