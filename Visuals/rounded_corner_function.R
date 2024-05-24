#rounding function
# geom_rtile for rounded Heatmap edges ###  DO NOT EDIT  ### -----
# https://stackoverflow.com/questions/64355877/round-corners-in-ggplots-geom-tile-possible

`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

GeomRtile <- ggproto("GeomRtile", 
                     statebins:::GeomRrect, # 1) only change compared to ggplot2:::GeomTile
                     
                     extra_params = c("na.rm"),
                     setup_data = function(data, params) {
                       data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                       data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
                       
                       transform(data,
                                 xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                 ymin = y - height / 2, ymax = y + height / 2, height = NULL
                       )
                     },
                     default_aes = aes(
                       fill = "grey20", colour = NA, size = 0.1, linetype = 1,
                       alpha = NA, width = NA, height = NA
                     ),
                     required_aes = c("x", "y"),
                     
                     # These aes columns are created by setup_data(). They need to be listed here so
                     # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                     # limits, not just those for which x and y are outside the limits
                     non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                     draw_key = draw_key_polygon
)

geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"), # 2) add radius argument
                       ...,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile, # 3) use ggproto object here
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}
############# END OF DO NOT EDIT SECTION #######################