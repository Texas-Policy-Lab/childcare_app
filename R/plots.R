theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#FFFFFF", color = NA), 
      panel.background = element_blank(), 
      legend.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.border = element_blank(),
      ...
    )
}

map_cbsa <- function(df,
                     caption = "") {
  gg <- ggplot(df, aes(x = long, y = lat, group = subregion, fill = fill)) +
    geom_polygon(fill = "grey",
                 color = "white") +
    ggiraph::geom_polygon_interactive(data = df,
                                      aes(tooltip =  glue::glue("County: {county}<br>Seats: {value}"))
                                      ) +
    coord_map() +
    theme_map() + 
    labs(x = NULL, 
         y = NULL, 
         title = NULL, 
         subtitle = "", 
         caption = caption) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184")
    ) +
    scale_fill_manual("Seats per 100", values=c("#e54e4d", "#cea2b2", "#00429d"))
  
  gg <- ggiraph::girafe(ggobj = gg)
}
