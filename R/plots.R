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

map_cbsa <- function(ccs_map_data = ccs_map_data,
                     covid_map_data = covid_map_data,
                     show_covid,
                     caption = "COVID-19 Source: DSHS data reported on {date}\nEstimates have been updated based on phase 3 reopening requirements as of June 3, 2020",
                     tt = "County: {county}<br>Seats per 100: {est_ccs}<br>COVID Cases: {Cases}<br>COVID Deaths: {Deaths}") {

  gg <- ggplot(ccs_map_data, aes(x = long, y = lat)) +
    ggiraph::geom_polygon_interactive(aes(group = subregion, fill = label, 
                                            tooltip =  glue::glue(tt)),
                                        color = "white"
                                        ) +
    coord_map() +
    theme_map() +
    labs(x = NULL,
         y = NULL,
         title = NULL,
         subtitle = "",
         caption = glue::glue(caption, date = cases %>%
                                dplyr::ungroup() %>% 
                                dplyr::distinct(date) %>% 
                                dplyr::pull(date))) +
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
      plot.caption = element_text(size = 10,
                                  hjust = 0,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184",
                                  family = "Arial"
                                  ),
      legend.title=element_text(size = 12, family="Arial"), 
      legend.text=element_text(size = 11, family="Arial")
    ) +
    scale_fill_manual(stringr::str_wrap("Child care seats per 100 children", 16),
                      values = c("#ee9391", "#b199bf", "#5c67b1"),
                      labels = levels(ccs_map_data$label),
                      drop = FALSE,
                      na.value = "grey")

  if(show_covid) {
    gg <- gg +
      geom_point(data = covid_map_data,
                 aes(x=long, y=lat, size = `Total # (COVID metrics)`),
                 alpha = .75, color = "#e54e4d")
  } else {
    gg <- gg
  }

  gg <- ggiraph::girafe(ggobj = gg, width_svg = 9, height_svg = 6.5)
}
