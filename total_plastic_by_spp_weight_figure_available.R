# total pieces by species weight figure #

#Needed to make plots -----

whale_latin_names <- tribble(
  ~species,          ~prey_type,   ~order, ~species_code,
  "B. musculus",     "krill", 4,      "bw",
  "B. physalus",     "krill", 3,      "bp",
  "M. novaeangliae", "fish",  1,      "mn",
  "M. novaeangliae", "krill", 2,      "mn"
) %>% 
  # Create a new column with the proper species/prey format
  mutate(tick_labels = fct_reorder(
    str_glue("italic(\"{species}\") ~ \"({prey_type})\""),
    order
  ))

pal <- c("mn" = "gray30", 
         "bp" = "chocolate3",
         "bw" = "dodgerblue2")


#Raincloud Builder -----
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 13),
  axis.title.y = element_text(size = 13),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


"%||%" <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }

GeomFlatViolin <-
  ggproto(
    "GeomFlatViolin",
    Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )



##### Total pieces by species weight
total_pieces_by_species_weight_figure <- function(results){
  figure <- results %>%
    left_join(whale_latin_names, by = c("species_code", "prey_type")) %>% ##here
    mutate(total_plastic_spp_weight = total_plastic/mass_skr,
           retained_plastic_spp_weight = retained_plastic/mass_skr,
           plastic_prey_spp_weight = plastic_prey/mass_skr) %>% 
    filter(total_plastic_spp_weight > 0, 
           retained_plastic_spp_weight > 0, 
           plastic_prey_spp_weight > 0) %>% 
    ggplot(aes(x = tick_labels)) + ##here
    geom_flat_violin(aes(y = total_plastic_spp_weight, 
                         color = species_code, fill = species_code), 
                     position = position_nudge(x = 0.35, y = 0), alpha = 0.4) +
    geom_boxplot(aes(y = retained_plastic_spp_weight), color = "blue", 
                 width = .2, position = position_nudge(x = 0.15, y = 0), 
                 outlier.shape = NA, alpha = 0.5) +
    geom_boxplot(aes(y = plastic_prey_spp_weight), color = "red", 
                 width = .2, outlier.shape = NA, alpha = 0.5) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    #guides(fill = FALSE, color = FALSE) + #here, combined it and removed
    annotation_logticks(sides = "l") +
    #labs(y = " ", x = " ") +
    scale_x_discrete(labels = function(lbl) parse(text = lbl)) + #function rather than object
    scale_y_log10(labels = function(x) ifelse(x == 0, "0", x), 
                  expression("Mass specific total plastic particles (day" ^ -1 * ")" ),
                  limits = c(0.01, 500), #max space of graph
                  breaks = c(0.1, 1, 10, 100, 1000)) + #where the ticks are
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.title.x = element_blank())  
}

pieces_by_species_weight_med <- total_pieces_by_species_weight_figure(all_scenarios %>% 
                                                                        filter(scenario_name == "med"))
pieces_by_species_weight_med 
#change limit to 500 when med

pieces_by_species_weight_hi <- total_pieces_by_species_weight_figure(all_scenarios %>% 
                                                                       filter(scenario_name == "hi"))
pieces_by_species_weight_hi
#change limit to 2000 when hi

pieces_by_species_weight_lo <- total_pieces_by_species_weight_figure(all_scenarios %>% 
                                                                       filter(scenario_name == "lo"))
pieces_by_species_weight_lo #change limit to 20
