
whale_latin_names <- tribble(
  ~species,          ~prey,   ~order, ~species_code,
  "B. musculus",     "krill", 1,      "bw",
  "B. physalus",     "krill", 2,      "bp",
  "M. novaeangliae", "krill", 3,      "mn"
) %>% 
  # Create a new column with the proper species/prey format
  mutate(tick_labels = fct_reorder(
    str_glue("italic(\"{species}\") ~ \"({prey})\""),
    order
  ))

pal <- c("mn" = "gray30", 
         "bp" = "chocolate3",
         "bw" = "dodgerblue2")


## Feeding Rate v Engulfment Capacity 
just_krill <- filter(all_scenarios, prey_type == "krill", )
just_krill_foo <- just_krill %>% 
  mutate(engulf_m3_skr = engulf_m3_skr*4) %>% 
  pivot_longer(cols = c(engulf_m3_skr, daily_lunges)) %>% 
  mutate(name = factor(name, levels = c("engulf_m3_skr", "daily_lunges"))) %>% 
  left_join(whale_latin_names, by = "species_code") %>% 
  ggplot(aes(x= fct_reorder(species_code, value, .desc=FALSE),y=value,fill=name)) +
  geom_boxplot(aes(y = value, fill = name), color = "black") +
  scale_y_continuous(name = bquote('Feeding rate'~(lunges~day^-1)),
                     sec.axis = sec_axis(~ . /5, 
                                         name = bquote('Engulfment capacity'~(m^3)))) +
  #scale_x_discrete(name = "Species Code")  +
  scale_color_manual(values = c(engulf_m3_skr = "dodgerblue2", daily_lunges = "cadetblue"))+ 
  #                  name = "Parameter",
                     #breaks=c("ctrl", "trt1", "trt2"),
  #                   labels=c("Engulfment Capacity", "Daily Lunges")) +
  scale_fill_manual(values = c(engulf_m3_skr = "dodgerblue2", daily_lunges = "cadetblue"))+ 
  #                  name = "Parameter",
                    #breaks=c("ctrl", "trt1", "trt2"),
  #                  labels=c("Engulfment Capacity", "Daily Lunges")) +
  labs(y = "",
       x = "")+
  scale_x_discrete(labels = parse(text = rev(levels(whale_latin_names$tick_labels)))) +
  theme_minimal() +
  theme(axis.title.y.right = element_text(colour="dodgerblue2"),
        axis.title.y.left = element_text(colour="cadetblue") )
#scale_fill_manual(values = c(engulf_m3_skr = "dodgerblue2", daily_lunges = "cadetblue")) +
#scale_color_manual(values = c(engulf_m3_skr = "dodgerblue2", daily_lunges = "cadetblue")) +
#scale_fill_discrete(name = "Parameters", labels = c("Engulfment Capacity", "Daily Lunges"))+
#scale_color_discrete(name = "Parameters", labels = c("Engulfment Capacity", "Daily Lunges"))+
#theme_bw()
just_krill_foo + theme(legend.position = "none")



