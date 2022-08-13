#---- Scenario Set Up ----
#includes all the params we'd like to vary 

set.seed(172021)

prey_plastic_conc <- tribble(
  ~prey,   ~plastic_level, ~plastic_conc,
  "krill", "lo",           0.001,#
  "krill", "med",          0.06, #Desforges 
  "krill", "hi",           0.5,  #Sun 
  "fish",  "lo",           0.02, #Hipfner, which is herring 
  "fish",  "med",          0.3, #Rochman 2015
  "fish",  "hi",           0.77 #Tanaka 2016
) %>% 
  mutate(plastic_level = factor(plastic_level, levels = c("lo", "med", "hi")))

prey_size <- tribble(
  ~prey,   ~prey_size_kg,
  "krill", 0.00008,
  "fish",  0.025
)

retention <- tribble(
  ~retention, ~retention_level,
  0.25,        "lo",
  0.5,         "med",
  0.75,        "hi"  
)


scenarios <- expand_grid(
  species_code = c("bw", "bp", "mn"),
  retention = c(0.25, 0.5, 0.75), 
  prey = c("krill", "fish"),
  plastic_level = factor(c("lo", "med", "hi"), levels = c("lo", "med", "hi"))
) %>% 
  left_join(prey_size, by = "prey") %>% 
  left_join(prey_plastic_conc, by = c("prey", "plastic_level")) %>% 
  filter(prey == "krill" | species_code == "mn")

scenario_lo <- scenarios %>% 
  filter(plastic_level == "lo", 
         retention == 0.25)

scenario_med <- scenarios %>% 
  filter(plastic_level == "med", 
         retention == 0.5)

scenario_hi <- scenarios %>% 
  filter(plastic_level == "hi", 
         retention == 0.75)


#Head to scenarios_divided.R 





                
                  