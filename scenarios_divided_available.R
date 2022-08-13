# Simulation Results
set.seed(172021)

species_weight <- tribble(
  ~species_code, ~massSlope, ~massIntercept,
  "bw",           3.3346,     0.3129,
  "bp",           2.595,      1.28388,
  "mn",           2.3373,     1.8535)

all_scenarios <- tibble(
  scenario_name = c("lo", "med", "hi"), 
  scenario = list(scenario_lo, scenario_med, scenario_hi)
  ) %>% 
  mutate(scenario_results = map(scenario, ~ pmap_dfr(.x, run_simulation,
                                                     n_sim = 1000,
                                                     lunge_rates = lunge_rates,
                                                     max_effort = max_effort))) %>% 
  unnest(scenario_results)


all_scenarios <- all_scenarios %>% 
  mutate(latin_name = substr(species_code, start = 1, stop = 2),
         latin_name = case_when(
           latin_name == "bw" ~ "B. musculus",
           latin_name == "bp" ~ "B. physalus",
           latin_name == "mn" ~ "M. novaeangliae")) %>%
  left_join(species_weight, by = "species_code") %>%
  mutate(mass_skr = length_m^massSlope * 10^massIntercept)


#To plot the scenarios, go to .R that plots the figure you want


# saveRDS(all_scenarios, "data/output/all_scenarios.RDS")

