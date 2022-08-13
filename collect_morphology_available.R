#Collect Morphology
# ---- Process Data ----

#these numbers are m3
engulfment_allo_data <- tribble(
  ~species_code, ~slopeMW, ~interceptMW,
  "mn" , 3.24603, -2.15150,
  "bp", 3.54883, -2.85446,
  "bw", 3.667115, -3.024580)


morphology <- read_csv("data/raw/ENP_whale_lengths.csv") %>% 
  left_join(engulfment_allo_data, by = "species_code") %>% 
  mutate(engulf_m3_skr = length_m^slopeMW * 10^interceptMW)
  

# ---- Export Data ----
saveRDS(morphology, "data/output/morphology.RDS")

  
