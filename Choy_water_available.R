#---- Choy and Kashiwabara Data Set Up ----
# data information

Choy <- read_csv("data/raw/Choy_etal_2019_Figure_1b_rawdata.csv") %>% 
  rename(lo_05 = hi_95,
         hi_95 = lo_05)
Choy %>% 
  filter(data_source == "observed values") %>% 
  ggplot(aes(x = med_50, y = DEPTH_m)) +
  geom_ribbon(aes(xmin= lo_05, xmax = hi_95), alpha= 0.25) +
  geom_path(size = 1.5) +
  ylim(-400, 0) +
  theme_classic()



# ---- Choy Process Data ----
Choy_cut <- Choy %>% 
  mutate(depth_bucket = cut(-DEPTH_m, 
                            breaks = c(-Inf, 5, 50, 150, Inf),
                            labels = c("Sub-Surface (0.5-5m)",
                                       "Shallow (5-50m)",
                                       "Moderate (50-150m)",
                                       "Deep (>150m)"))) %>% 
  filter(DEPTH_m > -550) %>% # pick a maximum lunge depth
  group_by(data_source, depth_bucket) %>% 
  summarize_at(vars(lo_05, med_50, hi_95),
               list(mean)) %>% 
  ungroup() %>% 
  filter(data_source == "observed values") %>% 
  select(-data_source)




#---- Kashiwabara Data Set Up ----
Kashiwabara <- read_csv("data/raw/LK_MBNMS_data.csv") %>% 
  select(-total_count, -total_fibers, -total_particles, -blank_adj_count, 
         -blank_adj_count_zero_corr, -blank_adj_count_m3, -prop_frag, -prop_fibers) %>% 
  mutate(depth_bucket = "Surface (0-0.5m)") %>% 
  rename(data_source = location)
  



# ---- Kashiwabara Process Data ----
Kashi_cut <- Kashiwabara %>% 
  group_by(depth_bucket) %>% 
  summarise(lo_05 = quantile(blank_adj_count_m3_zero_corr, 0.05),
            med_50 = quantile(blank_adj_count_m3_zero_corr, 0.5),
            hi_95 = quantile(blank_adj_count_m3_zero_corr, 0.95), 
            .groups = "drop")



depth_m <- tribble(
  ~depth_m, ~depth_bucket,
  0,         "Surface (0-0.5m)",
  0.5,       "Sub-Surface (0.5-5m)",
  5,         "Shallow (5-50m)",
  50,        "Moderate (50-150m)",
  150,        "Deep (>150m)",
)

Choy_Kashi <- rbind(Kashi_cut, Choy_cut) %>% 
  left_join(depth_m, by = "depth_bucket")


# ---- Utility Functions ----

rplastic <- function(depth_bucket, bounds) { 
  rplastic1 <- function(depth_bucket1) { 
    plastic_row <- match(depth_bucket1, Choy_Kashi$depth_bucket)
    x <- c(0.0, 0.05, 0.5, 0.95, 1.0)
    y <- c(bounds[1], 
           Choy_Kashi$lo_05[plastic_row], 
           Choy_Kashi$med_50[plastic_row], 
           Choy_Kashi$hi_95[plastic_row], 
           bounds[2])
    spinterp(x, y, xp = runif(1))
  } 
  stopifnot(all(depth_bucket %in% Choy_Kashi$depth_bucket))
  map_dbl(depth_bucket, rplastic1)
} 


# ----- Save to RDS ----
saveRDS(Choy_Kashi, "data/output/Choy_Kashi.RDS")  
saveRDS(rplastic, "data/output/rplastic.RDS")
