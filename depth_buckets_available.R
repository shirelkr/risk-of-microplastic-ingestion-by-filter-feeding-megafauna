#Depth Buckets- provides lunges rates per diel period 
# ---- Process Data ----

lunge_counts <- diel_lunges %>% 
  mutate(deployID = factor(deployID)) %>% 
         #date = date(lunge_time)) %>% 
  group_by(deployID, dielperiod, depth_bucket) %>% 
  summarise(n_lunges = n(), 
            .groups = "drop") %>% 
  complete(deployID, dielperiod, depth_bucket, fill = list(n_lunges = 0)) %>% 
  mutate(deployID = as.character(deployID))

diel_deployments_pivot <- diel_deployments %>% 
  rename_at(vars(ends_with("_hours")), ~ str_replace(.x, "_hours", "")) %>%  #removes the letters in quotes so that we are comparing the same words
  pivot_longer(cols = c(day, twilight, night), names_to = "dielperiod", values_to = "period_hours") %>% #new names, values already in the columns
  select(deployID, dielperiod, period_hours, species_code, prey_type) %>% 
  mutate(dielperiod = factor(dielperiod, levels = c("day", "twilight", "night")))

#rates per time of day AND per depth bin 
lunge_rates <- lunge_counts %>% 
  left_join(diel_deployments_pivot, by = c("deployID", "dielperiod")) %>% 
  mutate(lunge_rate = ifelse(period_hours == 0, 0, n_lunges/period_hours)) 
  #left_join(select(deployments, deployID, Species, prey_type), by = "deployID") #if you use select to join, you have to include the key (deployID in this case)

stopifnot(!is.na(lunge_rates$lunge_rate))


# ----Show Data----

ggplot(diel_deployments_pivot, aes(deployID, period_hours, fill = dielperiod)) +
  geom_col(position = "stack") +
  facet_wrap(vars(species_code, prey_type), scales = "free")

mean_lunges <- lunge_rates %>% 
  group_by(species_code, dielperiod, prey_type) %>% 
  summarise(median(lunge_rate),
            mean(n_lunges),
            .groups = "drop")

# ---- Export Data ----
saveRDS(lunge_counts, "data/output/lunge_counts.RDS")
saveRDS(diel_deployments_pivot, "data/output/diel_deployments_pivot.RDS")
saveRDS(lunge_rates, "data/output/lunge_rates.RDS")
