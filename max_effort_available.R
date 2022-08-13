# Max Effort
#This script find the deployments that are longer than 1 day and sees how many hours are 
#left past that time. Then, it looks at the max effort in those deployments 
#Over multiple days, there is less bias to the maximum possible effort over time then
#when just using sub daily deployments. 

#left join max effort by species, filter out anything that exceeds max effort 

#---- Process Data ----
deploy_cycle <- lunges %>% 
  mutate(hours_on = as.numeric(lunge_time - tag_on, unit = "hours"), #hours since tag on
         cycle = hours_on %/% 24 , # which 24 hours period following does the lunge fall in
         max_cycle = as.numeric(tag_off - tag_on, unit = "hours" ) %/% 24 - 1, # maximum elig cycle
         is_cycle = cycle <= max_cycle) %>% # is it valid 
  filter(is_cycle) %>% 
  group_by(deployID, species_code, cycle, prey_type) %>% 
  summarise(daily_lunges = n(), .groups = "drop") 
  
 max_effort_data <- deploy_cycle  %>% 
   filter(daily_lunges >= 10) %>% 
   group_by(species_code, prey_type) %>% 
   summarise(lunge_05 = quantile(daily_lunges, 0.05),
             lunge_25 = quantile(daily_lunges, 0.25),
             lunge_50 = quantile(daily_lunges, 0.50), 
             lunge_75 = quantile(daily_lunges, 0.75),
             lunge_95 = quantile(daily_lunges, 0.95),
             .groups = "drop") 
 
 
max_effort <- tribble(
  ~species_code, ~prey_type, ~lunge_95,
  "mn",          "krill",    600,
  "bp",          "krill",    454.00,
  "bw",          "krill",    425.45,
  "mn",          "fish",     153
)


#---- Plot Data ----  
ggplot(deploy_cycle, aes(daily_lunges, color = species_code)) + 
  stat_ecdf() + 
  geom_vline(aes(xintercept = lunge_95, color = species_code),
             data = max_effort) +
  theme_minimal()


ggplot(deploy_cycle, aes(daily_lunges, color = species_code)) + 
  stat_ecdf() + 
  geom_vline(aes(xintercept = lunge_05, color = species_code),
             data = max_effort) +
  xlim(0, 50) +
  theme_minimal()

#---- Export Data ----
 saveRDS(deploy_cycle, "data/output/deploy_cycle.RDS")
 saveRDS(max_effort, "data/output/max_effort.RDS")
 

 