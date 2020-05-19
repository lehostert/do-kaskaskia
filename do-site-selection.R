library(tidyverse)

network_prefix <- "//INHS-Bison"

#Read in data for EPA sites, and the known IDNR basin surveys
 
# basin_stein <- read_csv(file = "~/CREP/Analysis/Project-Shelley/Data/unique_list_basin.csv")
# basin_drake <- read_csv(file = "~/CREP/Analysis/Project-Shelley/Data/unique_list_drake.csv")

## Read in local watershed landscape information

fish <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv")) 

basin_drake_fish <- fish %>% 
  filter(data_source == "IDNR_basin_surveys_LDrake") %>% 
  select(1:4, 76:142)
  
  
## Filter EPA Sites to include only kaskty sites
site_list <- epa %>% 
  select(-c(1:3,14)) %>% 
  filter(str_detect(PUGAP_CODE, 'kasky') & TYPE == "STREAM") %>% 
  right_join(basin_drake_fish, by = c("PUGAP_CODE" = "pu_gap_code")) %>% 
  filter(c_order < 4)%>% 
  drop_na(LATD)


site_list$c_order <- as.factor(site_list$c_order)

# test<- basin_drake_fish %>% 
#   left_join(epa, by = c("pu_gap_code" ="PUGAP_CODE")) %>% 
#   select(72:80,2,1,3:71) %>% 
#   rename("PUGAP_CODE" = "pu_gap_code")

## Now that you have a list you need to reduce it 
  
## Consider looking at a histogram of teh sites with High Ag and High Forest separately then determine a cut off once your know the variation in values

#### Plots ####
theme_update(plot.title = element_text(hjust = 0.5))

# Forest
site_list %>% 
  ggplot2::ggplot(aes(x= w_forest_total, color = c_order)) +
  geom_histogram()+
  labs(x = "Watershed % Forest", y = "Count", title = "Watersehd %Forest @ IDNR Sites", color = "Order")

##Seems like above 10-15% Forest is where it is at

# Aggriculture
site_list %>% 
  ggplot2::ggplot(aes(x= w_agriculture, color = c_order)) +
  geom_histogram()+
  labs(x = "Watershed % Agg", y = "Count", title = "Watersehd %Agg @ IDNR Sites", color = "Order")

# ggsave("richness_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")


site_list_forest <- site_list %>%
  select(c(1:4, 9:18, 51:52, 57, 60)) %>% 
  arrange(desc(w_forest_total))

site_list_ag <- site_list %>% 
  select(c(1:4, 9:18, 51:52, 57, 60)) %>% 
  arrange(desc(w_agriculture))


site_<- rf_result %>% 
  select(-c(x)) %>% 
  group_by(metric) %>% 
  arrange(mtry, .by_group = TRUE) %>% 
  mutate(
    diff = round(lead(rsq)-rsq, 4)
  ) %>% 
  filter(diff <= 0.01) %>% 
  summarize(mtry = min(mtry)) %>% 
  ungroup()



##Summary?????
site_summary <- summarize_all(site_list,
                              c_order,
                              mean_agg = mean(w_agriculture),
                              max_agg = max(w_agriculture),
                              min_agg = min(w_agriculture),
                              mean_for = mean(w_forest_total),
                              max_for = max(w_forest_total),
                              min_for = min(w_forest_total)
                              )


