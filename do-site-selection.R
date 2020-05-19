library(tidyverse)

network_prefix <- "//INHS-Bison"

#Read in data for EPA sites, and the known IDNR basin surveys
epa <- read_csv("~/CREP/Data/IDNR_basin_surveys/il_epastation_2020_bmetzke_HUC8_join.csv")
# basin_stein <- read_csv(file = "~/CREP/Analysis/Project-Shelley/Data/unique_list_basin.csv")
# basin_drake <- read_csv(file = "~/CREP/Analysis/Project-Shelley/Data/unique_list_drake.csv")
# 
# basin_stein <- drop_na(basin_stein)
# basin <- full_join(basin_drake, basin_stein)
# 
# habitat <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_landuse_geology_metrics_revised.csv"))
# names(habitat) <- str_to_lower(names(habitat))
# 
# basin_hab <- habitat %>% 
#   right_join(basin, by = c("pu_gap_code"="pugap_code")) %>% 
#   select(70, 1, 69, 2:5, 38:39, 44, 47, 62:68)


## Read in local watershed landscape information

fish <- read_csv(file = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/kasky_fish_and_landuse_geology_metrics.csv")) 

basin_drake_fish <- fish %>% 
  filter(data_source == "IDNR_basin_surveys_LDrake") %>% 
  select(1:4, 76:142)
  
  
## Filter EPA Sites to include only kaskty sites
site_list <- epa %>% 
  select(-c(OBJECTID, Join_Count, TARGET_FID, HUC_8)) %>% 
  filter(str_detect(PUGAP_CODE, 'kasky') & TYPE == "STREAM") %>% 
  right_join(basin_drake_fish, by = c("PUGAP_CODE" = "pu_gap_code")) %>% 
  filter(c_order < 4)%>% 
  drop_na(LATD)


site_list$c_order <- as.factor(site_list$c_order)


####
# site_list2 <- epa %>% 
#   select(-c(1:3,14)) %>% 
#   filter(str_detect(PUGAP_CODE, 'kasky') & TYPE == "STREAM") %>% 
#   right_join(basin_hab, by = c("PUGAP_CODE" = "pu_gap_code")) %>% 
#   filter(c_order < 4)%>% 
#   drop_na(LATD)



## Now that you have a list you need to reduce it 
## Consider looking at a histogram of teh sites with High Ag and High Forest separately then determine a cut off once your know the variation in values

#### Plots ####
theme_update(plot.title = element_text(hjust = 0.5))

# Forest
site_list %>% 
  ggplot2::ggplot(aes(x= w_forest_total, color = c_order)) +
  geom_histogram()+
  labs(x = "Watershed % Forest", y = "Count", title = "Watershed %Forest @ IDNR Sites", color = "Order")

##Seems like above 10-15% Forest is where it is at

# Aggriculture
site_list %>% 
  ggplot2::ggplot(aes(x= w_agriculture, color = c_order)) +
  geom_histogram()+
  labs(x = "Watershed % Agg", y = "Count", title = "Watershed %Agg @ IDNR Sites", color = "Order")

# ggsave("richness_paired_sites_boxplot.pdf", width = 8, height = 8, path = paste0(network_prefix, analysis_folder), units = "in")


site_list_forest_grouped <- site_list %>%
  select(c(1:4, 9:18, 51:52, 57, 60)) %>% 
  group_by(c_order, HUC8_Name) %>% 
  arrange(desc(w_forest_total), .by_group = TRUE)

site_list_ag_grouped <- site_list %>% 
  select(c(1:4, 9:18, 51:52, 57, 60)) %>% 
  group_by(c_order, HUC8_Name) %>% 
  arrange(desc(w_agriculture), .by_group = TRUE)

write_csv(site_list_forest_grouped, path = "~/GitHub/do-kaskaskia/data/forest_list.csv")
write_csv(site_list_ag_grouped, path = "~/GitHub/do-kaskaskia/data/ag_list.csv")


lstf <- read_csv("~/GitHub/do-kaskaskia/data/paired_forest_list.csv")
lsta <- read_csv("~/GitHub/do-kaskaskia/data/paired_ag_list.csv")


final_list <- bind_rows(lsta, lstf) %>% 
  drop_na() %>% 
  select(-c(site_id, reach_name, event_date)) %>% 
  rename(landuse_category = Type) %>% 
  arrange(Pair)

names(final_list) <- str_to_lower(names(final_list))

write_csv(final_list, path = "~/GitHub/do-kaskaskia/data/kaskaskia_do_site_list.csv")

##Summary?????
# site_summary <- summarize_all(site_list,
#                               c_order,
#                               mean_agg = mean(w_agriculture),
#                               max_agg = max(w_agriculture),
#                               min_agg = min(w_agriculture),
#                               mean_for = mean(w_forest_total),
#                               max_for = max(w_forest_total),
#                               min_for = min(w_forest_total)
#                               )


