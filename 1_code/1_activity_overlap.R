
rm(list = ls())

# load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(purrr)
library(sf)
library(activity)
library(overlap)

load("C:/Users/kukwh001/OneDrive - Texas A&M University - Kingsville/Desktop/Documents/Research Projects/Florida_mesocarn/2_tmp/daily_cam_data.rda")

# determine activity of apex preds per camera
human_activity_weekly <- camera_parsed %>%
  mutate(human_vehicle = ifelse(human == 1 | vehicle == 1, 1, 0)) %>%
  group_by(camID, week) %>%
  summarise(human_activity = sum(human_vehicle), .groups = "drop") %>% # defined as detections per day
  mutate(human_presence = ifelse(human_activity > 0, 1, 0)) # presence is "there or not" 
human_activity_seasonal <- camera_parsed %>%
  mutate(human_vehicle = ifelse(human == 1 | vehicle == 1, 1, 0)) %>%
  group_by(camID, year_season) %>%
  summarise(human_activity = sum(human_vehicle), .groups = "drop") %>% # defined as detections per day
  mutate(human_presence = ifelse(human_activity > 0, 1, 0)) # presence is "there or not" 

coyote_activity_weekly <- camera_parsed %>%
  mutate(coyote = ifelse(sp == "coyote" | sp_1 == "coyote" | sp_2 == "coyote" | sp_3 == "coyote", 1, 0)) %>%
  group_by(camID,week) %>%
  summarise(coyote_activity = sum(coyote, na.rm = T), .groups = "drop") %>%
  mutate(coyote_presence = ifelse(coyote_activity > 0, 1, 0))
coyote_activity_seasonal <- camera_parsed %>%
  mutate(coyote = ifelse(sp == "coyote" | sp_1 == "coyote" | sp_2 == "coyote" | sp_3 == "coyote", 1, 0)) %>%
  group_by(camID, year_season) %>%
  summarise(coyote_activity = sum(coyote, na.rm = T), .groups = "drop") %>%
  mutate(coyote_presence = ifelse(coyote_activity > 0, 1, 0))

panther_activity_weekly <- camera_parsed %>%
  group_by(camID, week) %>%
  summarise(panther_activity = sum(panther), .groups = "drop") %>%
  mutate(panther_presence = ifelse(panther_activity > 0, 1, 0))
panther_activity_seasonal <- camera_parsed %>%
  group_by(camID, year_season) %>%
  summarise(panther_activity = sum(panther), .groups = "drop") %>%
  mutate(panther_presence = ifelse(panther_activity > 0, 1, 0))

bear_activity_weekly <- camera_parsed %>%
  mutate(bear = ifelse(sp == "bear" | sp_1 == "bear" | sp_2 == "bear" | sp_3 == "bear", 1, 0)) %>%
  group_by(camID, week) %>%
  summarise(bear_activity = sum(bear, na.rm = T), .groups = "drop") %>%
  mutate(bear_presence = ifelse(bear_activity > 0, 1, 0))
bear_activity_seasonal <- camera_parsed %>%
  mutate(bear = ifelse(sp == "bear" | sp_1 == "bear" | sp_2 == "bear" | sp_3 == "bear", 1, 0)) %>%
  group_by(camID, year_season) %>%
  summarise(bear_activity = sum(bear, na.rm = T), .groups = "drop") %>%
  mutate(bear_presence = ifelse(bear_activity > 0, 1, 0))

apex_activity_weekly<- panther_activity_weekly %>%
  left_join(., coyote_activity_weekly, by = c("camID", "week")) %>%
  left_join(., human_activity_weekly, by = c("camID", "week")) %>%
  left_join(., bear_activity_weekly, by = c("camID", "week")) 
names(apex_activity_weekly)[3:10] <- paste0(names(apex_activity_weekly)[3:10], "_weekly")

# join to detection data for overlap analyses
camera_parsed<- camera_parsed%>%
  left_join(., apex_activity_weekly, by = c("week", "camID"))
############### ACTIVITY ##################
# Bin activity
camera_parsed_cov <- camera_parsed %>%
  mutate(human_bin = cut(human_activity_weekly, breaks = quantile(human_activity_weekly, probs = c(0, 0.5, 1), na.rm = TRUE),
                         labels = c("low", "high"), include.lowest = TRUE),
         temp_bin = cut(tmax, breaks = quantile(tmax, probs = c(0, 0.5, 1), na.rm = TRUE),
                         labels = c("low", "high"), include.lowest = TRUE),
         swi_bin = cut(swi, breaks = quantile(swi, probs = c(0, 0.5, 1), na.rm = TRUE),
                       labels = c("low", "high"), include.lowest = TRUE),
         panther_bin = ifelse(panther_presence_weekly == 0, "low", "high")) # making panther binary presence
extract_activity <-function(species_name, season_name, binning_cov, bin) {
  camera_parsed_cov %>%
    filter(season == season_name, !!sym(binning_cov) == bin,
           sp == species_name | sp_1 == species_name | sp_2 == species_name | sp_3 == species_name) %>%
    pull(solar)
}
# Then inside your loop:
binning_cov<- c("human_bin", "temp_bin")
bins<- c("low", "high")
season_name<- c("wet", "dry")

# store results
overlap_results <- list()
plot_list <- list()
for (covs in binning_cov) {
  for(b in bins){
    for (s in seasons) {
  panther_act <- extract_activity("panther", s, covs, b)
  for (sp in c("raccoon", "bobcat", "coyote", "bear")) {
    key <- paste(sp, s, covs, b, sep = "_")
    sp_act <- extract_activity(sp, s, covs, b)
    if (length(sp_act) < 50 | length(panther_act) < 50) {
      dhat<- "Dhat1"
      n<- 1
    } else {
      dhat<- "Dhat4"
      n<- 2} # adjust for Dha1 vs Dhat4 
    # Estimate overlap (returns Dhat1, Dhat4, Dhat5)
    overlap_vals <- overlapEst(panther_act, sp_act)
    # Bootstrap CI (using Dhat4: circadian overlap)
    panther_boot <- overlap::resample(panther_act, 1000)
    sp_boot <- overlap::resample(sp_act, 1000)
    boot_out <- overlap::bootEst(panther_boot, sp_boot, type = dhat)  # Dhat4 vs Dhat1
    ci <- quantile(boot_out, probs = c(0.025, 0.975), na.rm = TRUE)
    # Save results
    overlap_results[[key]] <- list(
      species = sp,
      season = s,
      covariate = covs,
      level = b,
      Dhat4 = unname(overlap_vals[n]),
      lower = unname(ci[1]),
      upper = unname(ci[2]),
      n_panther = length(panther_act),
      n_other_species = length(sp_act)
    )
    # Generate plot
    overlapPlot(panther_act, sp_act, rug = TRUE, main = paste("Panther vs", sp, "-", cov, "-", b))
    legend(
      'top',
      legend = c(
        paste("Panther (n = ", length(panther_act), ")", sep = ""),
        paste(sp, " (n = ", length(sp_act), ")", sep = "")
      ),
      lty = c(1, 2),
      col = c(1, 4),
      bty = 'n',
      cex = 0.85
    )
    plot_list[[key]] <- recordPlot()
  }
}
  }
}

summary_df <- bind_rows(overlap_results) %>%http://127.0.0.1:42153/graphics/plot_zoom_png?width=1076&height=783
  select(species, season, covariate, level, Dhat4, lower, upper, n_panther, n_other_species)
# Plot
ggplot(summary_df, aes(x = species, y = Dhat4, color = level)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6), width = 0.2) +
  labs(
    title = "Activity Overlap with Panthers by Season",
    x = "Species",
    y = expression(hat(Δ)[4]),
    color = "Season"
  ) +
  facet_wrap(season ~ covariate) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 14),
    axis.title = element_text(size = 14, face = "bold", vjust = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )
ggsave("C:/Users/kukwh001/OneDrive - Texas A&M University - Kingsville/Desktop/Documents/Research Projects/Florida_mesocarn/3_output/activity_overlap_plot_byseason.png", width = 10, height = 6, dpi = 300)

# Set layout (adjust rows/cols as needed)
  # Create a directory to store the plots (optional)
setwd("C:/Users/kukwh001/OneDrive - Texas A&M University - Kingsville/Desktop/Documents/Research Projects/Florida_mesocarn/3_output/overlap_plots")
# Loop through each recorded plot and save it
for (i in seq_along(plot_list)) {
  key <- names(plot_list)[i]  # e.g., "fox_wet"
  file_name <- paste0("overlap_plots/", key, ".png")
  replayPlot(plot_list[[i]])
  ggsave(filename = file_name, width = 800, height = 600)
  dev.off()
}

###### activity based on spatial features... #####



