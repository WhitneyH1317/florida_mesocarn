
rm(list = ls())

# load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(purrr)
library(sf)
library(activity)
library(overlap)
library(terra)
# set path
camdata_path<- "0_data/cam_data_from_DeerDrive_1_29_2020/cam_data_from_DeerDrive_1_29_2020"
spatialdata_path<- "0_data/e_dist"

# List only .TXT files (case-sensitive)
txt_files <- list.files(camdata_path, pattern = "\\.TXT$", full.names = TRUE)
# account for sampling effort with cam operational matrix
ce <- read.csv("camera_operational_matrix.csv")
cam_locs<- data.frame(ce[,c(1:5)]) %>%
  st_as_sf(., coords = c("UTME", "UTMN"), crs = 26917)
# read in spatial data
    # List only the immediate subfolders
grid_dirs <- list.dirs(spatialdata_path, full.names = TRUE, recursive = FALSE)[1:11] # skipping info folder
    # Read all rasters
rasters <- lapply(grid_dirs, rast)
names(rasters) <- gsub("ed_", "", basename(grid_dirs))
r_stack <- rast(rasters)

# Read and combine them
camera_data <- txt_files %>%
  lapply(read.delim, stringsAsFactors = FALSE) %>%
  bind_rows() %>% 
  dplyr::select(-c(X.File.Name))

## Heather's code for fixing camera names: ##
fin<- camera_data
n <- str_sub(fin[,1], start=-8)
n2 <- strsplit(n,'.JPG')
n3 <- unlist(n2) #strsplit automatically creates a list, you have to remove this list
n4 <- strsplit(n3,'.jpg')
n5 <- unlist(n4) #strsplit automatically creates a list, you have to remove this list
n6 <- strsplit(n5,'.AVI')
n7 <- unlist(n6) #strsplit automatically creates a list, you have to remove this list

table(n7)

fin$camID <- n7
fin$camID <- gsub("_", "", fin$camID)

colnames(fin) <- c('File_Name','Keywords',"camID")

to.change <- fin

# remove those that don't contain A, B, or F 
fin <- fin[which(grepl("A", fin$camID) | grepl("B", fin$camID) | grepl("F", fin$camID)),]
# manual fixes
fin$camID <- gsub("-17FP31", "FP31", fin$camID)
fin$camID <- gsub("-42FP22", "FP22", fin$camID)
fin$camID <- gsub("0-12B21", "B22", fin$camID)
fin$camID <- gsub("0B2", "B26", fin$camID)
fin$camID <- gsub("1B2", "B26", fin$camID)
fin$camID <- gsub("2B2", "B26", fin$camID)
fin$camID <- gsub("3B2", "B26", fin$camID)
fin$camID <- gsub("4B2", "B26", fin$camID)
fin$camID <- gsub("5B2", "B26", fin$camID)
fin$camID <- gsub("6B2", "B26", fin$camID)
fin$camID <- gsub("7B2", "B26", fin$camID)
fin$camID <- gsub("8B2", "B26", fin$camID)
fin$camID <- gsub("9B2", "B26", fin$camID)
fin$camID <- gsub("B560", "B60", fin$camID)
camNam <- unique(fin$camID)
# manual species updating
fin$deer <- ifelse(grepl("deer", fin$Keywords, ",") ==T,1,0)
fin$deer_male <- ifelse(grepl("bucks=1", fin$Keywords, ",")==T, 1, 0)
fin$deer_female <- ifelse(grepl("does=1", fin$Keywords, ",")==T, 1, 0)
fin$deer_adults <- ifelse(grepl("deer", fin$Keywords, ",") ==T & grepl("spotfawns=1", fin$Keywords, ",")==F, 1, 0)
fin$panther <- ifelse(grepl("panther", fin$Keywords, ",") ==T,1,0)
fin$human <- ifelse(grepl("human", fin$Keywords, ",") ==T,1,0)
fin$vehicle <- ifelse(grepl("vehicle", fin$Keywords, ",") ==T,1,0)
fin$human_vehicle <- ifelse(fin$human == 1 | fin$vehicle == 1,1,0)

# dd a date column 
fin$DT <- as.POSIXct(str_sub(fin$File_Name, end=19), format="%Y-%m-%d-%H-%M-%S", tz = "EST")
fin0 <- fin[which(is.na(fin$DT)),]
fin <- fin[-which(is.na(fin$DT)),]
fin0$DT <- as.POSIXct(str_sub(fin0$File_Name, end=20), format="%Y-%m-%d--%H-%M-%S", tz = "EST")
fin <- rbind.data.frame(fin, fin0)
# filter to match the camera effort 
fin$date <- paste0(as.Date(fin$DT, tz='EST')) 
fin <- fin[which(fin$date >= "2015-01-01" & fin$date <= "2017-12-31"),]
fin <- fin[order(fin$camID, fin$DT),]

## create dataframe has columns for metadata ## 
  # Expand into long format by separating on comma
keywords_long <- fin %>%
  mutate(row_id = row_number()) %>%
  separate_rows(Keywords, sep = ",") %>%
  mutate(Keywords = str_trim(Keywords)) %>% # remove whitespace
  # Identify key-value vs standalone tags
  mutate(
    key = ifelse(str_detect(Keywords, "="), str_extract(Keywords, "^[^=]+"), "tag"),
    value = ifelse(str_detect(Keywords, "="), str_extract(Keywords, "(?<=\\=).*"), Keywords)
  ) %>%
  group_by(row_id) %>%
  filter(!any(value == "whiteout" | value == "blackout")) %>% # remove all entries that are blackout/whiteout
  ungroup() %>%
  # filter out unneeded rows
  filter(!grepl("cam", Keywords) & !grepl("bucks", Keywords) & !grepl("does", Keywords)) %>% # already have have a camID column, bucks, and does
  filter(key %in% c("sp", "grpsize", "grp")) %>%
  group_by(row_id, key) %>%
    # adding columns for sp 1, 2, 3, etc. 
  mutate(key = if_else(key == "sp" & n() > 1, paste0("sp_", row_number()), key),
         key = if_else(key == "grpsize" & n() > 1, paste0("grpsize_", row_number()), key)) %>%
  ungroup()
  # Pivot back to wide format
camera_parsed <- keywords_long %>%
  dplyr::select(row_id, key, value) %>%
  pivot_wider(names_from = key, values_from = value) %>%
  right_join(fin %>% mutate(row_id = row_number()), by = "row_id") %>%
  select(-row_id)

# filter out bird sightings
camera_parsed<- camera_parsed %>%
  filter(sp %in% c("spottedskunk", "hog", "pig", "housecat",
                   "mouse", "Fsquirrel", "armadillo", "squirrel", "fox", 
                   "cat", "coyote", "dog", "skunk", "alligator", "rabbit", "otter", 
                   "unknown", "bear", "raccoon", "bobcat", "deer", "panther", "opossum",
                   "human")) %>%
  filter(sp_1 %in% c(NA, "spottedskunk", "hog", "pig", "housecat",
                     "mouse", "Fsquirrel", "armadillo", "squirrel", "fox", 
                     "cat", "coyote", "dog", "skunk", "alligator", "rabbit", "otter", 
                     "unknown", "bear", "raccoon", "bobcat", "deer", "panther", "opossum",
                     "human")) %>%
  filter(sp_2 %in% c(NA, "spottedskunk", "hog", "pig", "housecat",
                     "mouse", "Fsquirrel", "armadillo", "squirrel", "fox", 
                     "cat", "coyote", "dog", "skunk", "alligator", "rabbit", "otter", 
                     "unknown", "bear", "raccoon", "bobcat", "deer", "panther", "opossum",
                     "human")) %>%
  filter(sp_3 %in% c(NA, "spottedskunk", "hog", "pig", "housecat",
                     "mouse", "Fsquirrel", "armadillo", "squirrel", "fox", 
                     "cat", "coyote", "dog", "skunk", "alligator", "rabbit", "otter", 
                     "unknown", "bear", "raccoon", "bobcat", "deer", "panther", "opossum",
                     "human")) 
# create list of carnivores
mesocarns<- c("skunk", "fox", "raccoon", "bobcat", "oposum", "otter")
apexes<- c("coyote", "bear")
carns<- c(mesocarns, "panther", "coyote", "bear")

# now filter detections that are less than 30 min apart (e.g. same species)
camera_parsed<- camera_parsed %>%
  mutate(sp = ifelse(sp == "spottedskunk", "skunk", sp),
         sp_1 = ifelse(sp_1 == "spottedskunk", "skunk", sp_1),
         sp_2 = ifelse(sp_2 == "spottedskunk", "skunk", sp_2),
         sp_3 = ifelse(sp_3 == "spottedskunk", "skunk", sp_3)) %>%
  arrange(camID, DT) %>%
  group_by(camID, sp) %>%
  mutate(sp_deltatime = difftime(DT, lag(DT), unit = "mins")) %>%
  filter(is.na(sp_deltatime) | sp_deltatime> 30) %>%
  ungroup() %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date),
    season = case_when(
      month %in% 2:6 ~ "dry",
      TRUE ~ "wet"  # includes Jan and Jul–Dec
    ),
    season_year = case_when(
      month == 1 ~ year - 1,  # assign Jan to previous year's wet season
      TRUE ~ year
    ),
    year_season = paste0(season, "_", season_year)
  ) %>%
  left_join(., cam_locs %>% rename(camID = Camera), by = "camID") %>%
  mutate(deltamins = difftime(DT, lag(DT), units = "mins")) %>%
  st_as_sf(.) %>%
  st_transform(., crs = 4326) %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()
# assign solar time
tmp <- solartime ( camera_parsed$DT, # the date time column 
                   camera_parsed$latitude,  # Latitude
                   camera_parsed$longitude, # Longitude
                   tz=-5,         # an offset in numeric hours to UTC (Romania is 2 hours ahead)
                   format="%Y:%m:%d %H:%M:%S")
camera_parsed$solar<- tmp$solar


# make species-specific counts by week
  # create a week and week-in-season
camera_parsed<- camera_parsed %>%
  mutate(date = as.Date(date),
         week = floor_date(date, unit = "week", week_start = 1))  # Round down to Monday
  
  # pivot so camID to week is a single row per species observation (may have duplicated camera-weeks for mult. spp detections)
long_sp <- camera_parsed %>%
  select(camID, week, sp, sp_1, sp_2, sp_3) %>%
  pivot_longer(cols = starts_with("sp"), names_to = "sp_col", values_to = "species") %>%
  filter(!is.na(species), species %in% carns)
  # aggregate to count species by week by camera
carnivore_counts <- long_sp %>%
  group_by( week, species) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0)
  # plot it
# Define 8 distinct colors (choose or generate your own)
my_colors <- c(
  "bear" = "darkblue",
  "bobcat" = "#d95f02",
  "fox" = "red",
  "panther" = "green",
  "raccoon" = "darkgreen",
  "coyote" = "yellow",
  "otter" = "black",
  "skunk" = "purple"
)
carnivore_counts %>%
  pivot_longer(
    cols = -week,
    names_to = "species",
    values_to = "count"
  ) %>%
  ggplot(., aes(x = week, y = count, color = species)) +
  scale_color_manual(values = my_colors) +
  geom_smooth(method = "loess", se = FALSE, span = 0.15, size = 1.5) +
  labs(
    title = "Weekly Carnivore Detection Counts Across Grids",
    x = "Week",
    y = "Weekly Detections Across Cameras",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # removes major gridlines
    panel.grid.minor = element_blank(),  # removes minor gridlines
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("3_output/weekly_carn_detections.png",
         last_plot(), width = 12, height = 6, dpi = 300)

# create detection data that has NA's when cameras were not operational, 0's if operational but not detected, 1's if detected
  # heather's code to deal with some issues? 
ce2 <- data.frame(t(ce[,c(1,6:ncol(ce))]))
colnames(ce2) <- paste0(ce2[1,])
# bring in the unique camera names saved from ealier and rename the cols 
colnames(ce2) <- camNam[order(as.character(camNam))]
rownames(ce2)[2:nrow(ce2)] <- gsub("X","", rownames(ce2)[2:nrow(ce2)])
ce3 <- ce2[-1,]
ce3$date  <- strptime(rownames(ce2)[2:nrow(ce2)], format="%d.%b.%Y")
rownames(ce3) <- ce3$date

# 1. Define all species of interest
carns

# 2. Prep detection data: one row per camera-date-species
detections_long <- camera_parsed %>%
  pivot_longer(cols = c(sp, sp_1, sp_2, sp_3), names_to = "sp_slot", values_to = "species") %>%
  filter(!is.na(species)) %>%
  distinct(camID, week, year_season, date, species) %>%
  filter(species %in% carns) %>%
  group_by(week, camID, year_season, species) %>%
  mutate(detected = n()) %>%
  distinct(week, camID, year_season, species, detected) %>%
  ungroup() 

# 3. Prep effort matrix: long format with camID + date + effort (1 = operational, 0 = not)
effort_long <- ce3 %>%
  pivot_longer(cols = -date, names_to = "camID", values_to = "effort") %>%
  mutate(
    date = as.Date(date),
    week = floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(week, camID) %>%
  summarize(effort = sum(as.numeric(effort))) %>%
  mutate(effort = ifelse(effort > 0, 1,effort))

# 4. Make full detection frame: expand all combinations of species x camID x date
detection_grid <- expand.grid(
  camID = unique(effort_long$camID),
  week = unique(effort_long$week),
  stringsAsFactors = FALSE
)

# 5. Join effort and detections
detections_matrix <- detection_grid %>%
  left_join(effort_long, by = c("camID", "week")) %>%
  left_join(detections_long, by = c("camID", "week")) %>%
  mutate(detected = case_when(
    is.na(effort) ~ NA_real_,       # shouldn't happen
    effort == 0 ~ NA_real_,         # not operational
    is.na(detected) ~ 0,            # operational but not detected
    TRUE ~ detected                        # detected
  )) %>%
  mutate(species = ifelse(is.na(species) & detected == 0, "any", species)) %>%
  select(species, camID, week, detected) %>%
  left_join(., cam_locs %>% rename(camID = Camera), by = "camID") %>%
  st_as_sf(.) %>%
  filter(!(is.na(species) & is.na(detected))) %>%
  mutate(
    week_mid = week + days(3),  # middle of the week
    year = lubridate::year(week_mid),
    month = lubridate::month(week_mid),
    season = case_when(
      month %in% 2:6 ~ "dry",
      TRUE ~ "wet"
    ),
    season_year = case_when(
      month == 1 ~ year - 1,
      TRUE ~ year
    ),
    year_season = paste0(season, "_", season_year)
  ) %>%
  distinct()
# add spatial data
detections_matrix <- st_transform(detections_matrix, crs(r_stack))
  # Extract raster stack values
vals <- terra::extract(r_stack, vect(detections_matrix))
  # Combine with sf geometry (optional)
detections_matrix_env <- cbind(detections_matrix, vals)

## add weekly and seasonal apex activity data per camera
 # determine activity of apex preds per camera
human_activity_weekly <- camera_parsed %>%
  rowwise() %>%
  mutate(human_vehicle = sum(human, vehicle, na.rm = F)) %>%
  ungroup() %>%
  group_by(camID, week) %>%
  summarise(human_activity = sum(human_vehicle), .groups = "drop") %>% # defined as detections per day
  mutate(human_presence = ifelse(human_activity > 0, 1, 0)) # presence is "there or not" 
human_activity_seasonal <- camera_parsed %>%
  rowwise() %>%
  mutate(human_vehicle = sum(human, vehicle, na.rm = F)) %>%
  ungroup() %>%
  group_by(camID, year_season) %>%
  summarise(human_activity = sum(human_vehicle), .groups = "drop") %>% # defined as detections per day
  mutate(human_presence = ifelse(human_activity > 0, 1, 0)) # presence is "there or not" 

coyote_activity_weekly <- camera_parsed %>%
  mutate(coyote = ifelse(sp == "coyote" | sp_1 == "coyote" | sp_2 == "coyote" | sp_3 == "coyote", 
                         1, 0)) %>%
  group_by(camID, week) %>%
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
  left_join(., bear_activity_weekly, by = c("camID", "week")) %>%
  left_join(., cam_locs %>% rename(camID = Camera), by = "camID") %>%
  st_drop_geometry(.) %>%
  dplyr::select(-c(Grid, Trail, geometry))
names(apex_activity_weekly)[3:10] <- paste0(names(apex_activity_weekly)[3:10], "_weekly")

apex_activity_seasonal<- panther_activity_seasonal %>%
  left_join(., coyote_activity_seasonal, by = c("camID", "year_season")) %>%
  left_join(., human_activity_seasonal, by = c("camID", "year_season")) %>%
  left_join(., bear_activity_seasonal, by = c("camID", "year_season")) %>%
  left_join(., cam_locs %>% rename(camID = Camera), by = "camID") %>%
  dplyr::select(-c(geometry))
names(apex_activity_seasonal)[3:10] <- paste0(names(apex_activity_seasonal)[3:10], "_seasonal")

# join to detection data for overlap analyses
detections_matrix<- detections_matrix_env %>%
  left_join(., apex_activity_seasonal, by = c("year_season", "camID", "Grid", "Trail")) %>%
  left_join(., apex_activity_weekly, by = c("week", "camID"))
# scale all env data now
scaling_values<- detections_matrix%>%
  st_drop_geometry() %>%
  dplyr::select(camID, ag:urban) %>%
  pivot_longer(., cols = ag:urban, values_to = "value") %>%
  group_by(name) %>%
  summarize(mean = mean(value), sd = sd(value))
# Create named vectors for means and sds
means <- setNames(scaling_values$mean, scaling_values$name)
sds   <- setNames(scaling_values$sd, scaling_values$name)

# Normalize each matching column
detections_matrix <- detections_matrix %>%
  mutate(across(
    .cols = all_of(names(means)),
    .fns = ~ (. - means[cur_column()]) / sds[cur_column()],
    .names = "{.col}_z"
  ))

# add temp data
library(climateR); library(earthdatalogin)
aoi_camgrid <- sf::st_union(cam_locs) %>% 
  sf::st_transform(4326)  # nClimGrid uses WGS84
Sys.setenv(EARTHDATA_USER = "kwhitneyhansen",
            EARTHDATA_PASS = "Ermagerdfires1")
# get climate data
system.time({
  temp_rast = getDaymet(AOI = aoi_camgrid, varname = "tmax", startDate = min(camera_parsed$date), 
                        endDate  = max(camera_parsed$date)) })
plot(temp_rast[[1]])  # first day
# Generate the dates based on start date
start_date <- min(camera_parsed$date)
end_date <- max(camera_parsed$date)
date_seq <- seq.Date(start_date, end_date, by = "day")
# Assign layer names to raster
names(temp_rast$tmax) <- as.character(date_seq)
# Convert sf to SpatVector if needed
cam_vect <- cam_locs %>%
  st_transform(., crs = crs(temp_rast[[1]])) %>% # and transform
  terra::vect(.)
  # Extract all days for each camera
temp_extract <- terra::extract(temp_rast$tmax, cam_vect)
temp_extract$camID <- cam_locs$Camera  # or whatever your ID column is
temp_long <- temp_extract %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "date",
    values_to = "tmax"
  ) %>%
  mutate(date = as.Date(date))  # Convert back to real Date
# add to camera_parsed
camera_parsed <- camera_parsed %>%
  left_join(temp_long, by = c("camID", "date"))
# make weekly averages 
temp_long_weekly<- temp_long %>% 
  mutate(
    week = floor_date(date, unit = "week", week_start = 1),
     ) %>%
  group_by(week, camID) %>%
  summarize(weekly_tmax = mean(tmax))
  
# add to matrix
detections_matrix<- detections_matrix %>% 
  left_join(., temp_long_weekly, by = c("week", "camID"))

## add water data
hydroList <- list.files("0_data/SurWaterIndex_RastersUTM_updated_3_27_19", 
                        pattern = '.tif', full.names = T)
names <- as.Date(gsub('.tif','', list.files("0_data/SurWaterIndex_RastersUTM_updated_3_27_19",
                        pattern = '.tif')), 
                        format = "%B-%d-%Y",
                        tz='EST')
rast_stack <- rast(hydroList)
names(rast_stack)<- names
cam_vect <- cam_locs %>%
  st_transform(., crs(rast_stack)) %>%
  vect(.)
extracted <- terra::extract(rast_stack, cam_vect)
extracted <- bind_cols(cam_locs, extracted[, -1])  # drop terra's ID column
long_water <- extracted %>%
  pivot_longer(cols = -c(Camera, Grid, Trail, geometry),  # adjust to your camera cols
               names_to = "date", values_to = "swi") %>%
  mutate(date = as.Date(date))
# join to cmaera parsed
camera_parsed<- camera_parsed %>%
  left_join(., long_water %>% rename(camID = Camera), by = c("camID", "date", "Grid", "Trail"))
# make weekly averaged for detection data
# make weekly averages 
water_long_weekly<- long_water %>% 
  mutate(
    week = floor_date(date, unit = "week", week_start = 1)
    ) %>%
  group_by(week, Camera) %>%
  summarize(weekly_swi = mean(swi)) %>%
  rename(camID = Camera) %>%
  st_drop_geometry()
# add to matrix
detections_matrix<- detections_matrix %>% 
  left_join(., water_long_weekly , by = c("week", "camID")) %>%
  dplyr::select(-ID) %>%
  distinct()

## save .rda's
save(camera_parsed, mesocarns, apexes, file = "2_tmp/daily_cam_data.rda")
save(detections_matrix, cam_locs, mesocarns, apexes, file = "2_tmp/weekly_detection_data.rda")

