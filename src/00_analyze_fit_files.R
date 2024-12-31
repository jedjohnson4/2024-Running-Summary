## ==================== Load Required Libraries/Styling ========================

source("set_up.R", echo = FALSE)


## ============================ Read FIT Files =================================

files <- list.files("data/raw/",
                    pattern = ".*.fit",
                    full.names = TRUE)
index = 1
activity_number = 1

combined <- data.frame()

for (file in files) {
  
  remove_file = TRUE
  
  temp_file <- FITfileR::readFitFile(file)
  
  temp_id <- FITfileR::getMessagesByType(temp_file, message_type = "file_id")
  
  session_id = temp_id$type[1]
  
  
  if (is.na(session_id)) {
    session_id = "none"
  }
  
  if (session_id == "activity") {
    temp_df <- FITfileR::records(temp_file)
    
    if (class(temp_df)[1] == "list") {
      temp_df <- data.frame(temp_df[1]) %>% 
        rename_with(~ str_remove(., "record_1."))
    }
    
    if (as.numeric(substr(temp_df$timestamp[1], start = 1, stop = 4)) >= 2024) {
      if (ncol(temp_df) == 9) {
        temp_df <- temp_df %>%
          mutate(activity = as.character(activity_number))
        
        combined <- rbind(combined, temp_df)
        
        activity_number = activity_number + 1
        
        remove_file = FALSE
      }
    }
  }
  
  if (remove_file) {
    file.remove(file)
  }
  
  percentage <- round((index/length(files)) * 100, 0)
  num_equal <- floor(percentage / 5)
  
  cat("\r", "|", rep("=", num_equal), rep("-", 20 - num_equal), "|", percentage, "%")
  
  index = index + 1
  
}

write.csv(combined, "data/clean/fit-data-compiled.csv")

combined <- read.csv("data/clean/fit-data-compiled.csv")

combined$timestamp <- as.POSIXct(combined$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")

## ====================== Plot Fayetteville Heatmap ============================

fay <- combined %>% 
  filter(position_long < -94) %>% 
  filter(position_lat < 36.2)

minlongfay <- min(fay$position_long); maxlongfay <- max(fay$position_long)
minlatfay <- min(fay$position_lat); maxlatfay <- max(fay$position_lat)

long_cen <- ((maxlongfay - minlongfay)/2) + minlongfay
lat_cen <- ((maxlatfay - minlatfay)/2)+ minlatfay
fay_map <- get_googlemap(center = c(lon= long_cen , lat = lat_cen),
                         zoom = 12,
                         maptype = "roadmap",
                         color = "color",
                         style = dark_style)

ggmap(fay_map) +
  geom_path(data = fay,
            aes(position_long, position_lat, group = activity),
            linewidth = 0.3,
            color = HIGHLIGHT_COLOR,
            alpha = 0.2) +
  theme_void()

ggsave(filename = "output/fayetteville-heatmap.png",
       height = 4,
       width = 4,
       dpi = 1000,
       units = "in")


## ======================= Plot Little Rock Heatmap ============================

lr <- combined %>% 
  filter(position_long > -92.4) |> 
  filter(position_lat > 34)

minlonglr <- min(lr$position_long); maxlonglr <- max(lr$position_long)
minlatlr <- min(lr$position_lat); maxlatlr <- max(lr$position_lat)

long_cen <- ((maxlonglr - minlonglr)/2) + minlonglr
lat_cen <- ((maxlatlr - minlatlr)/2)+ minlatlr
lr_map <- get_googlemap(center = c(lon= long_cen , lat = lat_cen),
                        zoom = 12,
                        maptype = "roadmap",
                        color = "color",
                        style = dark_style)


ggmap(lr_map) +
  geom_path(data = lr,
            aes(position_long, position_lat, group = activity),
            linewidth = 0.3,
            color = "#B10000",
            alpha = 0.2) +
  theme_void()

ggsave(filename = "output/littlerock-heatmap.png",
       height = 4,
       width = 4,
       dpi = 1000,
       units = "in")


## ======================== Histogram of Distances =============================

run_distances <- combined %>% 
  group_by(activity) %>% 
  summarize(total_distance = max(distance)) %>% 
  mutate(total_distance = round(total_distance / 1609.34, 2))

run_distances %>% 
  ggplot() +
  geom_histogram(aes(x = total_distance),
                 bins = 15,
                 fill = HIGHLIGHT_COLOR,
                 color = "white") +
  labs(x = "Distance (mi)",
       y = "Count") +
  # scale_y_continuous(limits = c(0,50),
  #                    breaks = c(0, 5, 10, 15, 20, 25, 30),
  #                    expand = c(0,0)) +
  theme_void() +
  my_theme

ggsave(filename = "output/histogram-of-distances.png",
       height = 4,
       width = 5,
       dpi = 1000,
       units = "in")

## ======================= Plot Avg HR vs. Avg Pace ============================

avg_speed_vs_hr <- combined %>% 
  group_by(activity) %>% 
  summarize(avg_speed = mean(speed),
            avg_hr = mean(heart_rate)) %>% 
  mutate(mile_per_min = avg_speed / 1609.34 * 60) %>% 
  mutate(min_per_mile = 1 / mile_per_min) %>% 
  mutate(mins = floor(min_per_mile)) %>% 
  mutate(seconds = round(min_per_mile %% mins * 60, 0)) %>% 
  mutate(pace = as.POSIXct(paste0(mins, ":", seconds), format = "%M:%S"))

avg_speed_vs_hr %>% 
  filter(min_per_mile < 14) %>% 
  ggplot(aes(x = min_per_mile, y = avg_hr)) +
  geom_smooth(method = "lm",
              color = HIGHLIGHT_COLOR) +
  geom_point(shape = 15,
             color = "black",
             size = 1.30) +
  geom_point(shape = 15,
             color = "white",
             size = .85) +
  scale_x_continuous(limits = c(5, 12),
                     breaks = c(5, 6, 7, 8, 9, 10, 11, 12),
                     labels = c("5:00", "6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00")) +
  scale_y_continuous(limits = c(129, 195),
                     breaks = c(130, 140, 150, 160, 170, 180, 190)) +
  labs(x = "Average Pace (min/mi)",
       y = "Average Heart Rate (bmp)",
       title = "**2024 AVERAGE HR VERSUS PACE**",
       subtitle = "Least squares regression with standard error plotted.",
       caption = plot_caption) +
  theme_void() +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 90,
                               hjust = 1,
                               margin = margin(t = -3)),
    plot.subtitle = element_markdown(margin = margin(b = -10))
  )

ggsave(filename = "output/hr-vs-pace.png",
       height = 4,
       width = 4,
       dpi = 800,
       units = "in")


## ========================== Histogram of Paces ===============================

mile_markers <- combined %>% 
  mutate(mile = floor(distance / 1609.34)) %>% 
  select(c(timestamp, distance, activity, mile)) %>% 
  mutate(newMile = ifelse(mile == lag(mile), FALSE, TRUE)) %>% 
  mutate(newMile = ifelse(is.na(newMile), TRUE, newMile)) %>% 
  mutate(sameActivity = ifelse(activity == lag(activity), TRUE, FALSE)) %>% 
  mutate(sameActivity = ifelse(is.na(sameActivity), FALSE, sameActivity)) %>% 
  filter(newMile == TRUE) %>% 
  mutate(dt = ifelse(sameActivity, timestamp - lag(timestamp), NA)) %>% 
  filter(!is.na(dt))

mile_markers %>% 
  filter(dt < 15) %>% 
  ggplot() +
  geom_histogram(aes(x = dt),
                 binwidth = 0.25,
                 fill = HIGHLIGHT_COLOR,
                 color = "white") +
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                     labels = c(paste0(seq(5, 15, 1), ":00"))) +
  scale_y_continuous(expand = c(0.02, 0),
                     limits = c(0,200)) +
  # geom_density(aes(x = dt, after_stat(count / 3.75))) +
  labs(x = "Pace (min/mile)",
       y = "Count",
       title = "**2024 HISTOGRAM OF PACES**",
       subtitle = "Bin width of 15 seconds, excluding paces over 15:00 min/mi",
       caption = plot_caption) +
  theme_void() +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 90,
                               hjust = 1)
  )

ggsave(filename = "output/histogram-of-paces.png",
       height = 4,
       width = 4,
       dpi = 800,
       units = "in")


## ============================ Miles Per Month ================================

mile_per_month <- combined |> 
  group_by(activity) |> 
  summarize(distance = max(distance) / 1609.34,
            date = as.Date(max(timestamp, na.rm = TRUE))) |> 
  mutate(month = month(date, label = TRUE, abbr = TRUE)) |> 
  group_by(month) |> 
  summarize(tot_distance = sum(distance))

mile_per_month |> 
  ggplot() +
  geom_col(aes(x = month, y = tot_distance),
           fill = HIGHLIGHT_COLOR) +
  theme_void() +
  labs(y = "Distance (miles)") +
  my_theme +
  theme(
    axis.title.x = element_blank()
  )

ggsave(filename = "output/monthly-distances.png",
       height = 4,
       width = 5,
       dpi = 1000,
       units = "in")


## ============================ Distance by Day ================================

distance_by_day <- combined |> 
  select(c(timestamp, distance)) |> 
  mutate(timestamp = as.Date(timestamp)) |> 
  group_by(timestamp) |> 
  summarize(distance = max(distance) / 1609.34) |> 
  mutate(day = wday(timestamp, label = TRUE)) |> 
  group_by(day) |> 
  summarise(tot_distance = sum(distance)) |> 
  filter(!is.na(day))

distance_by_day |> 
  ggplot() +
  geom_col(aes(x = day, y = tot_distance),
           fill = HIGHLIGHT_COLOR) +
  labs(y = "Distance (miles)") +
  theme_void() +
  my_theme +
  theme(
    axis.title.x = element_blank()
  )


## ======================= Cumulative Elevation Gain ===========================

elevation_gain <- combined |> 
  select(c(timestamp, altitude, activity)) |> 
  mutate(sameActivity = ifelse(activity == lag(activity), TRUE, FALSE)) |> 
  mutate(sameActivity = ifelse(is.na(sameActivity), FALSE, sameActivity)) |> 
  # mutate(altitude = altitude / 1609.34 * 5280) |> 
  mutate(dz = ifelse(sameActivity, ifelse(altitude - lag(altitude) > 0, altitude - lag(altitude), 0), 0)) |> 
  mutate(timestamp = as.Date(timestamp)) |> 
  group_by(timestamp) |> 
  summarize(elev_gain = sum(dz)) |> 
  filter(!is.na(timestamp)) |> 
  mutate(cum_elev_gain = cumsum(elev_gain))

elevation_gain |> 
  ggplot() +
  geom_line(aes(timestamp, cum_elev_gain)) +
  labs(y = "Total Elevation Gain (feet)") +
  theme_void() +
  my_theme +
  theme(
    axis.title.x = element_blank()
  )


