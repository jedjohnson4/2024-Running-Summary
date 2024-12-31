## ==================== Load Required Libraries/Styling ========================

source("set_up.R", echo = FALSE)

## ==================== Read Raw Strava Activity Data ========================== 

strava <- read.csv('data/raw/activities.csv')

strava_mod <- strava |> 
  select(c(Activity.Date, Activity.Type, Activity.Private.Note, Distance, Elapsed.Time)) |> 
  rename(datetime = Activity.Date,
         type = Activity.Type,
         names = Activity.Private.Note,
         distance = Distance,
         time = Elapsed.Time) |>
  filter(type == "Run") |> 
  mutate(datetime = as.POSIXct(datetime, format = "%b %d, %Y")) |> 
  filter(datetime > as.POSIXct("2024-01-01")) |> 
  mutate(names = tolower(names)) |> 
  mutate(names = gsub(" ", "", names))

strava_names <- strava_mod$names

name_list <- c()

for (name in strava_names) {
  
  current = str_split(name, ",")
  
  name_list <- c(name_list, current[[1]])
  
}

athletes <- data.frame(names = name_list)

athletes_summarized <- athletes |> 
  filter(names != "",
         names != "good",
         names != "j",
         names != "relaxedpace",
         names != "funone",
         names != "fun",
         names != "jed",
         names != "goodrun",
         names != "cold",
         names != "add") |> 
  mutate(names = case_when(
    names == "michael" ~ "michaelmiller",
    names == "jacob?" ~ "jacob",
    .default = names
  )) |> 
  group_by(names) |> 
  summarize(count = n())



## ======================== Plot Days Run Heat Map =============================

days_run <- strava_mod |> 
  group_by(datetime) |> 
  summarize(distance = sum(distance),
            time = sum(time)) |> 
  mutate(datetime = as.Date(datetime))

days_of_year <- seq(ymd("2024-01-01"), ymd("2024-12-31"), by = "day")

row_number <- c()
column_number <- c()

for (i in seq(1, 52, 1)){
  for (j in seq(1, 7, 1)){
    row_number <- c(row_number, j)
    column_number <- c(column_number, i)
  }
}

row_number <- c(row_number, 1, 2, 3)
column_number <- c(column_number, 53, 53, 53)

row_number <- row_number[-1]
column_number <- column_number[-1]

year_2024 <- data.frame(day = days_of_year,
                        row = row_number,
                        col = column_number)

year_2024 <- year_2024 |> 
  mutate(didRun = case_when(
    day %in% days_run$datetime ~ T,
    .default = F))

combined_year_2024 <- left_join(year_2024, days_run, by = c("day" = "datetime"))

combined_year_2024 |> 
  ggplot() +
  geom_tile(aes(x = col, y = row, fill = didRun, alpha = distance),
            width = 0.8,
            height = 0.8,
            color = NA,
            show.legend = FALSE) +
  scale_fill_manual(values = c("grey80", "#146910")) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  coord_fixed() +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(1, 5, 9,14, 18, 22, 27, 31, 35, 40, 44, 48),
                     labels = month.abb,
                     sec.axis = dup_axis()) +
  scale_y_reverse(breaks = c(1, 2, 3, 4, 5, 6, 7),
                  labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
  labs(title = "**2024 RUNNING HEATMAP**",
       subtitle = "Color intensity correlates to distance",
       caption = plot_caption) +
  theme_void() +
  theme(
    axis.text.y = element_text(family = FONT_FAMILY,
                               color = "black",
                               size = 30,
                               margin = margin(r = 2),
                               hjust = 1),
    axis.text.x.bottom = element_blank(),
    axis.text.x.top = element_text(family = FONT_FAMILY,
                                   color = "black",
                                   size = 30,
                                   hjust = 0,
                                   angle = 90),
    plot.background = element_rect(fill = "white",
                                   color = NA),
    plot.margin = unit(c(5,10,5,5), "pt"),
    plot.title = element_markdown(family = FONT_FAMILY,
                                  size = 150,
                                  margin = margin(b = 5)),
    plot.subtitle = element_markdown(family = FONT_FAMILY,
                                     size = 60,
                                     margin = margin(b = 20)),
    plot.caption = element_markdown(family = FONT_FAMILY,
                                    size = 50,
                                    margin = margin(t = 10))
  )

ggsave(filename = "output/days-run.png",
       height = 3,
       width = 5.73,
       dpi = 800,
       units = "in")


## ======================== Plot Name Wordcloud ===============================

color_vector <- rep(c('#000000', '#0c1821', "#1b2a41", "#324a5f"), length.out = nrow(athletes_summarized))

athletes_summarized |> 
  # mutate(raw = count,
  #        count = sqrt(count)) |> 
  ggplot(aes(label = names, size = count, color = names)) +
  geom_text_wordcloud(eccentricity = 0.75,
                      family = FONT_FAMILY) +
  scale_size_area(max_size = 130) +
  labs(title = "**2024 RUNNING CREW**",
       subtitle = "Size corresponds to number of runs together",
       caption = plot_caption) +
  scale_color_manual(values = color_vector) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(family = FONT_FAMILY,
                                  size = 150),
    plot.subtitle = element_markdown(family = FONT_FAMILY,
                                     size = 60),
    plot.caption = element_markdown(family = FONT_FAMILY,
                                    size = 50),
    plot.background = element_rect(fill = "white",
                                   color = NA),
    plot.margin = margin(30, 10, 30, 10)
  )

ggsave(filename = "output/name-wordcloud.png",
       height = 5,
       width = 5,
       dpi = 800,
       units = "in")
