setwd("/home/eclipse/Documents/HarborSpace/DataVisualization/data-visualization-fp")

library(tidyverse)
library(rcrimeanalysis)
library(dplyr)

# Create theme
theme_HS <- list(theme(
    plot.title = element_text(lineheight = 1, size = 16, face = "bold"),
    plot.subtitle = element_text(vjust = 0.5, size = 12, colour = "black"),
    plot.caption = element_text(vjust = 0.5, size = 12, colour = "black"),
    legend.title = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black", face = "bold"),
    legend.text = element_text(colour = "black", size = 15),
    # legend.position=c(1,0.025),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = NA),
    legend.key.size = unit(1.5, "lines"),
    strip.text = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black", face = "bold"),
    axis.title.x = element_text(colour = "black", size = 15),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black"),
    axis.title.y = element_text(colour = "black", size = 15),
    axis.text.y = element_text(vjust = 0.5, size = 15, colour = "black"),
    panel.grid.major = element_line(colour = "#E6E6E6"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF")
))

crime_df <- crimes

# Number of cases per year
ggplot(
    crime_df %>% count(year, sort = TRUE),
    aes(x = year, y = n)
) +
    geom_bar(
        stat = "identity",
        size = 0.01,
        color = "blue"
    ) +
    labs(
        title = "Cases reported",
        subtitle = "by year",
        x = "",
        y = "Number of cases reported",
    ) +
    theme_HS +
    theme(legend.position = "bottom")

ggsave("results/Num_cases_per_year.png",
    scale = 1,
    height = 8,
    width = 10,
    dpi = 300
)

# PERCENT OF CHANGE
crime_df %>% count(year, sort = TRUE)

# Fixed crimes per year
crime_df_fixed <- crime_df %>%
    separate(date, c("day", "time"), sep = " ") %>%
    separate(day, c("month", "num_day", "year"), sep = "/")

crime_df_fixed$month <- as.integer(crime_df_fixed$month)
crime_df_fixed$year <- as.integer(crime_df_fixed$year)

crime_df_fixed <- crime_df_fixed %>% filter(month < 9)
crime_df_fixed_2 <- crime_df_fixed %>% mutate(months = ((year - 2017) * 8 + month))
ggplot(
    crime_df_fixed_2 %>% count(months),
    aes(x = months, y = n, group = 1)
) +
    geom_point(size = 2, shape = 23) +
    scale_x_continuous(breaks = seq(0, 24, by = 9)) +
    labs(
        title = "Evolution of Crime",
        subtitle = "from 2017 to 2019",
        x = "",
        y = "Crimes",
    )

ggsave("results/evolution_scatter_year_not_fix.png",
    scale = 1,
    height = 8,
    width = 12,
    dpi = 300
)

ggplot(
    crime_df_fixed %>% count(year),
    aes(x = year, y = n, group = 1)
) +
    geom_line(color = "#DF013A", size = 1.25) +
    geom_point() +
    scale_y_continuous(limits = c(0, 7000)) +
    scale_x_continuous(breaks = seq(2017, 2019, by = 1)) +
    labs(
        title = "Evolution of Crime",
        subtitle = "from 2017 to 2019",
        x = "",
        y = "Crimes",
    )

ggsave("results/evolution_year.png",
    scale = 1,
    height = 8,
    width = 12,
    dpi = 300
)

# Cases per year corrected
ggplot(
    crime_df %>% filter() %>% count(year, sort = TRUE),
    aes(x = year, y = n)
) +
    geom_bar(
        stat = "identity",
        size = 0.01,
        color = "blue"
    ) +
    labs(
        title = "Cases reported",
        subtitle = "by year",
        x = "",
        y = "Number of cases reported",
    ) +
    theme_HS +
    theme(legend.position = "bottom")


# DOMESTIC CRIMES

# DOMESTIC VS NOT DMOESTIC
domestic <- crime_df %>%
    count(domestic, sort = TRUE)

domestic$n <- as.integer(domestic$n)

domestic <- domestic %>% mutate(per = n / sum(20893, 4107))

ggplot(
    domestic,
    aes(x = "", y = per, fill = domestic)
) +
    theme_HS +
    geom_bar(stat = "identity", width = 10) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = round(per * 100, 2)),
        size = 10,
        position = position_stack(vjust = 0.5)
    ) +
    theme_void()

ggsave("results/total.png",
    scale = 1,
    height = 10,
    width = 15,
    dpi = 300
)

# DOMESTIC PER CRIME TYPE
domestic <- crime_df %>% filter(domestic == TRUE)
domestic_count <- domestic %>%
    count(primary_type, sort = TRUE) %>%
    filter(rank(desc(n)) <= 6)

ggplot(
    domestic_count,
    aes(x = reorder(primary_type, -n), y = n)
) +
    geom_bar(
        stat = "identity",
        size = 0.01,
        color = "blue"
    ) +
    labs(
        title = "Cases reported",
        subtitle = "by year",
        x = "",
        y = "Number of cases reported",
    ) +
    theme_HS +
    theme(text = element_text(size = 10)) +
    theme(legend.position = "bottom")


ggsave("results/domestic_statistics.png",
    scale = 1,
    height = 10,
    width = 15,
    dpi = 300
)

domestic_count_loc <- domestic %>%
    count(location_description, sort = TRUE) %>%
    filter(rank(desc(n)) <= 6)

ggplot(
    domestic_count_loc,
    aes(x = reorder(location_description, -n), y = n)
) +
    geom_bar(
        stat = "identity",
        size = 0.01,
        color = "blue"
    ) +
    labs(
        title = "Cases reported",
        subtitle = "by year",
        x = "",
        y = "Number of cases reported",
    ) +
    theme_HS +
    theme(text = element_text(size = 10)) +
    theme(legend.position = "bottom")


ggsave("results/domestic_loc_statistics.png",
    scale = 1,
    height = 10,
    width = 15,
    dpi = 300
)
# ====

# Pie Chart
type_of_crime <- crime_df %>%
    count(primary_type, sort = TRUE) %>%
    filter(rank(desc(n)) <= 5)

type_of_crime <- type_of_crime %>%
    mutate(prop = n / sum(type_of_crime$n) * 100) %>%
    mutate(ypos = cumsum(prop) - 0.5 * prop)

ggplot(
    type_of_crime,
    aes(x = "", y = n, fill = primary_type)
) +
    theme_HS +
    geom_bar(stat = "identity", width = 10) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = round(prop, 2)),
        size = 10,
        position = position_stack(vjust = 0.5)
    ) +
    theme_void()


ggsave("results/top_5_cases.png",
    scale = 1,
    height = 10,
    width = 12,
    dpi = 300
)

# all
type_of_crime <- crime_df %>%
    count(primary_type, sort = TRUE) %>%
    filter(rank(desc(n)) <= 10)

ggplot(
    type_of_crime,
    aes(x = reorder(primary_type, -n), y = n)
) +
    geom_bar(
        stat = "identity",
        size = 0.01,
        color = "blue"
    ) +
    labs(
        title = "Cases reported",
        subtitle = "by year",
        x = "",
        y = "Number of cases reported",
    ) +
    theme_HS +
    theme(legend.position = "bottom")

ggsave("results/all_crimes.png",
    scale = 1,
    height = 10,
    width = 20,
    dpi = 300
)


# how many are arrested
new_crime <- crime_df %>% count(arrest, sort = TRUE)
new_crime <- new_crime %>% mutate(prop = n / sum(new_crime$n) * 100)

ggplot(
    new_crime,
    aes(x = "", y = prop, fill = prop)
) +
    geom_bar(stat = "identity", width = 10) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = prop),
        position = position_stack(vjust = 0.5)
    ) +
    labs(
        title = "Arrested vs not arrested",
        subtitle = "",
        x = "",
        y = ""
    ) +
    theme_void() +
    theme(text = element_text(size = 20))


ggsave("results/arrested.png",
    scale = 1,
    height = 8,
    width = 10,
    dpi = 300
)

# Assault arrested?
assault_crime <- crime_df %>% filter(primary_type == "ASSAULT")
assault_crime <- assault_crime %>% count(arrest, sort = TRUE)
assault_crime <- assault_crime %>% mutate(prop = n / sum(assault_crime$n) * 100)

ggplot(
    assault_crime,
    aes(x = "", y = prop, fill = arrest)
) +
    geom_bar(stat = "identity", width = 10) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = round(prop, 2)),
        position = position_stack(vjust = 0.5)
    ) +
    labs(
        title = "Arrested vs not arrested",
        subtitle = "",
        x = "",
        y = ""
    ) +
    theme_void() +
    theme(text = element_text(size = 20))


ggsave("results/arrested_assault.png",
    scale = 1,
    height = 8,
    width = 10,
    dpi = 300
)

# most crimes in proportion
# Assault arrested?
most_crime <- crime_df %>% count(primary_type, arrest, sort = TRUE)
most_crime <- most_crime %>% mutate()

assault_crime <- assault_crime %>% count(arrest, sort = TRUE)
assault_crime <- assault_crime %>% mutate(prop = n / sum(assault_crime$n) * 100)

ggplot(
    assault_crime,
    aes(x = "", y = prop, fill = arrest)
) +
    geom_bar(stat = "identity", width = 10) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = round(prop, 2)),
        position = position_stack(vjust = 0.5)
    ) +
    labs(
        title = "Arrested vs not arrested",
        subtitle = "",
        x = "",
        y = ""
    ) +
    theme_void() +
    theme(text = element_text(size = 20))


ggsave("results/arrested_assault.png",
    scale = 1,
    height = 8,
    width = 10,
    dpi = 300
)

# What time of day
time_df <- crime_df %>%
    separate(date, c("day", "time"), sep = " ") %>%
    separate(time, c("hour", "minute"), sep = ":")

time_df$hour <- as.integer(time_df$hour)

ggplot(
    time_df %>% count(hour),
    aes(x = hour, y = n, group = 1, fill = "#DF013A")
) +
    geom_area(color = "#DF013A", size = 1.25) +
    labs(
        title = "Crimes in Chicago during the day",
        x = "",
        y = "Number of Crimes",
        caption = ""
    ) +
    theme_HS

ggsave("results/chicago_hour.png",
    scale = 1,
    height = 8,
    width = 10,
    dpi = 300
)

# Location Description
location_df <- crime_df %>%
    count(location_description) %>%
    filter(rank(desc(n)) <= 10)


location_df[location_df == "PARKING LOT/GARAGE(NON.RESID.)"] <- "Park/Garage"
location_df[location_df == "RESIDENTIAL YARD (FRONT/BACK)"] <- "Yard"

ggplot(
    location_df,
    aes(x = reorder(location_description, -n), y = n)
) +
    geom_bar(
        stat = "identity",
        size = 0.01,
        color = "blue"
    ) +
    labs(
        title = "Cases reported",
        subtitle = "by year",
        x = "",
        y = "Number of cases reported",
    ) +
    theme_HS +
    theme(legend.position = "bottom")

ggsave("results/most_common_places.png",
    scale = 1,
    height = 10,
    width = 20,
    dpi = 300
)

# Location percentage
per_location_df <- location_df %>% mutate(por = n / sum(n))

ggplot(
    per_location_df,
    aes(x = "", y = por, fill = location_description)
) +
    geom_bar(stat = "identity", width = 10) +
    geom_text(aes(label = round(por * 100, 2)),
        size = 7,
        position = position_stack(vjust = 0.5)
    ) +
    theme(panel.background = element_rect(fill = "white"))

ggsave("results/pre_most_common_places.png",
    scale = 1,
    height = 10,
    width = 20,
    dpi = 300
)

# Adding map positions
a <- crimes
class(a)
names(a)
# convert to spatial object
a <- st_as_sf(a, coords = c("longitude", "latitude"), crs = 4326)

library(ggmap)
library(sf)
library(tidyverse)
## 6. get the bbox(bounding box) for the final map adding some margins space ####
bbox <- st_bbox(a)

left <- (as.numeric(bbox[1]) - mean(as.numeric(bbox[c(1, 3)]))) * 1.5 + mean(as.numeric(bbox[c(1, 3)]))
right <- (as.numeric(bbox[3]) - mean(as.numeric(bbox[c(1, 3)]))) * 1.5 + mean(as.numeric(bbox[c(1, 3)]))
bottom <- (as.numeric(bbox[2]) - mean(as.numeric(bbox[c(2, 4)]))) * 1.5 + mean(as.numeric(bbox[c(2, 4)]))
top <- (as.numeric(bbox[4]) - mean(as.numeric(bbox[c(2, 4)]))) * 1.5 + mean(as.numeric(bbox[c(2, 4)]))

## 7. Get a map tile for using it as background image #####
chicago <- get_stamenmap(
    bbox = c(
        left = left,
        bottom = bottom,
        right = right,
        top = top
    ),
    zoom = 12,
    maptype = c("toner-lite"),
    crop = TRUE,
    messaging = FALSE
)

chicago <- ggmap(chicago)
chicago

## 8. Plot the original John snow data and save the maps as an image. #####

theft <- crime_df %>% filter(primary_type == "THEFT")
theft <- st_as_sf(theft, coords = c("longitude", "latitude"), crs = 4326)
battery <- crime_df %>% filter(primary_type == "BATTERY")
battery <- st_as_sf(battery, coords = c("longitude", "latitude"), crs = 4326)
damage <- crime_df %>% filter(primary_type == "CRIMINAL DAMAGE")
damage <- st_as_sf(damage, coords = c("longitude", "latitude"), crs = 4326)
assault <- crime_df %>% filter(primary_type == "ASSAULT")
assault <- st_as_sf(assault, coords = c("longitude", "latitude"), crs = 4326)
deception <- crime_df %>% filter(primary_type == "DECEPTIVE PRACTICE")
deception <- st_as_sf(deception, coords = c("longitude", "latitude"), crs = 4326)


soho1 <- chicago +
    geom_sf(
        data = a,
        size = 1,
        color = "#0404B4",
        inherit.aes = FALSE
    ) +
    labs(
        title = "Cholera 1854",
        subtitle = "Soho",
        x = "\nLongitude",
        y = "Latidude\n",
        caption = "\nElaboration: Dr. Juan Galeano\nData: Dr. Jonh Snow"
    ) +
    theme_HS

soho1
ggsave("cholera_snow1.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

## 9. Plot a heatmad (2D density map) and save it #####
a <- cbind(a, st_coordinates(a))

soho2 <- chicago +
    stat_density2d(aes(
        x = X,
        y = Y,
        fill = ..level..
    ), # colour= ..level..
    alpha = .15,
    bins = 25,
    color = "black",
    size = .1,
    data = a,
    geom = "polygon"
    ) +
    scale_fill_gradient2("Reported Crime",
        low = "#2b83ba",
        mid = "#ffffbf",
        high = "#d7191c",
        midpoint = 50
    ) +
    labs(
        title = "All Crime Reports",
        subtitle = "Chicago",
        x = "\nLongitude",
        y = "Latidude\n",
    ) +
    theme_HS +
    theme(legend.position = "right")

soho2
ggsave("results/maps/all.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)


# THEFT
soho_theft <- chicago +
    geom_sf(
        data = theft,
        size = 1,
        color = "#0404B4",
        inherit.aes = FALSE
    ) +
    theme_HS

soho_theft
ggsave("cholera_snow1.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

theft <- cbind(theft, st_coordinates(theft))

soho2 <- chicago +
    stat_density2d(aes(
        x = X,
        y = Y,
        fill = ..level..
    ), # colour= ..level..
    alpha = .15,
    bins = 25,
    color = "black",
    size = .1,
    data = theft,
    geom = "polygon"
    ) +
    scale_fill_gradient2("Reported Crime",
        low = "#2b83ba",
        mid = "#ffffbf",
        high = "#d7191c",
        midpoint = 100
    ) +
    labs(
        title = "Theft Crime Reports",
        subtitle = "Chicago",
        x = "\nLongitude",
        y = "Latidude\n",
    ) +
    theme_HS +
    theme(legend.position = "right")

soho2
ggsave("results/maps/theft_map.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

# ASSAULT
soho1 <- chicago +
    geom_sf(
        data = assault,
        size = 1,
        color = "#0404B4",
        inherit.aes = FALSE
    ) +
    theme_HS

soho1
ggsave("cholera_snow1.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

assault <- cbind(assault, st_coordinates(assault))

soho2 <- chicago +
    stat_density2d(aes(
        x = X,
        y = Y,
        fill = ..level..
    ), # colour= ..level..
    alpha = .15,
    bins = 25,
    color = "black",
    size = .1,
    data = assault,
    geom = "polygon"
    ) +
    scale_fill_gradient2("Reported Crime",
        low = "#2b83ba",
        mid = "#ffffbf",
        high = "#d7191c",
        midpoint = 20
    ) +
    labs(
        title = "Assault Crime Reports",
        subtitle = "Chicago",
        x = "\nLongitude",
        y = "Latidude\n",
    ) +
    theme_HS +
    theme(legend.position = "right")

soho2
ggsave("results/maps/assault_map.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

# BATTERY
soho1 <- chicago +
    geom_sf(
        data = battery,
        size = 1,
        color = "#0404B4",
        inherit.aes = FALSE
    ) +
    theme_HS

soho1
ggsave("cholera_snow1.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

battery <- cbind(battery, st_coordinates(battery))

soho2 <- chicago +
    stat_density2d(aes(
        x = X,
        y = Y,
        fill = ..level..
    ), # colour= ..level..
    alpha = .15,
    bins = 25,
    color = "black",
    size = .1,
    data = battery,
    geom = "polygon"
    ) +
    scale_fill_gradient2("Reported Crime",
        low = "#2b83ba",
        mid = "#ffffbf",
        high = "#d7191c",
        midpoint = 2
    ) +
    labs(
        title = "Battery Crime Reports",
        subtitle = "Chicago",
        x = "\nLongitude",
        y = "Latidude\n",
    ) +
    theme_HS +
    theme(legend.position = "right")

soho2
ggsave("results/maps/battery_map.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

# CRIMINAL DAMAGE
soho1 <- chicago +
    geom_sf(
        data = damage,
        size = 1,
        color = "#0404B4",
        inherit.aes = FALSE
    ) +
    theme_HS

soho1
ggsave("cholera_snow1.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

damage <- cbind(damage, st_coordinates(damage))

soho2 <- chicago +
    stat_density2d(aes(
        x = X,
        y = Y,
        fill = ..level..
    ), # colour= ..level..
    alpha = .15,
    bins = 25,
    color = "black",
    size = .1,
    data = damage,
    geom = "polygon"
    ) +
    scale_fill_gradient2("Crime Reports",
        low = "#2b83ba",
        mid = "#ffffbf",
        high = "#d7191c",
        midpoint = 20
    ) +
    labs(
        title = "Damage Crime Reports",
        subtitle = "Chicago",
        x = "\nLongitude",
        y = "Latidude\n",
    ) +
    theme_HS +
    theme(legend.position = "right")

soho2
ggsave("results/maps/damage_map.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

# DECEPTIVE PRACTICE
soho1 <- chicago +
    geom_sf(
        data = deception,
        size = 1,
        color = "#0404B4",
        inherit.aes = FALSE
    ) +
    theme_HS

soho1
ggsave("cholera_snow1.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)

deception <- cbind(deception, st_coordinates(deception))

soho2 <- chicago +
    stat_density2d(aes(
        x = X,
        y = Y,
        fill = ..level..
    ), # colour= ..level..
    alpha = .15,
    bins = 25,
    color = "black",
    size = .1,
    data = deception,
    geom = "polygon"
    ) +
    scale_fill_gradient2("Crime Reports",
        low = "#2b83ba",
        mid = "#ffffbf",
        high = "#d7191c",
        midpoint = 50
    ) +
    labs(
        title = "Deceptive Practice Crime Reports",
        subtitle = "Chicago",
        x = "\nLongitude",
        y = "Latidude\n",
    ) +
    theme_HS +
    theme(legend.position = "right")

soho2
ggsave("results/maps/deception_map.png",
    scale = 1,
    height = 12,
    width = 20,
    dpi = 300
)
