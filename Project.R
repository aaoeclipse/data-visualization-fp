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


help(crimes)

crime_df <- crimes

View(crime_df)

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

# Pie Chart
type_of_crime <- crime_df %>%
    count(primary_type, sort = TRUE) %>%
    filter(rank(desc(n)) <= 10)

type_of_crime <- type_of_crime %>%
    arrange(desc(primary_type)) %>%
    mutate(prop = n / sum(data$n) * 100) %>%
    mutate(ypos = cumsum(prop) - 0.5 * prop)

ggplot(
    type_of_crime,
    aes(x = "", y = n, fill = primary_type)
) +
    geom_bar(stat = "identity", width = 10) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = n),
        size = 7,
        position = position_stack(vjust = 0.5)
    ) +
    theme_void()

ggsave("results/top_10_cases.png",
    scale = 1,
    height = 8,
    width = 10,
    dpi = 300
)

# how many are arrested

ggplot(
    crime_df %>% count(arrest, sort = TRUE),
    aes(x = "", y = arrest, fill = arrest)
) +
    geom_bar(stat = "identity", width = 10) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = n),
        position = position_stack(vjust = 0.5)
    ) +
    labs(
        title = "Arrested vs not arrested",
        subtitle = "",
        x = "",
        y = "",
    ) +
    theme(text = element_text(size = 20))

ggsave("results/arrested.png",
    scale = 1,
    height = 8,
    width = 10,
    dpi = 300
)

# What time of day
crime_df <- crime_df %>%
    mutate(hour = strsplit(strsplit(crime_df$date, split = "\ ")[[1]][2]), split = ":")


library(plotly)

library(rjson)


data <- fromJSON(file = "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")

data$features[[1]]

library(plotly)
library(rjson)

url <- "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"
counties <- rjson::fromJSON(file = url)
url2 <- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- read.csv(url2, colClasses = c(fips = "character"))
g <- list(
    scope = "usa",
    projection = list(type = "albers usa"),
    showlakes = TRUE,
    lakecolor = toRGB("white")
)
fig <- plot_ly()
fig <- fig %>% add_trace(
    type = "choropleth",
    geojson = counties,
    locations = df$fips,
    z = df$unemp,
    colorscale = "Viridis",
    zmin = 0,
    zmax = 12,
    marker = list(line = list(
        width = 0
    ))
)
fig <- fig %>% colorbar(title = "Unemployment Rate (%)")
fig <- fig %>% layout(
    title = "2016 US Unemployment by County"
)

fig <- fig %>% layout(
    geo = g
)

fig
