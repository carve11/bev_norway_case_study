options(encoding="UTF-8")
library(tidyverse)
library(lubridate)
library(rjstat)
library(ggplot2)
library(plotly)
library(htmlwidgets)

LINE_SIZE <- 0.8
PLOT_MARGIN <- unit(c(0.5,0.5,0.5,0.5), 'cm')
TITLE_FNT = list(size = 16)
LABEL_FNT = list(size = 14)
AXIS_FNT = list(size = 12)

fpath_root <- "/Users/jonasgravekristensen/Documents/Kursus/Coursera" %>% 
  paste("Google Data Analytics Certification", sep = "/") %>% 
  paste("Course 8 - Capstone project", sep = "/")
  
fpath_data <- paste(fpath_root, "data_capstone_project", sep = "/")
fpath_template <- paste(fpath_root, "templates", sep = "/")

fnames <- list(
  "first_time_reg" = "14020_20240610-083200.json",
  "ghg_road_traffic_main" = "13931_20240610-085437.json",
  "ghg_road_traffic_vehicle_type" = "13931_20240610-085621.json",
  "milage_type_fuel" = "12577_20240610-085102.json"
)

df_json <- function(fname) {
  f <- paste(fpath_data, fname, sep = "/")
  fromJSONstat(f) %>% 
    mutate_all(type.convert, as.is=TRUE)
}

html_ouput <- function(plot_obj, fname) {
  f <- paste(fpath_template, fname, sep = "/")
  saveWidget(plot_obj, file = f, selfcontained = FALSE)
}

plotly_style <- function(p_obj) {
  p_obj %>% layout(
    title = list(font = TITLE_FNT),
    xaxis = list(tickfont = AXIS_FNT),
    yaxis = list(tickfont = AXIS_FNT, title = list(font = LABEL_FNT)),
    legend = list(title = list(font = LABEL_FNT), font = AXIS_FNT)
    )
}

# ----------------------------------------
# first registration of passenger cars
# ----------------------------------------
df_first_time_reg <- df_json(fnames["first_time_reg"])

head(df_first_time_reg)
str(df_first_time_reg)
glimpse(df_first_time_reg)

fuel_type <- unique(df_first_time_reg$"type of fuel")

# split `month` column to `year` and `month`, 
# convert to numeric values and create date
df_first_time_reg <- separate(df_first_time_reg, month, into=c('year', 'month'), sep='M')
df_first_time_reg <- df_first_time_reg %>% 
  mutate(date = make_date(year, month))

# calculate total number of cars registred per month
# and calculate fraction of total per fuel type
df_first_time_reg <- df_first_time_reg %>% 
  group_by(date) %>% 
  mutate(num_cars_month = sum(value)) %>% 
  mutate("Share (%)" = round(value/num_cars_month*100, 2))

df_first_time_reg <- filter(
  df_first_time_reg, 
  (`type of fuel` != 'Other fuel')
)

p1 <- ggplot(data = df_first_time_reg) +
  geom_line(
    mapping = aes(x = date, y = value, color = `type of fuel`),
    size = LINE_SIZE
    ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 5, unit = "pt")),
    plot.margin = PLOT_MARGIN,
    legend.position='hidden'
    ) + 
  labs(
    title = "First time registered passenger cars by fuel type", 
    colour = "Type of fuel", 
    y="Number of cars"
    )
p2 <- ggplot(data = df_first_time_reg) +
  geom_line(
    mapping = aes(x = date, y = `Share (%)`, color = `type of fuel`),
    size = LINE_SIZE
    ) + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_text(margin = margin(r = 15, unit = "pt")),
    plot.margin = PLOT_MARGIN,
    legend.position='bottom'
    ) + 
  labs(colour = "Type of fuel", y="Percent of total (%)")

first_time_reg_p <- subplot(
  ggplotly(p1, height = 400) %>% plotly_style(), 
  style(ggplotly(p2, height = 400) %>% plotly_style(), showlegend = FALSE),
  nrows = 2,
  shareX = TRUE,
  titleY = TRUE
)

html_ouput(first_time_reg_p, "first_time_reg.html")
# ----------------------------------------
# GHG emissions from road traffic
# ----------------------------------------
df_ghg_road_traffic <- df_json(fnames["ghg_road_traffic_main"])

head(df_ghg_road_traffic)
str(df_ghg_road_traffic)
glimpse(df_ghg_road_traffic)

df_ghg_vehicle_type <- df_json(fnames["ghg_road_traffic_vehicle_type"])

head(df_ghg_vehicle_type)
str(df_ghg_vehicle_type)
glimpse(df_ghg_vehicle_type)

# merge the two df
total <- rbind(df_ghg_road_traffic, df_ghg_vehicle_type)

ghg_plot <- ggplot(data = total) +
  geom_line(
    mapping = aes(x = year, y = value, color = `source (activity)`),
    size = LINE_SIZE
  ) +
  theme(axis.title.x = element_blank()) + 
  labs(
    title = "Yearly emissions from road traffic, 2010-2023",
    colour = "Emission source", y="Emissions (1000 tonnes CO2e, AR5)") +
  scale_color_discrete(
    breaks=c('Road traffic', 'Passenger cars', 'Heavy duty vehicles', 'Light duty vehicles', 'Motorcycles and mopeds')
    )

html_ouput(ggplotly(ghg_plot, height = 400) %>% plotly_style(), "ghg_plot.html")

# in order to assess whether the change in GHG emissions is due to BEV or a
# change in distance covered we need to view the data of distance
df_milage <- df_json(fnames["milage_type_fuel"])

head(df_milage)
str(df_milage)
glimpse(df_milage)

milage_plot <- ggplot(data = df_milage) +
  geom_line(
    mapping = aes(x = year, y = value, color = `type of fuel`),
    linewidth = LINE_SIZE
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    ) + 
  labs(
    title = "Yearly distance, passenger cars by fuel type, 2010-2023", 
    colour = "Fuel type", y="Distance (million km)"
    )

html_ouput(ggplotly(milage_plot, height = 300) %>% plotly_style(), "milage_plot.html")
