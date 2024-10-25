---
title: "MTA Data Challenge"
format: 
  html:
    code-fold: true
    toc: true
---

::: {.cell}
<style type="text/css">
.basic {
  border: 3px solid lightblue;
}
</style>
:::

::: {.cell}

:::



# Code for Map Creation

This page shows all the code written to create the map `.gif` on the home page. Our methodology written in words can be found under the **Methods** tab.

## Loading libraries

These are the libraries needed to run the code.



::: {.cell}

```{.r .basic .cell-code}
library(tidyverse)
library(janitor)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(tmap)
library(leaflet)
library(knitr)
library(gganimate)
library(lubridate)
```
:::



## Loading data

We combine 4 data sets to create the points and lines on the final map. The first two datasets provide information on capital projects. We combine these two datasets so that we know the timeline, location, and type of each project. The second two datasets provides information on additional journey time (AJT) for each subway line for each month. These datasets are stratified by year (2015-2019) and 2020 on, so they need to be combined.



::: {.cell}

```{.r .basic .cell-code}
# Loading data for the project locations
location <- read_csv("data/project_locations.csv") %>% clean_names()

# Loading data for the project summaries, which show project progress
summary <- read_csv("data/project_summary.csv") %>% 
  clean_names() %>% 
  select(-c("agency_name", "category_description", 
            "element_description", "project_description",
            "location_indicator", "capital_plan"))

# Joining the project location and progress data and simplifying the categories
project_data <- location %>% 
  left_join(summary, by = "project_number") %>% 
  filter(current_quarter_flag == "CQ") %>% 
  filter(agency_name == "New York City Transit") %>% 
  mutate(category_description = 
           case_when(category_description %in% 
                       c("SIGNALS & COMMUNICATIONS",
                         "SIGNALS AND COMMUNICATIONS") ~ 
                       "SIGNALS & COMMUNICATION",
                     category_description %in% c("MISC./EMERGENCY", "MISCELLANEOUS")  ~ 
                       "MISCELLANEOUS/EMERGENCY",
                     category_description %in% c("SHOPS AND YARDS", "YARDS") ~
                       "SHOPS & YARDS",
                     category_description %in% c("TRACTION POWER", "POWER") ~ "POWER/TRACTION POWER",
                     TRUE ~ category_description)
  ) %>% 
  filter(!category_description %in% c("DEPOTS", 
                                      "BUSES", 
                                      "SHOPS & YARDS"))

journey_data1 <- read_csv("data/journey_time.csv")
journey_data2 <- read_csv("data/journey_time_2020.csv")

journey_data <- rbind(journey_data1, journey_data2) %>% 
  filter(period == "peak") %>% 
  select(-period) %>% 
  filter(line != "Systemwide") %>% 
  mutate(line = case_when(line %in% c("S Fkln", "S Rock", "S 42nd") ~ "S",
                          line == "JZ" ~ "J-Z",
                          TRUE ~ line)) %>% 
  mutate(ajt = total_apt + total_att) %>% 
  select(month, line, ajt) %>% 
  mutate(month = as.character(month))
```
:::



Now we have two datasets: one for projects (circles on the map) and one for additional journey time per line (lines on the map). However, we have to do some data manipulation so that the datasets include entries for each project, the completion percentage of the project, each line, and each AJT amount for each month and each year. To do this, we write some for loops to add entries to the datasets.



::: {.cell}

```{.r .basic .cell-code}
# Reformatting the date variable here to make the iteration easier
project_data <- project_data %>% 
  mutate(project_id = paste(project_number, project_number_sequence, sep = "")) %>%
  filter(!is.na(current_start_year), !is.na(current_completion_year)) %>% 
  mutate(current_start_month = if_else(is.na(current_start_month), 
                                       01, 
                                       current_start_month),
         current_completion_month = if_else(is.na(current_completion_month),
                                            01,
                                            current_completion_month)) %>% 
  mutate(start_date = as.Date(paste(current_start_year, 
                                           current_start_month, 
                                           01,
                                           sep = "-"), "%Y-%m-%d")) %>% 
  mutate(end_date = as.Date(paste(current_completion_year, 
                                         current_completion_month, 
                                          01,
                                  sep = "-"), "%Y-%m-%d")) %>% 
  mutate(date = start_date,
         project_left = 100) %>% 
  # Start in 2015 because this is where the AJT data starts
  filter(start_date > "2014-12-01") %>% 
  select(project_id, category_description, latitude, longitude, location,
         start_date, end_date, date, project_left)

# Begin the iteration to add rows to the dataset
for (i in unique(project_data$project_id)) {
  curr_row_num <- which(project_data$project_id == i)
  curr_row <- project_data[curr_row_num,]
  # Get number of months the project is active
  start <- project_data$start_date[curr_row_num]
  end <- project_data$end_date[curr_row_num]
  num_months <- interval(start, end) %/% months(1)
  if(num_months > 0) {
    rate <- 100/num_months
    for (j in 1:num_months) {
      curr_row$date <- start %m+% months(j)
      curr_row$project_left <- if_else(j == num_months,
                                     0,
                                     (num_months - j)*rate)
      project_data[nrow(project_data) + 1,] = curr_row
    }
  }
}

dates <- as.character(seq(as.Date("2015-01-01"), as.Date("2024-08-01"), by="months"))

grab_ajt <- function(line_name, date) {
  
  ajt <- journey_data %>% 
    filter(month == date) %>% 
    filter(line == line_name) %>% 
    pull(ajt)
  
  if (length(ajt) == 0) {
    ajt <- 0
  }
  
  return(ajt)
}

add_new_row <- function(line_combo, ajt_val) {
  journey_data %>% 
    add_row(month = date,
            line = line_combo,
            ajt = ajt_val)
}

for (date in dates) {
  
  ajts <- map(.x = list(B = "B", D = "D", FF = "F", M = "M", N = "N", Q = "Q", 
                   R = "R", A = "A", C = "C", W = "W", E = "E", l1 = "1",
                   l2 = "2", l3 = "3", l4 = "4", l5 = "5", l6 = "6"), 
              .f = grab_ajt,
              date)
  
  journey_data <- add_new_row("B-D", mean(ajts$B, ajts$D))
  journey_data <- add_new_row("B-D-F-M", mean(ajts$B, ajts$D, ajts$FF, ajts$M)) 
  journey_data <- add_new_row("N-Q-R", mean(ajts$N, ajts$Q, ajts$R)) 
  journey_data <- add_new_row("N-Q", mean(ajts$N, ajts$Q)) 
  journey_data <- add_new_row("N-R", mean(ajts$N, ajts$R)) 
  journey_data <- add_new_row("F-M", mean(ajts$FF, ajts$M)) 
  journey_data <- add_new_row("A-C", mean(ajts$A, ajts$C)) 
  journey_data <- add_new_row("1-2-3", mean(ajts$l1, ajts$l2, ajts$l3)) 
  journey_data <- add_new_row("4-5-6", mean(ajts$l4, ajts$l5, ajts$l6)) 
  journey_data <- add_new_row("N-W", mean(ajts$N, ajts$W)) 
  journey_data <- add_new_row("2-3", mean(ajts$l2, ajts$l3)) 
  journey_data <- add_new_row("4-5", mean(ajts$l4, ajts$l5))
  journey_data <- add_new_row("A-C-E", mean(ajts$A, ajts$C, ajts$E)) 
  journey_data <- add_new_row("N-Q-R-W", mean(ajts$N, ajts$Q, ajts$R, ajts$W)) 
  journey_data <- add_new_row("N-R-W", mean(ajts$N, ajts$R, ajts$W))
  journey_data <- add_new_row("R-W", mean(ajts$R, ajts$W))
}
```
:::



Finally, we use shape data to be able to map the subway lines and the location of the projects in NYC.



::: {.cell}

```{.r .basic .cell-code}
lines <- st_read("nyc_subways/lines.shp")
```

::: {.cell-output .cell-output-stdout}

```
Reading layer `lines' from data source 
  `/Users/safiyasirota/Desktop/research/mta_challenge/nyc_subways/lines.shp' 
  using driver `ESRI Shapefile'
Simple feature collection with 742 features and 6 fields
Geometry type: LINESTRING
Dimension:     XY
Bounding box:  xmin: -74.03088 ymin: 40.57559 xmax: -73.75541 ymax: 40.90312
Geodetic CRS:  WGS84(DD)
```


:::

```{.r .basic .cell-code}
# We exclude Staten Island since there are no subway data there
nyc <- st_read("nyc_base_map/nyc.shp") %>% 
  filter(boro_name != "Staten Island")
```

::: {.cell-output .cell-output-stdout}

```
Reading layer `nyc' from data source 
  `/Users/safiyasirota/Desktop/research/mta_challenge/nyc_base_map/nyc.shp' 
  using driver `ESRI Shapefile'
Simple feature collection with 5 features and 4 fields
Geometry type: MULTIPOLYGON
Dimension:     XY
Bounding box:  xmin: -74.25559 ymin: 40.49613 xmax: -73.70001 ymax: 40.91553
Geodetic CRS:  WGS84(DD)
```


:::

```{.r .basic .cell-code}
# Turn the project data into a form that can be mapped
proj_locs <- st_as_sf(project_data, coords = c("longitude", "latitude"), 
                      crs = 4326, agr = "constant") %>% 
  filter(date <= "2024-08-01") %>% 
  mutate(date = as.factor(date))

# Combine the AJT data with the geography of the lines so it can be mapped
line_data <- lines %>% 
  rename(line = name) %>% 
  select(-c(url, id, objectid, rt_symbol)) %>% 
  left_join(journey_data, by = "line") %>%
  rename(date = month) %>% 
  mutate(date = as.factor(date))
```
:::



## Mapping

Now we use the `tmap` package to create the final map.



::: {.cell}

```{.r .basic .cell-code}
# Making sure our final .gif includes all the frames
tmap_options(facet.max = 200)

# Create the map
urb_anim <- tm_shape(nyc) + 
  tm_polygons(fill = "white") + 
  tm_shape(line_data) +
  tm_lines(col = "ajt", lwd = 3.5, palette = "heat" ) +
  tm_scale_intervals(style = "pretty") +
  tm_facets_wrap(by = "date",
                 nrow = 1, ncol = 1) +
  tm_shape(proj_locs) + 
  tm_symbols(fill = "category_description", size = "project_left",
             fill_alpha = 0.5, col_alpha = 0) +
  tm_polygons() + 
  tm_facets_wrap(by = "date",
                 nrow = 1, ncol = 1)

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 30)
```

::: {.cell-output .cell-output-stdout}

```
Creating frames
```


:::

::: {.cell-output .cell-output-stdout}

```

Creating animation
Animation saved to /Users/safiyasirota/Desktop/research/mta_challenge/urb_anim.gif 
```


:::
:::

