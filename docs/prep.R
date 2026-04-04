library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(here)

df_stations <- read_csv(here('docs/CEMP_Station_Metadata.csv'))
sightings <- fromJSON(readLines(here('docs/data.json')))

sightings <- sightings %>%
  mutate(
    Date = as.Date(as.numeric(Date) - 25569, origin = '1970-01-01'),
    month_year = format(Date, '%b %Y')
  )

current_month <- as.Date(format(Sys.Date(), '%Y-%m-01'))
month_starts <- sort(seq(from = current_month, by = '-1 month', length.out = 12))
month_years <- format(month_starts, '%b %Y')

sighting_flags <- sightings %>%
  select(Station, month_year, `Approximate Quantity`, first_sighting_raw = `Is this the first golden mussel sighting at this station?`) %>%
  distinct()

month_year_index <- tibble(month_year = month_years, month_year_idx = seq_along(month_years))

first_sighting_idx <- sighting_flags %>%
  left_join(month_year_index, by = 'month_year') %>%
  group_by(Station) %>%
  summarise(first_sighting_idx = min(month_year_idx))

frames_data <- crossing(
  df_stations %>% select(`Station Acronym`, Latitude, Longitude),
  tibble(month_year = month_years)
) %>%
  left_join(month_year_index, by = 'month_year') %>%
  left_join(sighting_flags, by = c('Station Acronym' = 'Station', 'month_year')) %>%
  left_join(first_sighting_idx, by = c('Station Acronym' = 'Station')) %>%
  mutate(
    first_sighting = !is.na(first_sighting_raw) & first_sighting_raw == 'Yes',
    category = case_when(
      `Approximate Quantity` == '100+' ~ '100+ Mussels',
      `Approximate Quantity` == '50-100' ~ '50-100 Mussels',
      !is.na(`Approximate Quantity`) ~ '1-50 Mussels',
      !is.na(first_sighting_idx) & first_sighting_idx < month_year_idx ~ 'Previous Sightings',
      TRUE ~ 'No Sightings'
    ),
    marker_color = case_when(
      category == '100+ Mussels' ~ '#8B0000',
      category == '50-100 Mussels' ~ '#FF4444',
      category == '1-50 Mussels' ~ '#FFB3B3',
      category == 'Previous Sightings' ~ '#4C78A8',
      TRUE ~ 'lightgray'
    ),
    hover_text = paste0(
      '<b>', `Station Acronym`, '</b><br>',
      category, '<br>',
      'Date: ', format(as.Date(paste('01', month_year), '%d %b %Y'), '%m/%d/%y')
    )
  ) %>%
  select(-first_sighting_raw, -first_sighting_idx) %>%
  arrange(`Station Acronym`, month_year_idx)

saveRDS(list(frames_data = frames_data, df_stations = df_stations), here('docs/frames_data.rds'))
