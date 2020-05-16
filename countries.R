library(dplyr)
library(tidyr)
library(sf)
library(spData)
library(plyr)
library(hash)

df = read.csv("obesity-cleaned.csv")
# Replace column names
names(df) = c('id', 'name', 'year', 'obesity', 'sex')

countries = distinct(df, name)

# Filter No data
df = df %>% filter(obesity != 'No data')

# Clean obesity column and convert to float
df = df %>%
  separate("obesity", "obesity", sep = " ", remove = TRUE, convert = TRUE) %>%
  transform(obesity = as.double(obesity)) %>%
  mutate(date = as.Date(ISOdate(year, 1, 1)))


w = world %>% select(name_long, geom)
names(w) = c('name', 'geom')

h = hash()

h[["Bolivia"]] = "Bolivia (Plurinational State of)"
h[["Czech Republic"]] = "Czechia"
h[["Dem. Rep. Korea"]] = "Democratic People's Republic of Korea"
h[["eSwatini"]] = "Eswatini"
h[["The Gambia"]] = "Gambia"
h[["Iran"]] = "Iran (Islamic Republic of)"
h[["Lao PDR"]] = "Lao People's Democratic Republic"
h[["Moldova"]] = "Republic of Moldova"
h[["Macedonia"]] = "Republic of North Macedonia"

h[["Syria"]] = "Syrian Arab Republic"
h[["United Kingdom"]] = "United Kingdom of Great Britain and Northern Ireland"
h[["Tanzania"]] = "United Republic of Tanzania"
h[["United States"]] = "United States of America"
h[["Venezuela"]] = "Venezuela (Bolivarian Republic of)"
h[["Vietnam"]] = "Viet Nam"


for (k in keys(h)) {
  w$name[w$name == k] = h[[k]]
}

df_with_countries = merge(df, w, by = "name")
