library(dplyr)
library(tidyr)
library(plyr)
#library(tidyverse)
library(rworldmap)
#library(raster)
#library(sf)
#library(spData)
#library(spDataLarge)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source", Ncpus = 8)
df = read.csv("obesity-cleaned.csv")
# Replace column names
names(df) = c('id', 'name', 'year', 'obesity', 'sex')

# Filter No data
df = df %>% filter(obesity != 'No data')

# Clean obesity column and convert to float
df = df %>%
  separate("obesity", "obesity", sep = " ", remove = TRUE, convert = TRUE) %>%
  transform(obesity = as.double(obesity))

df_countries = read.csv("countries.csv", sep = ",")

df = join(df, df_countries, by = 'name', type = "inner")

df_afg_all = df %>% filter(name == 'Afghanistan' & sex == 'Both sexes')
df_afg_male = df %>% filter(name == 'Afghanistan' & sex == 'Male')
df_afg_female = df %>% filter(name == 'Afghanistan' & sex == 'Female')
plot(df_afg_all$year, df_afg_all$obesity, col = 'green', xlab = "Both sexes", ylab = "Obesity %")
points(df_afg_male$year, df_afg_male$obesity, col = 'blue', xlab = "Male")
points(df_afg_female$year, df_afg_female$obesity, col = 'red', xlab = "Female")
