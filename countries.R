library(dplyr)
library(tidyr)
library(sf)
library(spData)
library(plyr)
library(ggplot2)
library(tidyverse)
library(ggplotify)
library(gridExtra)
df = read.csv("obesity-cleaned.csv")
# Replace column names
names(df) = c('id', 'name_long', 'year', 'obesity', 'sex')


# Filter No data
df = df %>% filter(obesity != 'No data')

# Clean obesity column and convert to float
df = df %>%
  separate("obesity", "obesity", sep = " ", remove = TRUE, convert = TRUE) %>%
  transform(obesity = as.double(obesity)) %>%
  mutate(date = as.Date(ISOdate(year, 1, 1)))

# Merge with world geo data
df_with_countries = merge(world, df, by = "name_long")

sexes = c("Male", "Female", "Both sexes")

years = c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)

plots = list()
i = 1

for (year in years) {
  for (s in sexes) {
    df1 = df_with_countries %>%
      filter(year == year & sex == s) %>%
      select(obesity)
    df2 = ggplot(df1) +
      geom_sf(aes(fill = obesity)) +
      theme(panel.background = element_rect(fill = 'azure'), legend.position = "bottom") +
      scale_fill_gradientn(colours = heat.colors(10, rev = TRUE)) +
      labs(title = sprintf("%d Obesity %%", year), subtitle = s)
    plots[[i]] = df2
    i = i + 1
  }
}

f = partial(arrangeGrob, ncol = 3, nrow = 3)


q = do.call(f, plots[1:9])
ggsave(file = "pdfs/geo_1975-1985.pdf", q)

q = do.call(f, plots[10:18])
ggsave(file = "pdfs/geo_1990-2000.pdf", q)

q = do.call(f, plots[19:27])
ggsave(file = "pdfs/geo_2005-2015.pdf", q)




