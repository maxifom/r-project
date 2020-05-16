library(dplyr)
library(tidyr)
library(plyr)

library(anomalize)
library(tibble)
library(tibbletime)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(purrr)
suppressMessages(suppressWarnings(library(dplyr)))


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

countries = c("United States", "Russian Federation", "Ghana")
sexes = list("Both sexes", "Male", "Female")

plots = list()
i = 1
for (c in countries) {
    for (s in sexes) {
      df_country_sex = df %>% filter(name == c & sex == s)

      df_obesity = df_country_sex %>%
        as_tbl_time(index = date) %>%
        as_period("yearly")

      tdf_obesity = df_obesity %>%
        time_decompose(obesity) %>%
        anomalize(remainder) %>%
        time_recompose() %>%
        plot_anomalies(time_recomposed = TRUE, alpha_dots = 1, color_no = "#228B22", color_yes = "red") +
        labs(title = c, subtitle = s) +
        xlab("Year") +
        ylab("Obesity %")
      plots[[i]] = tdf_obesity
      i = i + 1
    }
}

f = partial(arrangeGrob, ncol = 3, nrow = 3)
q = do.call(f, plots)
ggsave(file = "pdfs/anomaly_analysis.pdf", q)







