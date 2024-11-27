library(readr)
library(dplyr)
library(ggplot2)
library(scales)


dataset <- read_csv("/home/jordan/covid.csv")


data_BR_PE <- dataset %>%
  filter(location %in% c("Brazil", "Peru")) %>%
  select(date, location, total_deaths, population)


data_BR_PE$date <- as.Date(data_BR_PE$date)


data_BR_PE_summary <- data_BR_PE %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  summarize(deaths_per_million = sum((total_deaths / population) * 1e6, na.rm = TRUE)) %>%
  ungroup()


ggplot(data_BR_PE_summary, aes(x = deaths_per_million, y = location, fill = location)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Deaths per Million Inhabitants",
    subtitle = "Brasil x Peru",
    x = "Deaths per Milion",
    y = "Country",
    fill = "Country"
  ) +
  scale_fill_manual(values = c("Brazil" = "#4DAF4A", "Peru" = "#E41A1C")) +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"  
  )

