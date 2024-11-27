library(readr)
library(dplyr)
library(ggplot2)
library(scales)

dataset <- read_csv("/home/jordan/covid.csv")

south_american_countries <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
  "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
  "Uruguay", "Venezuela"
)

# América do Sul
data_SA <- dataset %>%
  filter(location %in% south_american_countries) %>%
  select(date, location, total_deaths, population)


data_SA$date <- as.Date(data_SA$date)

data_SA <- data_SA %>%
  mutate(deaths_per_million = (total_deaths / population) * 1e6)

# Cores
custom_colors <- c(
  "Argentina" = "#999999",  
  "Bolivia"   = "#000000",  
  "Brazil"    = "#4DAF4A",  
  "Chile"     = "#984EA3",  
  "Colombia"  = "#FF7F00",  
  "Ecuador"   = "#FFFF33",  
  "Guyana"    = "#ffb600",  
  "Paraguay"  = "#F781BF",  
  "Peru"      = "#E41A1C",  
  "Suriname"  = "#870218",  
  "Uruguay"   = "#00D4FF",  
  "Venezuela" = "#8DA0CB"   
)

ggplot(data_SA, aes(x = date, y = deaths_per_million, color = location)) +
  geom_line(size = 1) +
  labs(
    title = "Deaths per Million Inhabitants",
    subtitle = "South America",
    x = "Year",
    y = "Deaths",
    color = "Country"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )
