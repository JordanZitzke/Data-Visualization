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

América do Sul
data_SA <- dataset %>%
  filter(location %in% south_american_countries) %>%
  select(date, location, total_deaths, total_cases, population)

data_SA$date <- as.Date(data_SA$date)

#Último dia disponível para cada país
data_SA_latest <- data_SA %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  summarise(
    total_deaths = sum(total_deaths, na.rm = TRUE),
    total_cases = sum(total_cases, na.rm = TRUE),
    population = first(population)
  ) %>%
  ungroup()

# Top 5
top_5_countries <- data_SA_latest %>%
  top_n(5, total_deaths)

# Normalizar
data_SA_normalized <- top_5_countries %>%
  mutate(
    deaths_per_1000_cases = (total_deaths / total_cases) * 1000,
    cases_per_1000_population = (total_cases / population) * 1000
  )

data_SA_tidy <- data_SA_normalized %>%
  pivot_longer(
    cols = c(cases_per_1000_population, deaths_per_1000_cases),
    names_to = "metric",
    values_to = "value"
  )

data_SA_tidy$metric <- recode(data_SA_tidy$metric,
                              "cases_per_1000_population" = "Cases",
                              "deaths_per_1000_cases" = "Deaths"
)

custom_colors <- c(
  "Cases" = "#1F77B4",   # Azul para casos confirmados
  "Deaths" = "#E41A1C"    # Vermelho para mortes
)

ggplot(data_SA_tidy, aes(x = value, y = reorder(location, value), fill = metric)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Cases x Deaths",
    subtitle = "Top 5 Countries With the Most Deaths",
    x = "Amount (per 1000)",
    y = "Country"
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()  
  )
