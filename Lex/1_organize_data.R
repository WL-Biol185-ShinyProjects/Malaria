
library(tidyverse)

raw <- read_csv("Lex/incidence-of-malaria.csv",
                show_col_types = FALSE)

glimpse(raw)       # column names, types, first values
summary(raw)       # ranges, NA counts

malaria <- raw %>%
  transmute(
    country   = Entity,
    code      = Code,
    year      = Year,
    incidence = raw[[4]] #the 4th column name
  ) %>%
  filter(!str_starts(code, "OWID"))  # remove World and income-group rows

n_distinct(malaria$country)   # how many countries?
range(malaria$year)           # which years?
sum(is.na(malaria$incidence)) # any missing values?

# Which countries have gaps (fewer years than the maximum)?
malaria %>%
  count(country) %>%
  filter(n < max(n))

dir.create("data", showWarnings = FALSE)
write_csv(malaria, "data/malaria_clean.csv")
