library(tidyverse)
library(lubridate)
library(glue)
library(jsonlite)

df <- read_csv("https://www.bde.es/webbe/es/estadisticas/compartido/datos/csv/ti_1_7.csv", skip = 2) %>%
  select(`ALIAS DE LA SERIE`,TI_1_7.5, TI_1_7.6, TI_1_7.7) %>%
  rename("date" = "ALIAS DE LA SERIE",
         "euribor_3meses" = "TI_1_7.5",
         "euribor_6meses" = "TI_1_7.6",
         "euribor_1ano" = "TI_1_7.7") %>%
  mutate(euribor_3meses = as.numeric(euribor_3meses),
         euribor_6meses = as.numeric(euribor_6meses),
         euribor_1ano = as.numeric(euribor_1ano)) %>%
  filter(!is.na(euribor_3meses)) %>%
  mutate(date = gsub("DIC", "12", date)) %>%
  mutate(date = gsub("ENE", "01", date)) %>%
  mutate(date = gsub("FEB", "02", date)) %>%
  mutate(date = gsub("MAR", "03", date)) %>%
  mutate(date = gsub("ABR", "04", date)) %>%
  mutate(date = gsub("MAY", "05", date)) %>%
  mutate(date = gsub("JUN", "06", date)) %>%
  mutate(date = gsub("JUL", "07", date)) %>%
  mutate(date = gsub("AGO", "08", date)) %>%
  mutate(date = gsub("SEP", "09", date)) %>%
  mutate(date = gsub("OCT", "10", date)) %>%
  mutate(date = gsub("NOV", "11", date)) %>%
  mutate(date = dmy(date))

get_max_date <- df %>% filter(date == max(date)) %>% pull(date)

place_holder <- data.frame(
  date = Sys.Date()
)

df <- df %>% 
  bind_rows(place_holder) %>% 
  filter(date >= "2000-01-03")


euribor_month <- df %>%
  mutate(mes = month(date),
         ano = year(date)) %>%
  group_by(mes, ano) %>%
  summarise(euribor_3meses = mean(euribor_3meses, na.rm = T),
            euribor_6meses = mean(euribor_6meses, na.rm = T),
            euribor_1ano = mean(euribor_1ano, na.rm = T)) %>%
  mutate(date = glue("{ano} {mes}")) %>%
  mutate(date = ymd(date, truncated = 1)) %>%
  arrange(date) %>% 
  filter(!is.na(euribor_3meses))

df <- df %>% 
  filter(!is.na(euribor_3meses))



updated_pt_text <- glue('{day(Sys.time())} de {month(Sys.time(), label = TRUE, abbr = FALSE, locale="pt_PT")} de {year(Sys.time())}')

data <- list(
  updated_time = Sys.time(),
  updated_string = updated_pt_text,
  max_date = get_max_date,
  all_euribor = df,
  euribor_month = euribor_month
)

data <- data %>% toJSON(pretty = FALSE, auto_unbox = TRUE, na = "null")

data %>% write('data.json')



