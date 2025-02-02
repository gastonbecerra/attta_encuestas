library(tidyverse)
library(googlesheets4)


extraer_comun <- function(grupo) {
  if (length(grupo) == 1) return(grupo)
  palabras <- str_split(grupo, "\\s+")
  reduce(palabras, intersect) %>% paste(collapse = " ")
}

detect_coercion_type <- function(df) {
  df %>%
    summarise(across(where(is.character),
                     ~ case_when(
                       all(.x %in% c("0", "1")) ~ "logical",  # Ahora detecta correctamente booleanos
                       all(str_detect(.x, "^-?[0-9]+(\\.[0-9]+)?$")) ~ "numeric",
                       all(!is.na(lubridate::ymd(.x))) ~ "date",
                       TRUE ~ "character"
                     )))
}

coerce_variables <- function(df, types) {
  df %>%
    mutate(across(names(types)[types == "numeric"], as.numeric)) %>%
    mutate(across(names(types)[types == "logical"], ~ .x == "1")) %>%
    mutate(across(names(types)[types == "date"], lubridate::ymd))
}

replace_zeros_with_na <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ na_if(.x, "0")))
}


googlesheets4::gs4_deauth()
data <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1ZwlgHKESY_1731WIZ-BwoH8-N_F5Kv8rD-KXBLvDgiU/edit?gid=0#gid=0',
                                  sheet = 'transposed')

data2 <- data %>%
  mutate(across(where(is.list), ~ map_chr(.x, ~ ifelse(is.null(.x), NA, as.character(.x))))) %>%
  mutate(across(where(is.logical), ~ map_chr(.x, ~ ifelse(is.null(.x), NA, as.character(.x))))) %>%
  mutate(across(where(is.numeric), ~ map_chr(.x, ~ ifelse(is.null(.x), NA, as.character(.x))))) %>%
  mutate(id=row_number()) %>%
  pivot_longer(-id) %>%
  mutate( q = str_extract(name, "^[0-9]+"), name = str_remove(name, "^[0-9]+\\s*") ) %>%
  mutate(q=as.integer(q)) %>%
  select(id,q,name,value) %>%
  mutate(name = stringi::stri_trans_general(name, "Latin-ASCII")) %>%
  mutate( value2 = if_else(value == "1", name, value) )  %>%
  group_by(id,q) %>%
  mutate(comun = extraer_comun(name)) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  mutate(value3 = str_remove_all(value2, regex(comun)) %>% trimws(.) ) %>%
  group_by(id,q,comun) %>%
  summarize( value4 = paste(value3, collapse=' , ' ) ) %>%
  mutate( value5 = if_else(value4 == "", "1", value4) )  %>%
  select(-value4) %>%
  mutate(comun2 = paste(str_pad(q, width = 2, side = "left", pad = "0"), comun, sep = " ")) %>%
  pivot_wider(id_cols = id, names_from = comun2, values_from = value5, values_fill = "0") %>%
  select(sort(names(.))) %>% relocate(id) %>%
  ungroup()

types_detected <- detect_coercion_type(data2)
glimpse(types_detected)

data2 <- coerce_variables(data2, types_detected)
data2 <- data2 %>% mutate(across(where(is.character), ~ na_if(.x, "0")))

glimpse(data2)


data2$`57 Esta buscando fuente de ingresos` %>% table()
data2$`58 Busqueda laboral dificultada por identidad` %>% table()
