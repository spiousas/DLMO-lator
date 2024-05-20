pacman::p_load(here, tidyverse, writexl, hms, envalysis, ggsci, lubridate)

data <- readxl::read_xlsx(here("data/Resultados para analisis  ALL.xlsx"))

multiplesheets <- function(fname) { 
  
  # getting info about all excel sheets 
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  
  data <- data_frame[[1]] |>
    mutate(Placa = sheets[1])
  
  for (i in 2:length(sheets)) {
    data <- data |>
      rbind(data_frame[[i]] |> mutate(Placa = sheets[i]))
  }
  
  data
}

data <- multiplesheets(here("data/Resultados para analisis  ALL.xlsx"))
data_prepro <- data |>
  drop_na(Time) |>
  mutate(Hour = as.numeric(as_hms(ymd_hms(Time)))/3600) |>
  mutate(`Concentration (pg/ml)` = as.numeric(`Concentration (pg/ml)`)) |>
  drop_na(`Concentration (pg/ml)`)

data_prepro |>
  filter(`Concentration (pg/ml)` < 100) |>
  ggplot(aes(x = Hour, y = `Concentration (pg/ml)`, color = Condition)) +
  facet_wrap(~ID, labeller = label_both, scales="free_y") +
  geom_point() +
  scale_color_npg() +
  geom_hline(yintercept = 3, color = "black", linetype = "dashed") +
  #scale_y_continuous(limits = c(0, 800)) +
  geom_smooth(se = F) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background =element_blank())

    
