pacman::p_load(here, tidyverse, writexl)

data <- readxl::read_xlsx(here("data/data_Lau.xlsx"))

multiplesheets <- function(fname) { 
  
  # getting info about all excel sheets 
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  
  data <- data_frame[[1]] %>%
    mutate(Placa = sheets[1])
  
  for (i in 2:length(sheets)) {
    data <- data %>%
      rbind(data_frame[[i]] %>% mutate(Placa = sheets[i]))
  }
  
  data
}

data <- multiplesheets(here("data/data_Lau2.xlsx"))
data %>% 
  drop_na(`Concentration (pg/ml)`) %>%
  drop_na(Time) %>%
  #filter(Placa == "Placa 1 12-3-24") %>%
  mutate(`Concentration (pg/ml)` = as.numeric(`Concentration (pg/ml)`)) %>%
  ggplot(aes(x = Time, y = `Concentration (pg/ml)`, color = Condition)) +
  scale_y_continuous(limits = c(0, 200)) +
  facet_wrap(~ID) +
  geom_vline(xintercept = data$Time[23], color = "red") +
  geom_vline(xintercept = data$Time[76]) +
  geom_point() +
  geom_smooth(se = F) +
  theme_bw()
  
