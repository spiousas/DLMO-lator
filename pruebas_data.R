pacman::p_load(here, tidyverse, writexl, hms, envalysis, ggsci, lubridate, suncalc)

data <- readxl::read_xlsx(here("data/Resultados para analisis  ALL.xlsx"))

latitude <- -23.882442303596772
longitude <- -61.84619994923446

data_mod <- data |> 
  drop_na(Date) |>
  rowwise() |> 
  mutate(Time = as_hms(ymd_hms(Time)),
         Date = as.Date(Date),
         Datetime = as.POSIXct(paste(Date, Time), tz = "America/Argentina/Buenos_Aires"),
         Sunset = getSunlightTimes(Date,
                                   latitude,
                                   longitude, keep = "sunset", tz = "America/Argentina/Buenos_Aires")$sunset,
         relTime = difftime(Datetime, Sunset, units = "mins"),
         `Concentration (pg/ml)` = as.numeric(`Concentration (pg/ml)`))


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
data_mod <- data |> 
  drop_na(Date) |>
  rowwise() |> 
  mutate(Time = as_hms(ymd_hms(Time)),
         Date = as.Date(Date),
         Datetime = as.POSIXct(paste(Date, Time), tz = "America/Argentina/Buenos_Aires"),
         Sunset = getSunlightTimes(Date,
                                   latitude,
                                   longitude, keep = "sunset", tz = "America/Argentina/Buenos_Aires")$sunset,
         relTime = difftime(Datetime, Sunset, units = "mins"),
         `Concentration (pg/ml)` = as.numeric(`Concentration (pg/ml)`)) |>
  drop_na(`Concentration (pg/ml)`)

data_mod |>
  filter(`Concentration (pg/ml)` < 100) |>
  ggplot(aes(x = relTime, y = `Concentration (pg/ml)`, color = Condition)) +
  facet_wrap(~ID, labeller = label_both, scales="free_y") +
  geom_point() +
  scale_color_npg() +
  geom_hline(yintercept = 3, color = "black", linetype = "dashed") +
  #scale_y_continuous(limits = c(0, 800)) +
  geom_smooth(se = F) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background =element_blank())

var_x <- list("relTime", # Variable
              list(labs(x = "Time relative to sunset (mins)"), 
                   scale_x_continuous(breaks = seq(-540, 540, 60)))) # ggplot components

data_mod |>
  filter(`Concentration (pg/ml)` < 1000) |>
  ggplot(aes(x = .data[[ var_x[[1]] ]], y = `Concentration (pg/ml)`, color = Condition)) +
  facet_wrap(~ID, labeller = label_both, scales="free_y") +
  scale_color_npg() +
  #geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  #geom_hline(yintercept = 3, color = "black", linetype = "dashed") +
  geom_point() +
  geom_smooth(se = F, method = loess) +
  var_x[[2]] +
  #labs(x = "Time relative to sunset (mins)") +
  #scale_x_continuous(breaks = seq(-540, 540, 60)) +
  theme_bw() +
  theme(legend.position = "top",
        strip.background =element_blank())

