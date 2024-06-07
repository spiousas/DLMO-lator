#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic 
shinyServer(function(session, input, output) {
  
  # Setup ####
  # Disable action buttons
  #  shinyjs::disable('plot_raws')
  shinyjs::disable('run_prepro')
  shinyjs::disable('plot_prepro1')
  shinyjs::disable('plot_prepro2')
  shinyjs::disable('plot_periods')
  shinyjs::disable('plot_amps')
  shinyjs::disable('plot_acros')
  
  # Turn summarise notifications off
  options(dplyr.summarise.inform = FALSE)
  options(shiny.useragg = FALSE)
  
  # ggplot2 theme
  theme_set(
    theme_minimal(
      ## increase size of all text elements
      base_size = 16,
      ## set custom font family for all text elements
      base_family = "Archivo")
  )
  
  # Function to load xls files with multipĺe sheets
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
        rbind(data_frame[[i]] %>% mutate(Placa = sheets[i]))
    }
    
    data
  }

  data <- reactive({
    file <- input$input_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == c("xlsx"), "Please upload a xlsx file"))
    
    multiplesheets(file$datapath)
  })

  data_prepro <- reactive({
    data() |>
      drop_na(Date) |>
      rowwise() |> 
      mutate(Time = as_hms(ymd_hms(Time)),
             Date = as.Date(Date),
             Datetime = as.POSIXct(paste(Date, Time), tz = "America/Argentina/Buenos_Aires"),
             Sunset = getSunlightTimes(Date,
                                       input$latitude,
                                       input$longitude, 
                                       keep = "sunset", 
                                       tz = input$tz)$sunset,
             relTime = as.numeric(difftime(Datetime, Sunset, units = "mins")),
             `Concentration (pg/ml)` = as.numeric(`Concentration (pg/ml)`)) |>
      drop_na(`Concentration (pg/ml)`)
  })
  
  observe({
    updateSelectInput(
      session,
      "raw_id", "ID to plot:",
      choices = as.character(unique(data_prepro()$ID)),
      selected = as.character(unique(data_prepro()$ID))[1])
  })
  
  observe({
    updateSelectInput(
      session,
      "raw_condition", "Condition to plot:", 
      choices = as.character(unique(data_prepro()$Condition)),
      selected = as.character(unique(data_prepro()$Condition))[1])
  })
  
  observe({
    updateSelectInput(
      session,
      "filter_raw_data", "ID(s) to keep (only selected wells will be further processed):", 
      choices = as.character(unique(data_prepro()$ID)),
      selected = as.character(unique(data_prepro()$ID)))
  })
  
  output$table_data <- renderDataTable(
    data_prepro() |> 
      select(-Time) |>
      relocate(Hour, .after = `Curve Point`)
  )
  var_x <- reactive({
    if (input$relplot == "rel") {
      var_x <- list("relTime", # Variable
                    list(labs(x = "Time relative to sunset (mins)"),
                         scale_x_continuous(breaks = seq(-540, 540, 60)),
                         geom_vline(xintercept = 0, color = "black", linetype = "dashed"))) # ggplot components
    } else if(input$relplot == "clock") {
      var_x <- list("Time", # Variable
                    labs(x = "Time")) # ggplot components
    }
    })
  
  # Raw plot (all subjects) ####
  output$rawplot <- renderPlot({  
    data_prepro() |>
      filter(`Concentration (pg/ml)` < input$max_conc_plot) |>
      ggplot(aes(x = .data[[ var_x()[[1]] ]], y = `Concentration (pg/ml)`, color = Condition)) +
      facet_wrap(~ID, labeller = label_both, scales="free_y") +
      var_x()[[2]] +
      geom_point() +
      geom_smooth(se = F) +
      scale_color_npg() +
      theme_bw() +
      theme(legend.position = "top",
            strip.background =element_blank())},
    height = 600, 
    width = 800)
  
  # Raw plot (individual subjects) ####
  output$indiv_rawplot <- renderPlot({  
    data_prepro() |>
      filter(`Concentration (pg/ml)` < input$max_conc_plot) |>
      filter(ID == input$raw_id) |>
      ggplot(aes(x = .data[[ var_x()[[1]] ]], y = `Concentration (pg/ml)`, color = Condition)) +
      var_x()[[2]] +
      geom_point() +
      geom_smooth(se = F) +
      scale_color_npg() +
      theme_bw() +
      theme(legend.position = "top",
            strip.background =element_blank())},
    height = 300, 
    width = 400)
  
  # Raw plot (average) ####
  output$condition_rawplot <- renderPlot({  
    data_prepro() |>
      filter(`Concentration (pg/ml)` < input$max_conc_plot) |>
      filter(Condition == input$raw_condition) |>
      ggplot(aes(x = .data[[ var_x()[[1]] ]], y = `Concentration (pg/ml)`)) +
      var_x()[[2]] +
      geom_point(color = "gray80") +
      geom_smooth(aes(group = ID), se = F, color = "gray80") +
      geom_smooth(color = "black", fill = "darkorange", se = T, linewidth = 1) +
      theme_bw() +
      theme(legend.position = "bottom",
            strip.background =element_blank())},
    height = 300, 
    width = 400)
  
  data_filtered <- reactive({
    data_prepro() |>
      dplyr::filter(ID %in% input$filter_raw_data)
  })
  
  # Preprocessed plot plot (all subjects) ####
  output$preprocplot <- renderPlot({  
    data_filtered() |>
      filter(`Concentration (pg/ml)` < input$max_conc_plot) |>
      ggplot(aes(x = .data[[ var_x()[[1]] ]], y = `Concentration (pg/ml)`, color = Condition)) +
      facet_wrap(~ID, labeller = label_both, scales="free_y") +
      var_x()[[2]] +
      geom_point() +
      geom_smooth(se = F) +
      scale_color_npg() +
      theme_bw() +
      theme(legend.position = "top",
            strip.background =element_blank())},
    height = 600, 
    width = 800)
  
})