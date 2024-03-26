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
      base_size = 14,
      ## set custom font family for all text elements
      base_family = "Arial")
  )
  
  # Function to load xls files with multipĺe sheets
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
  
  data <- reactive({
    multiplesheets(input$input_file$datapath)
  })
  
  output$table_data <- renderDataTable(
    data()
  )
  
  output$rawplot <- renderPlot({  
    data() %>%
      drop_na(Time) %>%
      drop_na(`Concentration (pg/ml)`) %>%
      #filter(Placa == "Placa 1 12-3-24") %>%
      mutate(`Concentration (pg/ml)` = as.numeric(`Concentration (pg/ml)`)) %>%
      ggplot(aes(x = Time, y = `Concentration (pg/ml)`, color = Condition)) +
      scale_y_continuous(limits = c(0, 1500)) +
      scale_x_time() +
      facet_wrap(~ID) +
      geom_point() +
      geom_line() +
      theme_bw()},
    height = 500, 
    width = 800)
  
})