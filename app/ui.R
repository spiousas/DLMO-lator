#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, viridis, ggplot2, dplyr, readr, tidyr, shinyjs, patchwork, 
               ggsci, shinyWidgets, scales, here, writexl)
pacman::p_load_gh("emo")

useShinyjs()
useSweetAlert()

color_choices = list(
  list(
    'black',
    'white',
    'red',
    'blue',
    'forestgreen',
    '#666666',
    '#7f7f7f',
    "#7373FF", 
    "#FF7272"
  ),
  as.list(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")),
  as.list(brewer_pal(palette = "Blues")(9)),
  as.list(brewer_pal(palette = "Greens")(9)),
  as.list(brewer_pal(palette = "Spectral")(11)),
  as.list(brewer_pal(palette = "Dark2")(8))
)

shinyUI(fluidPage(
  #theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # Application title ####
  titlePanel(paste0("DLMO-lator v0.1 ", emo::ji("moon"))),
  
  # Sidebar ####
  sidebarLayout(
    sidebarPanel(
      fileInput("input_file",
                "Choose input data (.xlsx)",
                accept = c(".xlsx")),
      downloadButton("downloadData", "Download data")
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          # Raw data ####
          "RAW data",
          fluidPage(
            fluidRow(
              br(),
              "<- Please start the analysis by loading data on the lefside panel",
              br()
            ),
            fluidRow(
              plotOutput("rawplot", height = "auto"),
            ),
            fluidRow(
              dataTableOutput('table_data')
            )
          )
        )
      )
    )
  )
)
)
