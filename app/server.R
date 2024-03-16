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
  
})