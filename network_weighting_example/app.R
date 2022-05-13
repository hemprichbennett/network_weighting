# This is a very basic R shiny app that uses the functions that'll be used for analysis
# to show how different methods of network generation alter our measurements of ecological networks

library(shiny)
library(here)
library(bipartite)
library(dplyr)
library(tidyr)
source(here('scripts', 'datagen_functions.R'))

indices <- c('connectance', 'ISA', 'weighted NODF', 'H2')

# Define UI for application
ui <- fluidPage(
  
   # Application title
   titlePanel("Network weighting considerations"),
   
   # Sidebar with user inputs
   sidebarLayout(
      sidebarPanel(
         sliderInput("n_upper_i",
                     "Number of Upper individuals",
                     min = 10,
                     max = 40,
                     value = 10),
         
         sliderInput("n_upper_sp",
                     "Number of Upper species",
                     min = 3,
                     max = 8,
                     value = 4),
         
         sliderInput("n_lower_i",
                     "Number of Lower individuals",
                     min = 10,
                     max = 40,
                     value = 10),
         sliderInput("n_lower_sp",
                     "Number of Lower species",
                     min = 3,
                     max = 8,
                     value = 4)
      ),
      
      # Show the resulting network values
      mainPanel(
         dataTableOutput("out_table")
      )
   )
)

# Define server logic required to generate data and calculate summary values
server <- function(input, output) {
   
  
  
  
   output$out_table <- renderDataTable({
     
     field_dataset <- field_data_gen(n_upper_individuals = input$n_upper_i,
                                     n_upper_species = input$n_upper_sp,
                                     n_lower_individuals = input$n_lower_i,
                                     n_lower_species = input$n_lower_sp,
                                     prop_realised_interactions = 0.1)
     
     transformed <- data_transform(field_dataset)
     
     nets <- lapply(transformed[-1], tib_to_matrix)
     
     out_vals <- lapply(nets, function(x) bipartite::networklevel(x, index = indices)) %>%
       bind_rows(.id = 'normalisation_type')
     
    print(out_vals)
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

