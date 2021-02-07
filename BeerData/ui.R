library(shiny)
library(shinyWidgets)
library(plyr)
library(dplyr)
library(ggplot2)
library(usmap)
library(stringr)

# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Beers and Breweries in the USA"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            fileInput("beers", "Upload Beers Data", accept = ".csv"),
            fileInput("breweries", "Upload Breweries Data", accept = ".csv"),
            br(),
            
            # Input: Select the random distribution type ----
            radioButtons("dist", "Distribution Plot:",
                         c("Histogram" = "hist",
                           "Box Plot" = "box")),
            br(),
            
            pickerInput(
                inputId = "id", label = "State(s) :",
                "",
                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                               `count-selected-text` = "{0}/{1} States"),
                multiple = TRUE
            ),
            br(),
            
            checkboxInput("line",
                               label = "Add Linear Regression Line")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Data Table", 
                                 h4("Beers Data"),
                                 tableOutput("beers"), 
                                 br(),
                                 h4("Breweries Data"),
                                 tableOutput("breweries")),
                        tabPanel("ABV and IBU Distribution", 
                                 plotOutput("ABVhist"),
                                 plotOutput("IBUhist")),
                        tabPanel("ABV vs. IBU Scatter Plot", 
                                 plotOutput("scatter")),
                        tabPanel("Number of Breweries by State", 
                                 plotOutput("usmap"))
                        
            )
        )
    )
)
