
library(shiny)
library(BH)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
library(DT)

shinyUI(
        navbarPage("Pricing Overview", 
                   tabPanel("Explore the Data",
                            sidebarPanel(
                                    sliderInput("weeks", 
                                                "Weeks No.:", 
                                                min = 0,
                                                max = 70,
                                                value = c(0, 70)),
                                    sliderInput("price", 
                                                "Price Point:",
                                                min = 0,
                                                max = 160,
                                                value = c(0, 160) 
                                    ),
                                    
                                    uiOutput("brandsControl"),
                                    actionButton(inputId = "clearAll", 
                                                 label = "Clear Selected", 
                                                 icon = icon("square-o")),
                                    actionButton(inputId = "selectAll", 
                                                 label = "Select all", 
                                                 icon = icon("check-square-o"))
                                    
                            ),
                            mainPanel(
                                    tabsetPanel(
                                         
                                            tabPanel(p(icon("table"), "Detail Information"),
                                                     dataTableOutput(outputId="dTable")
                                            )
                                          
                                    )
                                    
                            )     
                   ), 
                   
                   tabPanel("About Dataset",
                            mainPanel(
                                    includeMarkdown("about.md")
                            )
                   ) 
        )  
)