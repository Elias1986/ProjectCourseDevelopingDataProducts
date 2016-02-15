library(shiny)

# Load data processing file
source("data_processing.R")
brands <- sort(unique(data$brand))

# Shiny server
shinyServer(
        function(input, output) {
                
                # Initialize reactive values
                values <- reactiveValues()
                values$brands <- brands
                
                # Create event type checkbox
                output$brandsControl <- renderUI({
                        checkboxGroupInput('brands', 'Brands List:', 
                                           brands, selected = values$brands)
                })
                
                # Add observer on select-all button
                observe({
                        if(input$selectAll == 0) return()
                        values$brands <- brands
                })
                
                # Add observer on clear-all button
                observe({
                        if(input$clearAll == 0) return()
                        values$brands <- c() # empty list
                })
                
                # Prepare dataset
                dataTable <- reactive({
                        groupBybrand(data, input$weeks[1], 
                                     input$weeks[2], input$price[1],
                                     input$price[2], input$brands)
                })
                
                dataTableByclientweek <- reactive({
                        groupByweekclient(data, input$weeks[1], 
                                       input$weeks[2], input$price[1],
                                       input$price[2], input$brands)
                })
                
                dataTableByweek <- reactive({
                        groupByweekAgg(data, input$weeks[1], 
                                       input$weeks[2], input$price[1],
                                       input$price[2], input$brands)
                })
                
                dataTableByprice <- reactive({
                        groupByweekAll(data, input$weeks[1], 
                                       input$weeks[2], input$price[1],
                                       input$psrice[2], input$brands)
                })
                
                dataTableBypriceAvg <- reactive({
                        groupBypriceAvg(data, input$weeks[1], 
                                        input$weeks[2], input$price[1],
                                        input$price[2], input$brands)
                })
                
                dataTableBypricebrandAvg <- reactive({
                        groupBypricebrandAvg(data, input$weeks[1], 
                                             input$weeks[2], input$price[1],
                                             input$price[2], input$brands)
                })
                
                # Render data table
                output$dTable <- renderDataTable({
                        dataTable()
                } #, options = list(bFilter = FALSE, iDisplayLength = 50)
                )
                
                output$clientByweek <- renderChart({
                        plotclientCountByweek(dataTableByclientweek())
                })
                
                output$brandsByweek <- renderChart({
                        plotbrandsCountByweek(dataTableByweek())
                })
                
                output$priceByweek <- renderChart({
                        plotpriceByYear(dataTableByprice())
                })
                
                output$priceByweekAvg <- renderChart({
                        plotpriceByweekAvg(dataTableBypriceAvg())
                })
                
                output$priceBybrandAvg <- renderChart({
                        plotpriceBybrandAvg(dataTableBypricebrandAvg())
                })
                
        }
)
