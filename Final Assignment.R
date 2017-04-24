# 05-tabs.R
    
    library(shiny)
    library(plotly)
    library(dplyr)
    library(RColorBrewer)
    
    ui <- fluidPage(title = "Chicago City Crime",
            tags$table( tags$tr(tags$td(img(src= "CHICAGOPOLICE.jpg")),tags$td(h1("City of Chicago Crime Analysis"))),
                      tags$tr(tags$td( selectInput(inputId = "YearSel",
                                                   label = " Select the Time period for analysis",
                                                   choices = c("All years" = "0", "Last 5 years" ="1", "Last 10 years"="2", "Last 15 years"="3"),
                                                   multiple = F)))),
            submitButton( "Process"),
            
            tabsetPanel(              
                        tabPanel(title = "Top Ten Crime type",
                                 plotlyOutput("TopTenCrimesType")
                        ),
                        tabPanel(title = "Top Ten Crime locations",
                                 plotlyOutput("TopTenCrimesLocation")
                         )
             )
    )
    
    server <- function(input, output) {
       
        m <- list(
            l = 50,
            r = 50,
            b = 100,
            t = 100,
            pad = 4
        )
        
       Time_period <- reactive({
           input$YearSel
           }) 
       
        output$TopTenCrimesType <- renderPlotly({
           plot_data <- read.csv(".\\Top10Crimes.csv"
                                 ,stringsAsFactors = TRUE
                                 , header = TRUE)
           
           heading <- " City of Chicago Top 10 crimes by type in all years"
           if(Time_period() == "1") {
               plot_data <- filter(plot_data,Year >= max(Year) - 5 )
               heading <- " City of Chicago Top 10 crimes by type in last 5 years"
           }

           if(Time_period() == "2") {
               plot_data <- filter(plot_data,Year >= max(Year) - 10 )
               heading <- "City of Chicago Top 10 crimes by type in last 10 years"
           }

           if(Time_period() == "3"){
               plot_data <- filter(plot_data,Year >= max(Year) - 15 )
               heading <- " City of Chicago Top 10 crimes by type in last 15 years"
            }
           
       
                  # use plot to plot.
              plot_ly(data = plot_data,
                           x = ~Year,
                           y = ~count,
                           color = ~Primary.Type,
                           size = ~size,
                           colors = brewer.pal(10, "Paired"), 
                           type = 'scatter',
                           mode = 'markers',
                           width = 800,
                           height = 600,
                           marker = list( symbol = 'circle',
                                          sizemode = 'diameter',
                                          line = list( width = 2,
                                                       color = '#FFFFFF')),
                           text = ~paste('Year:', Year, '<br>Crime Type:', Primary.Type, '<br> Count:', count)
              )   %>%
                  layout( title =  heading 
                           , autosize = F
                            , margin = m
                            ,xaxis = list(title = 'Time Series in years',
                            range = c(min(plot_data$Year)-2, max(plot_data$Year)+2)
                          ),
                          yaxis = list(title = 'Crime count',
                                       range = c(min(plot_data$count), max(plot_data$count))
                          )
                  )
         
      })
       
      output$TopTenCrimesLocation <- renderPlotly({
        Crime_byLocation_data <-read.csv(".\\Crime_by_Location.csv"
                             ,stringsAsFactors = TRUE
                             , header = TRUE)  
        
        Crime_byLocation_data$size <- sqrt(Crime_byLocation_data$count * 2.666051223553066e-05 )
        
        m <- list(
            l = 50,
            r = 50,
            b = 100,
            t = 100,
            pad = 4
        )
        heading <- " City of Chicago Top 10 crimes by location in all years"
        if(Time_period() == "1") {
            Crime_byLocation_data <- filter(Crime_byLocation_data,Year >= max(Year) - 5 )
            heading <- " City of Chicago Top 10 crimes by location in last 5 years"
        }
        
        if(Time_period() == "2") {
            Crime_byLocation_data <- filter(Crime_byLocation_data,Year >= max(Year) - 10 )
            heading <- "City of Chicago Top 10 crimes by location in last 10 years"
        }
        
        if(Time_period() == "3"){
            Crime_byLocation_data <- filter(Crime_byLocation_data,Year >= max(Year) - 15 )
            heading <- " City of Chicago Top 10 crimes by location in last 15 years"
        }
        
            # use plot to plot.
        plot_ly(data = Crime_byLocation_data,
                x = ~Year,
                y = ~count,
                color = ~Location.Description,
                size = ~size,
                colors = brewer.pal(10, "Paired"), 
                type = 'scatter',
                mode = 'markers',
                width = 800,
                height = 600,
                marker = list( symbol = 'circle',
                               sizemode = 'diameter',
                               line = list( width = 2,
                                            color = '#FFFFFF')),
                text = ~paste('Year:', Year, '<br>Crime Type:', Location.Description, '<br> Count:', count)
        )   %>%
            layout( title = heading,autosize = F, margin = m, xaxis = list(title = 'Time Series in years',
                                range = c(min(Crime_byLocation_data$Year)-2, max(Crime_byLocation_data$Year)+2)
            ),
            yaxis = list(title = 'Crime count',
                         range = c(min(Crime_byLocation_data$count), max(Crime_byLocation_data$count))
            )
            )
      })
       
   
    }
    
    shinyApp(server = server, ui = ui)