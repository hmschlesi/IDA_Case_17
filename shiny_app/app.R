library(shiny)
library(plotly)
library(tidyverse)
library(readr)
library(lubridate)


path_gen <-'T2_cars_bund_general.csv'
path_all <- 'T2_cars_all.csv'

T2_gen <- read_csv(path_gen)
T2_all <- read_csv(path_all)


ui <-
    navbarPage("IDA Case Study 17",
        tabPanel("Overview",
            fluidPage(
                mainPanel(
                plotlyOutput(outputId = "genPlot")
                )
                )
        ),
        tabPanel("check my car",
                 fluidPage(
                     mainPanel(
                         textInput("car_id", "Enter Car Id here", "-"),
                         tableOutput('tab_out'),
                         downloadLink("downloadData", "Download")
                     )
                 )
                 )
                )


server <- function(input, output) {
    
    my_car <- reactive({
        T2_all %>% 
            filter(ID_Fahrzeug== input$car_id)%>%
            mutate(rep_date=as.character(rep_date))
        })

    
    output$genPlot <- renderPlotly(
            plot_ly(T2_gen,lat=~Breitengrad , lon = ~Laengengrad,
                    marker=list(color=~repair_days, size=~n*0.1,
                                colorscale='Cividis,', sizemode="area"),
                    text = ~paste(Gemeinden, paste("affected Vehicels:", n), paste("average duration for repair", repair_days), sep = "<br />"),
                    hoverinfo="text",
                    type='scattermapbox')%>%
            layout(
                mapbox=list(
                    style = 'carto-positron',
                    zoom =5.5,
                    center = list(lon = 10, lat = 51)),
                autosize=F, width=1000, height=1400)
    )
    output$tab_out <- renderTable({ T2_all%>%
                                      filter(ID_Fahrzeug== input$car_id)%>%
                                        mutate(rep_date=as.character(rep_date))})    
    output$info <- renderText(input$car_id)
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep=",")
        },
        content = function(file) {
            write.csv(my_car(), file)
        }
    )
}

shinyApp(ui = ui, server = server)
