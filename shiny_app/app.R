library(shiny)
library(plotly)
library(tidyverse)
library(readr)
library(lubridate)


path_gen <-'T2_cars_bund_general4.csv'
path_all <- 'T2_cars_all.csv'

PAGE_TITLE <- "My great title"

T2_gen <- read_csv(path_gen)
T2_gen_affected <- T2_gen%>%
    filter(affected=="affected")

T2_all <- read_csv(path_all)

plotModal <- function(session) {
    modalDialog(
        textOutput("modalText"),
        ggiraphOutput("modalplot")
    )
}

ui <-

    shiny::navbarPage(
        theme = "style.css",
        #the line of code places the logo on the left hand side before the tabs start. See image below.
        title = div(img(src='logo.png',style="margin-top: -14px; padding-right:15px;padding-bottom:15px", height = 60)),
        #theme = "journal",
        windowTitle="Logan Together: Service Map",
                   tabPanel("Overview",
                            p("Map with a general OVerview of the affected Vehicel. You can check if your car is affected under the 'check my car' tab"),
                            fluidPage(
                                mainPanel(
                                    verbatimTextOutput("click"),
                                    plotlyOutput(outputId = "genPlot", width="auto")
                                )
                            )),
                    tabPanel("check my car",
                            fluidPage(
                                mainPanel(
                                    textInput("car_id", "Enter Car Id here", "-"),
                                    actionButton("check","Check Car ID"),
                                    tableOutput('tab_out'),
                                    downloadButton("download","Download Data"))
                                     )
                            )
    )

server <- function(input, output) {
    
    my_car <- reactive({
        my_car<- T2_all %>% 
            filter(ID_Fahrzeug== input$car_id)%>%
            mutate(rep_date=as.character(rep_date))
    })
    
    observeEvent(input$check,{
        my_car<- T2_all %>% 
            filter(ID_Fahrzeug== input$car_id)%>%
            mutate(rep_date=as.character(rep_date))
        
        output$tab_out <- renderTable({
            if(is.null(my_car$ID_Fahrzeug)) return()
            my_car })
        
    })
    
    
    #Donwloads the Dataset for the searched car ID
    output$download <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep=",")
        },
        content = function(file) {
            write.csv(my_car(), file)
        }
    )
    
    #general PLot of the HEatmap, adjust size and positon at the en
    output$genPlot <- renderPlotly(
        plot_ly(T2_gen_affected ,lat=~Breitengrad , lon = ~Laengengrad,
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
                 autosize=F, width=800, height=1000 ,
                paper_bgcolor='rgba(0,0,0,0)',
                plot_bgcolor='rgba(0,0,0,0)'
             )
    )
    
    
    #PLot function for the pop up plot also should filter for the right city 
    dataModal <- function(ed){
        modalDialog(
            renderPlot({
                ggplot(T2_gen%>%filter(Gemeinden==as.character(T2_gen_affected[ed$pointNumber+1,"Gemeinden"])), aes(x=Gemeinden, y=n, fill=affected)) +
                    geom_bar(stat="identity",position=position_dodge())+
                    ggtitle(T2_gen_affected[ed$pointNumber+1,"Gemeinden"])
            })
        )
    }
    
    #checks for click events and runs the plot function for the pop up plot
    observeEvent(event_data("plotly_click"), {
        event_data = event_data("plotly_click")
        showModal(dataModal(event_data))
    })
}

shinyApp(ui = ui, server = server)