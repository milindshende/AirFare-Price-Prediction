# Importing libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(xts)
library(moments)
library(forecast)
library(fpp)
library(smooth) 
library(tseries)
library(readxl)
library(MLmetrics) 
library(TSstudio)
library(TTR)
#library(rlang)
library(shiny)
library(tsbox)
library(lubridate)
library(tidyverse)
library(plotly)
library(shinythemes)

################### Domestic Dataset #######################
#Importing for Domestic dataset
airlines_dom <- read.csv("C:/Users/abc/Documents/FinalDomesticDataset.csv")
str(airlines_dom)
airlines_dom$InvoiceDate1 <- as.Date(airlines_dom$InvoiceDate1)
airlines_dom <-airlines_dom[,c(2,3)]
airlines_dom_ts <- ts(airlines_dom$M_NetFare,     # random data
                      start = min(airlines_dom$InvoiceDate1),end=max(airlines_dom$InvoiceDate1),frequency = 1)
# Domestic Model Building
(new_model_domestic<-arima(airlines_dom_ts,c(0,0,2),seasonal = list(order=c(0,0,2),period=90)))
new_model_domestic
Predict_dom<-predict(new_model_domestic,n.ahead = 90)
predict_date<-as.Date(as.numeric(time(Predict_dom$pred)))
predict_dom_airfare<-data.frame(predict_date,Predict_dom$pred)
colnames(predict_dom_airfare)<-c("DomDate","DomNetfare")
predict_dom_airfare$DomNetfare <- as.numeric(predict_dom_airfare$DomNetfare)
str(predict_dom_airfare)

################### International Dataset #######################
#Importing for International dataset
airlines_international <- read.csv("C:/Users/abc/Documents/FinalInternationalDataset.csv")
str(airlines_international)
airlines_international$InvoiceDate2 <- as.Date(airlines_international$InvoiceDate2)
airlines_international <-airlines_international[,c(2,3)]

airlines_inter_ts <- ts(airlines_international$M_NetFare2,     # random data
                      start = min(airlines_international$InvoiceDate2),end=max(airlines_international$InvoiceDate2),frequency = 1)

# International Model Building
(new_model_international<-arima(airlines_inter_ts,c(0,0,2),seasonal = list(order=c(0,0,2),period=90)))
new_model_international
Predict_inter<-predict(new_model_international,n.ahead = 90)
predict_date1<-as.Date(as.numeric(time(Predict_inter$pred)))
predict_inter_airfare<-data.frame(predict_date1,Predict_inter$pred)
colnames(predict_inter_airfare)<-c("InterDate","InterNetfare")
predict_inter_airfare$InterNetfare <- as.numeric(predict_inter_airfare$InterNetfare)
str(predict_inter_airfare)

###########################################################

ui<- fluidPage(
    theme = shinytheme("darkly"),
    titlePanel(title= h2(tags$b("Air Fare  Prediction"), align="center", style='background-color:#4682B4;',style='font-style: italic;')),
    br(),
    br(),
    sidebarLayout(sidebarPanel(  h4("Hello! Greetings from Team G5 ", style = "color: white;"),
                                 br(),
                                 br(),
                                 fluidRow( column( 12, dateInput(inputId = "date", label = "Please Select Date" ,value = "2019-06-11", min = "2019-06-11", max = "2019-09-08",
                                                                 width="150px", format = "yyyy-mm-dd"))),
                                 br(),
                                 fluidRow(column(12, radioButtons(inputId = "ItineraryType", label = "Please Select The Itinerary Type",
                                                                  choices =c("Domestic", "International"), selected = "Domestic", inline = TRUE))),
                                 br(),
                                 fluidRow( column(12,  submitButton(text="Submit",icon = NULL, width=NULL ))),
                                 style = "border: 1px solid purple"
    ),
    
    mainPanel( 
        br(),
        fluidRow(column(12,  verbatimTextOutput("dateoutput"))),
        fluidRow(column(12,  verbatimTextOutput("itineryoutput"))),
        fluidRow(column(12,  verbatimTextOutput("NetFareoutput"))),
        tags$style((HTML("#NetFare{font-size:20px;color:coral;border-radius:4px 4px 4px 4px;border:1px solid coral;}"))),
        br(),
        br(),
        br(),
        fluidRow(column(12,
                       tabsetPanel(type="tab",
                                  tabPanel(tags$b(tags$i("GRAPH"))  ,tags$i(plotlyOutput("Plot" ))),
                                  tabPanel(tags$b(tags$i("DETAILS")), tableOutput("Details") )
                                 ))),
        style= " border: 1px solid purple"
        )
    ), 
        style= " border: 2px solid red"
    )
####################################################

server<- function(input, output, session){    
    date_sel <- reactive({
        input$date })
    type_sel <- reactive({
        input$ItineraryType })
    
    output$dateoutput <- renderText({
        paste("You have selected Date : ", date_sel())
                })
    output$itineryoutput <- renderText({
        paste("You have selected Itinery Type : ",type_sel())   
                })
    model_building<- reactive({
        if (type_sel()=="Domestic"){
            dom_netfare <- predict_dom_airfare%>%
                filter(DomDate==date_sel())%>%
                summarise(DomNetfare=mean(DomNetfare))
                return(dom_netfare)
            }
        else {
            
            inter_netfare <- predict_inter_airfare%>%
               filter(InterDate==date_sel())%>%
              summarise(InterNetfare=mean(InterNetfare))
              return(inter_netfare)
            }
            })
    output$NetFareoutput <- renderText({
            paste("Predicted NetFare is : INR", round(model_building(),digits = 0))
            }) 
    output$Details <- renderTable({result()})
    
    result <- reactive({
            if (type_sel()=="Domestic"){
                df<- predict_dom_airfare%>%
                    filter(DomDate>=date_sel())
                df$DomDate<- format(df$DomDate,"%Y-%m-%d")
                colnames(df) <- c("Date","MeanNetFare")
                df[1:7,]
            }
            else {
                df1<- predict_inter_airfare%>%
                filter(InterDate>=date_sel())
                df1$InterDate<- format(df1$InterDate,"%Y-%m-%d")
                colnames(df1) <- c("Date","MeanNetFare")
                df1[1:7,]
            }
            })
   output$Plot <- renderPlotly({
       Result <- result()
       p <- ggplot(data=Result,aes(x=Date,y=MeanNetFare))+ geom_line(linetype="dashed",colour="blue")+ geom_point()+geom_path()+
       ylab("MeanNetFare")+ xlab('Date')+theme(axis.text.x = element_text(angle = 45,vjust = 0.5,hjust = 1))
       p
       })
}

# Run the application 
shinyApp(ui = ui, server = server)

