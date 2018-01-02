#load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tm)
library(SnowballC)
library(wordcloud)
library("RColorBrewer")

# creating subset of just Piedmont butterflies
# setwd("C:/Users/keapa/Dropbox/My research/R Shiny/cloud.app")
mydat <- read.csv("legrand.kingsolver.mf.dat.12.12.17.csv")

mydat<-mydat[mydat$location == "mf",]
mydat<-mydat[mydat$number > 0,]
mydat$date.observed<-as.Date(mydat$date.observed)
mydat$julian<-yday(mydat$date.observed)

ui<-fluidPage(
  titlePanel("Butterfly species abundance by month"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      h4("How to use this wordcloud:"),
      p("Use this sidebar to select a butterfly family and month to display the most commonly observed and least commonly observed butterfly species at Mason Farm."),
      br(),
      selectInput("family", "Choose a butterfly family:", c("Nymphalidae", "Papilionidae","Pieridae","Lycaenidae","Hesperiidae")),
      sliderInput("month", "Choose a month (1-12):",min=1, max=12, value=6),
      hr(),
      h6("Powered by:"),
      tags$img(src="rlogo.png", height=50, width=50),
      br(),
      h6("Written by: Kate E. Augustine")
      # sliderInput("freq",
      #             "Minimum Frequency:", min=1, max=50, value=15),
      # sliderInput("max",
      #             "Maximum Number of Words:",
      #             min=1,max=100,value=100)
    ),
    
    # Show Word Cloud
    mainPanel(
      tabsetPanel(
        tabPanel("Common species wordcloud",  plotOutput("plot")),
        tabPanel("List of common species", dataTableOutput("commontable")),
        tabPanel("Uncommon species wordcloud",  plotOutput("plot2")),
        tabPanel("List of uncommon species", dataTableOutput("uncommontable")))
  )
)
)

server <- function(input, output) {
  
  dat<-reactive({
    mydat<-mydat[mydat$month == input$month,]
    mydat<-mydat[mydat$family == input$family,]
    
    mydat$comName2<-gsub("-", "", mydat$comName)
    mydat$comName3<-gsub("'", "", mydat$comName2)
    mydat$name<-gsub(" ", "", mydat$comName3)
    
    dat<-Corpus(VectorSource(mydat$name))
    
  })
  
  dat2<-reactive({
    mydat<-mydat[mydat$month == input$month,]
    mydat<-mydat[mydat$family == input$family,]
    x<-aggregate(number~comName+month, data=mydat, sum, na.rm=TRUE)
    x<-x[order(x$number),]
    x$inverse<-(203-x$number)
    
    comName<-rep(x$comName, x$inverse)
    month<-rep(x$month, x$inverse)
    
    x2<-data.frame(cbind(as.character(comName),month))
    colnames(x2)<-c("comName","month")
    
    #x2<-x2[x2$month == x2$month,]
    
    x2$comName2<-gsub("-", "", x2$comName)
    x2$comName3<-gsub("'", "", x2$comName2)
    x2$name<-gsub(" ", "", x2$comName3)
    
    dat2<-Corpus(VectorSource(x2$name))
  })
  
  commondat<-reactive({
    mydat<-mydat[mydat$month == input$month,]
    mydat<-mydat[mydat$family == input$family,]
    x<-aggregate(number~comName+month, data=mydat, sum, na.rm=TRUE)
    x<-x[x$month == input$month,]
    x<-x[order(-x$number),]
  })
  
  uncommondat<-reactive({
    mydat<-mydat[mydat$month == input$month,]
    mydat<-mydat[mydat$family == input$family,]
    x<-aggregate(number~comName+month, data=mydat, sum, na.rm=TRUE)
    x<-x[x$month == input$month,]
    x<-x[order(x$number),]
  })

  output$commontable <- renderDataTable({commondat()
  })

  output$uncommontable <- renderDataTable({uncommondat()
  })

  output$plot<-renderPlot({wordcloud(dat(), min.freq=1, max.words=20,
              colors=brewer.pal(8,"Dark2"), random.order=TRUE)
  })
  
  output$plot2<-renderPlot({wordcloud(dat2(), min.freq=1, max.words=20,
                                     colors=brewer.pal(8,"Dark2"), random.order=TRUE)
  })
}

#app call
shinyApp(ui, server)
