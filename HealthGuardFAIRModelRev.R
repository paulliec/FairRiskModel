#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(triangle)
library(ggplot2)
library(shiny)
library(pastecs)
library(gridExtra)

#Genericized panel for min/max/mode inputs. Inputs are prefixed with the type parameter.
layoutpanel <- function(type, title, defaultMin, defaultMax, defaultMode){
  tags$div(
    tags$h4(title),
    splitLayout(
      numericInput(paste(type,"_min", sep=""), "Minimum", defaultMin, min=1, max=NA),
      numericInput(paste(type,"_max", sep=""), "Maximum", defaultMax, min=1, max=NA)
    ),
    sliderInput(paste(type,"_mode", sep=""), "Most Likely", min=defaultMin, max=defaultMax, value=defaultMode),
    sliderInput("vuln",
                "Vulnerability Percentage",
                min = .01,
                max = 1,
                value = .02),
    sliderInput("SLEF",
              "Secondary Loss Event Frequency",
              min = .01,
              max = 1,
              value = .03)
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("HealthGuard FAIR model"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        layoutpanel("tef", "Threat Event Frequency", defaultMin=10, defaultMax=750, defaultMode=150),
        tags$hr(),
        layoutpanel("plm", "Primary Loss Magnitude", defaultMin=200, defaultMax= 100000, defaultMode=750),
       tags$hr(),
       layoutpanel("slm", "Secondary Loss Magnitude", defaultMin=100000, defaultMax= 15000000, defaultMode=250000),
       tags$hr()
       
       # sliderInput("vuln",
        #             "Vulnerability Percentage",
         #            min = .01,
          #           max = 1,
           #          value = .02)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = "distPlot3",width = "100%"),
        plotOutput("gmplot"),
        
        plotOutput(outputId = "distPlot",width = "50%"),
        plotOutput(outputId = "distPlot2",width = "50%"),
        tableOutput("Sum1Stat"),
         htmlOutput("text"),
        tableOutput("table")
      )
   )
)

# Extracted calculation to a new function. Any changes here will impact histogram and text output.
calculateValues <- function(input, output, session){
  TeF<-rtriangle(1000,a=input$tef_min,b=input$tef_max,c=input$tef_mode)
  VulnDist<-rbinom(1000,1,input$vuln)
  SLEV<-rbinom(1000,1,input$SLEF)
  PLEF<-TeF*VulnDist
  SLEF<-TeF*SLEV
  PrimLossExp<- rtriangle(PLEF, a=input$plm_min, b=input$plm_max,c=input$plm_mode)
  SecLossExp<-rtriangle(SLEF,a=input$slm_min,b=input$slm_max,c=input$slm_mode)
  TotalLossExp<-PrimLossExp+SecLossExp
  new_df<-data.frame(TeF,VulnDist,SLEV,PLEF,SLEF,PrimLossExp,SecLossExp,TotalLossExp)
 
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #Added observables for the mode selectors to stay up to date with min/max limits
  observe({
    val<- input$tef_min
    updateSliderInput(session, "tef_mode", min=val)
  })
  observe({
    val<- input$tef_max
    updateSliderInput(session, "tef_mode", max=val)
  })
  
  observe({
    val<- input$plm_min
    updateSliderInput(session, "plm_mode", min =val)
  })
  observe({
    val<- input$plm_max
    updateSliderInput(session, "plm_mode", max=val)
  })
  observe({
    val<- input$slm_min
    updateSliderInput(session, "slm_mode", min =val)
  })
  observe({
    val<- input$slm_max
    updateSliderInput(session, "slm_mode", max=val)
  })

  #observe({
   # new_df<-calculateValues(input)
  #  write.csv(new_df, file= "HGData.Frame.csv")
  #})
 # output$plotgraph({
  #  new_df <- calculateValues(input)
    #draw a histogram
    
   # grid.arrange(pl,sl,ncol=2)
  #})
  output$gmplot <-renderPlot({
  new_df<-calculateValues(input)
  pl<-ggplot(new_df,aes(x=PrimLossExp))+scale_x_continuous(name="Expected Primary Loss",labels = scales::dollar)+ylab("Frequency of Outcome")+ggtitle("Histogram of Expected Primary Loss") +geom_histogram(colour="white",fill="darkgreen")
  sl<-ggplot(new_df,aes(x=SecLossExp))+scale_x_continuous(name="Expected Secondary Loss",labels = scales::dollar)+ylab("Frequency of Outcome")+ggtitle("Histogram of Expected Secondary Loss") +geom_histogram(colour="white",fill="darkgreen")
  grid.arrange(pl,sl,ncol=2) 
  })
    
  # output$distPlot <- renderPlot({
   #  new_df <- calculateValues(input)
     #draw a histogram
    # ggplot(new_df,aes(x=PrimLossExp))+scale_x_continuous(name="Expected Primary Loss",labels = scales::dollar)+ylab("Frequency of Outcome")+ggtitle("Histogram of Expected Primary Loss") +geom_histogram(colour="white",fill="darkgreen")
   #})
   
   #output$distPlot2 <- renderPlot({
    # new_df <- calculateValues(input)
     #draw a histogram
    # ggplot(new_df,aes(x=SecLossExp))+scale_x_continuous(name="Expected Secondary Loss",labels = scales::dollar)+ylab("Frequency of Outcome")+ggtitle("Histogram of Expected Secondary Loss") +geom_histogram(colour="white",fill="darkgreen")
   #})
   output$distPlot3 <- renderPlot({
     new_df <- calculateValues(input)
     #draw a histogram
     ggplot(new_df,aes(x=TotalLossExp))+scale_x_continuous(name="Total Expected Loss",labels = scales::dollar)+ylab("Frequency of Outcome")+ggtitle("Histogram of Total Expected Loss") +geom_histogram(colour="white",fill="darkgreen")
   })
   output$text<-renderText({
     new_df <- calculateValues(input)
     x<-fivenum(round(new_df$TotalLossExp))
    labels<-c("Min","1stQ","Median","3rdQ","Max")
     y<-rbind(labels,x)
     
       HTML(paste(y))

   })
  # output$Sum1Stat<-renderTable({
     
   #  new_df<-calculateValues(input)
    # fivenum(round(new_df$PrimLossExp))
     
   #})
   
   output$table<-renderTable({
    new_df<-calculateValues(input)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

