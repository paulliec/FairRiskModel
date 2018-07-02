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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("HealthGuard FAIR model"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        #putting numeric user inputs no max
        numericInput("dmin","Minimum Threat Event Frequency", 10,min = 1,max=NA),
        
        numericInput("dmax","Minimum Threat Event Frequency", 750,min = 1,max=NA),
        verbatimTextOutput("value"),
       #commenting out slider inputs for now  
         #sliderInput("min",
                    # "Min Tef",
                    # min = 1,
                     #max = 1000,
                     #value = 10),
        # sliderInput("max",
                    #"Max TeF",
                     #min = 1,
                     #max = 10000,
                     #value = 750),
         sliderInput("mode",
                     "Most likely Threat Event Frequency",
                     min = 1,
                     max = 1000,
                     value = 150),
         sliderInput("PLMmin",
                     "min PLM",
                     min = 100,
                     max = 1000,
                     value = 200),
         sliderInput("PLMmax",
                     "max PLM",
                     min = 1000,
                     max= 500000,
                     value= 100000),
         sliderInput("PLMmode",
                     "most likely PLM",
                     min= 1000,
                     max= 500000,
                     value=750),
         sliderInput("vuln",
                     "Vulnerability Percentage",
                     min = .01,
                     max = 1,
                     value = .02)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        plotOutput("distPlot"),
        #plotOutput("Boxplot")
         htmlOutput("text")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #working on getting these to be reactive variables
 #TeF<-rtriangle(1000,a=input$min,b=input$max,c=input$mode)
  #VulnDist<-rbinom(1000,1,input$vuln) 
  #PLEF<-TeF*VulnDist
  #FreqAmts1<- reactive({rtriangle(PLEF, a=input$PLMmin, b=input$PLMmax,c=input$PLMmode)})
  #new_df<-data.frame(TeF(),VulnDist(),PLEF(),FreqAmts())
   output$distPlot <- renderPlot({
     #to do: make reactive, create a table,summary statistics, better histogram, secondary loss model
     TeF<-rtriangle(1000,a=input$dmin,b=input$dmax,c=input$mode)
     VulnDist<-rbinom(1000,1,input$vuln)
     PLEF<-TeF*VulnDist
     FreqAmts<- rtriangle(PLEF, a=input$PLMmin, b=input$PLMmax,c=input$PLMmode)
     new_df<-data.frame(TeF,VulnDist,PLEF,FreqAmts)
     #draw a histogram
     ggplot(new_df,aes(x=FreqAmts))+scale_x_continuous(name="Primary Loss Amounts",labels = scales::dollar)+ylab("Frequency of Outcome")+ggtitle("Histogram of Primary Loss Magnitude") +geom_histogram(colour="white",fill="darkgreen")
     #misc scratch items 
     #str<-paste(VulnDist)
      #HTML(paste(round(FreqAmts)))
     
    #hist(FreqAmts, color="darkgreen",bin = 5, xaxt="n")
    # axis(side=1, at=axTicks(1), 
     #     labels=formatC(axTicks(1), format="d", big.mark=','))
          
   })
   output$text<-renderText({
     TeF<-rtriangle(1000,a=input$dmin,b=input$dmax,c=input$mode)
     VulnDist<-rbinom(1000,1,input$vuln)
     PLEF<-TeF*VulnDist
     FreqAmts<- rtriangle(PLEF, a=input$PLMmin, b=input$PLMmax,c=input$PLMmode)
     new_df<-data.frame(TeF,VulnDist,PLEF,FreqAmts)
     x<-fivenum(round(new_df$FreqAmts))
     #this shows the five num but not the labels...make as a table then print?
       HTML(paste(x))
   #ggplot(new_df,aes(x=FreqAmts,y=PLEF))+geom_boxplot(color = "black")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

