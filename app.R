#
# play with dot plot 
# Yue Wang
# April,2018


library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
   
   titlePanel("Play with dot plot"),
   
   tags$h4("Dotplot 1", class = "subtitle"),
   fluidRow(
     column(4, 
            verticalLayout( 
              plotOutput(outputId = "dotPlot11", width = "300px", height = "100px"),
              sliderInput("binsize1",
                          "Bin size for chart1:",
                          min = 0,
                          max = 1,
                          value = 30),
              sliderInput("stackratio1",
                          "Stackratio for chart1:",
                          min = 0.5,
                          max = 1.8,
                          value = 30)
            )
     ),column(8,
              verticalLayout(
                plotOutput(outputId = "dotPlot12", width = "600px", height = "200px"),
                fluidRow(
                  column(6, sliderInput("binsize2",
                                        "Bin size for chart2:",
                                        min = 0,
                                        max = 1,
                                        value = 30)
                         ),
                  column(6, sliderInput("stackratio2",
                                        "Stackratio for chart1:",
                                        min = 0.5,
                                        max = 1.8,
                                        value = 30)
                         )
                )
              )
    )),
    fluidRow(
      column(12, 
             verticalLayout(
               plotOutput(outputId = "dotPlot13", width = "900px", height = "300px"),
               fluidRow(
                 column(6, sliderInput("binsize3",
                                       "Bin size for chart2:",
                                       min = 0,
                                       max = 1,
                                       value = 30)
                        ),
                 column(6, sliderInput("stackratio3",
                                       "Stackratio for chart1:",
                                       min = 0.5,
                                       max = 1.8,
                                       value = 30)
                        )
               )
             ) 
      )
    )  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   getData <- function(mu, sigma){
     samples <- 100
     mu <- log(11.4)
     sigma <- 0.2
     data <- qlnorm(ppoints(samples), mu, sigma)
     #ppoints -> vecter(0-n)
     tem <- data.frame( time = data ) 
     return(tem)
   }
   dotplot <- function(ratio, num) {
     data <- getData(11.4, 0.2)
     if(num == 1) g <- ggplot(data, aes(x=time)) + geom_dotplot(binwidth=input$binsize1, stackratio = input$stackratio1)+xlim(0,30)+theme(aspect.ratio = ratio)
     else if(num == 2) g <- ggplot(data, aes(x=time)) + geom_dotplot(binwidth=input$binsize2, stackratio = input$stackratio2)+xlim(0,30)+theme(aspect.ratio = ratio)
     else g <- ggplot(data, aes(x=time)) + geom_dotplot(binwidth=input$binsize3, stackratio = input$stackratio3)+xlim(0,30)+theme(aspect.ratio = ratio)
     g
   }
   output$dotPlot11 <- renderPlot(dotplot(1/3, 1))
   output$dotPlot12 <- renderPlot(dotplot(1/3, 2))
   output$dotPlot13 <- renderPlot(dotplot(1/3, 3))
}

# Run the application 
shinyApp(ui = ui, server = server)

