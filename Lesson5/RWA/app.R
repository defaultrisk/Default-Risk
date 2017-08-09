#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(plotly)

ui <- fluidPage(
  fluidRow(
    splitLayout(wellPanel(
      selectInput("choice1", "RWA #1", c("Corporate Exposure"="corporate","SME"="SME", "Residential Mortgage"="mortgage", "Revolving Retail"="revolving")),  
      sliderInput("LGD1",
                  "Choice for LGD:",
                  min = 0,
                  max = 1,
                  value = 0.7),
      sliderInput("M1",
                                                              "Maturity:",
                                                              min = 1,
                                                              max = 30,
                                                              value = 15),
      conditionalPanel("input.choice1=='SME'",sliderInput("S1",
                                                              "Sales (in Million):",
                                                              min = 1,
                                                              max = 100,
                                                              value = 5))),
      
      wellPanel(
        selectInput("choice2", "RWA #2", c("Corporate Exposure"="corporate","SME"="SME", "Residential Mortgage"="mortgage", "Revolving Retail"="revolving"), selected="mortgage"),  
        sliderInput("LGD2",
                    "Choice for LGD:",
                    min = 0,
                    max = 1,
                    value = 0.7),
        sliderInput("M2",
                    "Maturity:",
                    min = 1,
                    max = 30,
                    value = 4),
      conditionalPanel("input.choice2=='SME'",sliderInput("S2",
                                                          "Sales (in Million):",
                                                          min = 1,
                                                          max = 100,
                                                          value = 5))))),
    fluidRow(splitLayout(
                         plotlyOutput("plot_1"),
                         plotlyOutput("plot_2"))
    ),
  withMathJax(),
  fluidRow(splitLayout(uiOutput("formula1"), uiOutput("formula2")))
  )

RWA<-function(PD,M,LGD,EAD,S,type)
{
  if (type=="corporate")
  {
    R=1.25*(0.12*((1-exp(-50*PD))/(1-exp(-50)))+0.24*(1-((1-exp(-50*PD))/(1-exp(-50)))))  
    b=(0.11852-0.05478*log(PD))^2
    K=LGD*(pnorm(sqrt(1/(1-R))*qnorm(PD)+sqrt(R/(1-R))*qnorm(0.999))-PD)*(1+(M-2.5)*b)/(1-1.25*b)
  }
  else if (type=="SME")
  {
    R=1.25*(0.12*((1-exp(-50*PD))/(1-exp(-50)))+0.24*(1-((1-exp(-50*PD))/(1-exp(-50)))))-0.004*(1-max(S-5,0)/45)  
    b=(0.11852-0.05478*log(PD))^2
    K=LGD*(pnorm(sqrt(1/(1-R))*qnorm(PD)+sqrt(R/(1-R))*qnorm(0.999))-PD)*(1+(M-2.5)*b)/(1-1.25*b)
  }
  else if (type=="mortgage")
  {  
    R=0.15
    K=LGD*(pnorm(sqrt(1/(1-R))*qnorm(PD)+sqrt(R/(1-R))*qnorm(0.999))-PD)
  }
  else if (type=="revolving")
  {
    R=0.04
    K=LGD*(pnorm(sqrt(1/(1-R))*qnorm(PD)+sqrt(R/(1-R))*qnorm(0.999))-PD)
  }
}

RWA_formula<-function(type)
{
  if (type=="corporate")
  {
  "$$ R=1.25 \\times 0.12*{\\frac {1-e^{-50*PD}}{1-e^{-50}}}+0.24*\\left(1-{\\frac {1-e^{-50*PD}}{1-e^{-50}}}\\right) $$ 
    $$ b=(0.11852-0.05478*\\ln(PD))^{2} $$ 
    $$ K=LGD*\\left[N\\left({\\sqrt {\\frac {1}{1-R}}}*G(PD)+{\\sqrt {\\frac {R}{1-R}}}*G(0.999)\\right)-PD\\right]*{\\frac {1+(M-2.5)b}{1-1.5b}} $$"
  }
  else if (type=="SME")
  {
    "$$ R=0.12*{\\frac {1-e^{-50*PD}}{1-e^{-50}}}+0.24*\\left(1-{\\frac {1-e^{-50*PD}}{1-e^{-50}}} \\right) $$ 
    $$ b=(0.11852-0.05478*\\ln(PD))^{2} $$ 
    $$ K=LGD*\\left[N\\left({\\sqrt {\\frac {1}{1-R}}}*G(PD)+{\\sqrt {\\frac {R}{1-R}}}*G(0.999)\\right)-PD\\right]*{\\frac {1+(M-2.5)b}{1-1.5b}} $$"
}
  else if (type=="mortgage")
  {  
"$$ R=0.15 $$ 
    $$ K=LGD*\\left[N\\left({\\sqrt {\\frac {1}{1-R}}}*G(PD)+{\\sqrt {\\frac {R}{1-R}}}*G(0.999)\\right)-PD\\right]$$"
    
  }
  else if (type=="revolving")
  {
"$$ R=0.04 $$ 
    $$ K=LGD*\\left[N\\left({\\sqrt {\\frac {1}{1-R}}}*G(PD)+{\\sqrt {\\frac {R}{1-R}}}*G(0.999)\\right)-PD\\right]$$"
  }
}



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot_1 <- renderPlotly({
    x<-seq(0.001,0.20,0.001)
    y<-RWA(PD=x,M=input$M1,LGD=input$LGD1,S=input$S1,type=input$choice1)
    data1 <- data.frame(x, y)
    plot_1 <- plot_ly(data1, x = ~x, y = ~y, type="scatter", mode = 'lines') %>%
      layout(
        xaxis = list(range = c(0, 0.2), title = "PD"),
        yaxis = list(range = c(0, 1.1), title = "Capital Requirement (in %)"))
  })
  output$plot_2 <- renderPlotly({
    x<-seq(0.001,0.2,0.001)
    y<-RWA(PD=x,M=input$M2,LGD=input$LGD2,S=input$S2,type=input$choice2)
    data2 <- data.frame(x, y)
    plot_2 <- plot_ly(data2, x = ~x, y = ~y, type="scatter", mode = 'lines') %>%
      layout(
        xaxis = list(range = c(0, 0.2), title = "PD"),
        yaxis = list(range = c(0, 1.1), title = "Capital Requirement (in %)"))
  })
  output$formula1 <- renderUI({
    return(withMathJax(RWA_formula(input$choice1)))
  })
    output$formula2 <- renderUI({
      return(withMathJax(RWA_formula(input$choice2), "</p>"))
})}


# Run the application 
shinyApp(ui = ui, server = server)

