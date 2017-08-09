---
title: "Exploring copulas"
author : Loïc BRIN
#bibliography: bibliography.bib
output:
  html_document:
    code_folding: show
    highlight: pygments #("tango", "pygments", "kate", "zenburn", "textmate")
    self_contained: true
    theme: cosmo #("cerulean", "journal", "flatly", "readable", "spacelab", "united", and "cosmo")
    toc: yes
    code_download: true
    toc_float: true
    css: style.css
runtime: shiny
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(shiny)
library(plotly)
library(mvtnorm)
library(MASS)
library(mvtnorm)
```

The purpose of this RMarkdown is to introduce the statistical tool known as **copulas**. Recalling Sklar's theorem, **Part I** shows (i) how to simulate copulas, (ii) compare their behaviour through graphical representations of simulated observations, and (iii) propose a complete multivariate model based on a Gaussian copula. **Part II** (i) introduces the concept of copula densities and (ii) compare the densities of different copulas. **Part III** uses the data generated in Part I, the density of a Gaussian copula exhibitied in Part II and apply a maximum likelihood algorithm to fit a copula on the simulated dataset to find back the parameters of the simulated copula in part I.

# Simulating copulas
## Simulation and Sklar's theorem
Sklar's theorem asserts that from any continuous multivariate distribution $G$, a copula can be deduced with the following formula:
$$\forall (u_1,...,u_d)\in[0;1]^d,\quad C(u_1,...,u_d)=G(F_1^{-1}(u_1),...,F_d^{-1}(u_d))$$
## Application and exploration of different copulas
Here, we will explore the case of Gaussian and Student copulas and will simulate them simulating Gaussian and Student multivariate variables, and then apply Sklar's theorem. 

### Gaussian copula
Let us consider a 2-dimensional Gaussian vector with mean $\boldsymbol \mu = \left(\begin{array}{c}0\\ 0\end{array}\right)$ and covariance matrix: $$\boldsymbol R=\begin{bmatrix} 1 & \rho \\ \rho & 1 \end{bmatrix} $$with $\rho=0,3$$.

In order to simulate $N$ observations of a two-dimensional Gaussian copula with the same parameters, we need to:


1. Simulate $N$  two-dimensional Gaussian vector $(X_1,X_2)$ with parameters $\boldsymbol \mu $ and $\boldsymbol R $;


2. Copose $X_1$ and $X_2$ by $\Phi(\cdot)$ is the univariate Gaussian reduced and centered distribution.

Below, we define `Gaussian_copula', a function with two parameters $N$ and $\rho$ to simulate $N$, two-dimensional, copulas with correlation $\rho$. 

```{r GaussianCopula}
Gaussian_copula<-function(N, rho)
{
  x<-mvrnorm(n = N, mu=rep(0,2), Sigma=matrix(rho,2,2)+diag(1-rho,2))
  apply(x,2,rank)/N
}
```

### Student copula

```{r StudentCopula}
Student_copula<-function(N, rho, nu)
{
  x<-rmvt(N, sigma = matrix(rho,2,2)+diag(1-rho,2), df = nu)
  apply(x,2,rank)/N
}

```


### Comparing copulas on simulations

```{r SimulationComparison, echo=FALSE, fig.width=16, fig.height=16}
# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(wellPanel(
    sliderInput("number",
                  "Number of simulations:",
                  min = 0,
                  max = 5000,
                  value = 2000),
      splitLayout(wellPanel(
            selectInput("choice1", "Copula #1", c("Gaussian copula"="gaussian","Student copula"="student")),  
      sliderInput("rho1",
                  "Choice for rho:",
                  min = 0,
                  max = 1,
                  value = 0.3),
      conditionalPanel("input.choice1=='student'",sliderInput("nu1",
                  "Degree of freedom:",
                  min = 1,
                  max = 30,
                  value = 2))),
      wellPanel(
      selectInput("choice2", "Copula #2", c("Gaussian copula"="gaussian","Student copula"="student"), selected="student"),
      sliderInput("rho2",
                  "Choice for rho:",
                  min = 0,
                  max = 1,
                  value = 0.3),
      conditionalPanel("input.choice2=='student'",sliderInput("nu2",
                  "Degree of freedom:",
                  min = 1,
                  max = 30,
                  value = 3))))),
      fluidRow(splitLayout(cellWidths = c("45%", "45%"),
      plotlyOutput("plot_1"),
      plotlyOutput("plot_2"))
    )
  )
)



server <- function(input, output) {
      output$plot_1 <- renderPlotly({
       if (input$choice1=="gaussian")
      {
        data1 <- data.frame(Gaussian_copula(N=input$number,rho = input$rho1))
      plot_1 <- plot_ly(data1, x = ~data1[,1], y = ~data1[,2], type = 'scatter')
      }
       else
       {
          data1 <- data.frame(Student_copula(N=input$number,rho = input$rho1, nu=input$nu1))
     plot_1 <- plot_ly(data1, x = ~data1[,1], y = ~data1[,2], type = 'scatter')
       }
})
      output$plot_2 <- renderPlotly({
       if (input$choice2=="gaussian")
      {
        data2 <- data.frame(Gaussian_copula(N=input$number,rho = input$rho2))
      plot_2 <- plot_ly(data2, x = ~data2[,1], y = ~data2[,2], type = 'scatter')
      }
       else
       {
          data2 <- data.frame(Student_copula(N=input$number,rho = input$rho2, nu=input$nu2))
     plot_2 <- plot_ly(data2, x = ~data2[,1], y = ~data2[,2], type = 'scatter')
       }
})
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1000))

```


## Beyond the dependence structure

# Copulas densities
```{r Gaussian_copula_density, echo=FALSE, fig.width=16, fig.height=16}
Gaussian_copula_density<-function(x, rho){
R<-matrix(rho,2,2)+diag(1-rho,2)
1/sqrt(det(R))*exp(1/2*t(qnorm(x))%*%(solve(R)-diag(1,2))%*%qnorm(x))
}
```

```{r Student_copula_density, echo=FALSE, fig.width=16, fig.height=16}
Student_copula_density<-function(x, rho, nu){
R<-matrix(rho,2,2)+diag(1-rho,2)
num<-lgamma((nu+2)/2)+lgamma(nu/2)*(1)+log(1+t(qt(x,nu))%*%solve(R)%*%qt(x,nu)/nu)*(-(nu+2)/2)
den<-0.5*log(det(R))+lgamma((nu+1)/2)*2+(sum(log(1+(qt(x,nu)^2/nu)))*(-(nu+1)/2))
exp(num-den)
}
```


```{r CopulaDensityPlot, echo=FALSE}
ui <- fluidPage(
  fluidRow(wellPanel(
    sliderInput("number",
                "Number of simulations:",
                min = 0,
                max = 5000,
                value = 2000),
    splitLayout(wellPanel(
      selectInput("choice1", "Copula #1", c("Gaussian copula"="gaussian","Student copula"="student")),  
      sliderInput("rho1",
                  "Choice for rho:",
                  min = 0,
                  max = 1,
                  value = 0.3),
      conditionalPanel("input.choice1=='student'",sliderInput("nu1",
                                                              "Degree of freedom:",
                                                              min = 1,
                                                              max = 30,
                                                              value = 2))),
      wellPanel(
        selectInput("choice2", "Copula #2", c("Gaussian copula"="gaussian","Student copula"="student"), selected="student"),
        sliderInput("rho2",
                    "Choice for rho:",
                    min = 0,
                    max = 1,
                    value = 0.3),
        conditionalPanel("input.choice2=='student'",sliderInput("nu2",
                                                                "Degree of freedom:",
                                                                min = 1,
                                                                max = 30,
                                                                value = 3))))),
    fluidRow(splitLayout(cellWidths = c("45%", "45%"),
                         plotlyOutput("plot_1"),
                         plotlyOutput("plot_2"))
    )
  )
)



server <- function(input, output) {
  output$plot_1 <- renderPlotly({
    if (input$choice1=="gaussian")
    {
      x<-y<-seq(0.01,0.99,0.01)
      z <- x %o% y
      for (i in 1:length(x))
      {
        for (j in 1:length(x))
        {
          z[i,j]<-Gaussian_copula_density(c(x[i],y[j]), rho=input$rho1)
        }
      }
      plot_1 <- plot_ly(x = x, y = y, z = z) %>% add_surface()
    }
    else
    {
      x<-y<-seq(0.01,0.99,0.01)
      z <- x %o% y
      for (i in 1:length(x))
      {
        for (j in 1:length(x))
        {
          z[i,j]<-Student_copula_density(c(x[i],y[j]), rho=input$rho1, nu=input$nu1)
        }
      }
      plot_1 <- plot_ly(x = x, y = y, z = z) %>% add_surface()
    }
  })
  output$plot_2 <- renderPlotly({
    if (input$choice2=="gaussian")
    {
      x<-y<-seq(0.01,0.99,0.01)
      z <- x %o% y
      for (i in 1:length(x))
      {
        for (j in 1:length(x))
        {
          z[i,j]<-Gaussian_copula_density(c(x[i],y[j]), rho=input$rho2)
        }
      }
      plot_2 <- plot_ly(x = x, y = y, z = z) %>% add_surface()
    }
    else
    {
      x<-y<-seq(0.01,0.99,0.01)
      z <- x %o% y
      for (i in 1:length(x))
      {
        for (j in 1:length(x))
        {
          z[i,j]<-Student_copula_density(c(x[i],y[j]), rho=input$rho2, nu=input$nu2)
        }
      }
      plot_2 <- plot_ly(x = x, y = y, z = z) %>% add_surface()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1000))


```

# Fitting copulas






