#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)
library(openxlsx)
library(broom)
library(tidyr)
data1<-read.xlsx("data.xlsx","data")
x<-data1$Height_cm
y<-data1$Weight_kg
m1<-lm(Weight_kg~Height_cm,data1)

source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bayesian Linear Model"),

    # Sidebar with a slider input for number of bins

    mainPanel(
      tabsetPanel(
      tabPanel("Data",DT::dataTableOutput("data")),
      tabPanel("Frequentist Model",plotOutput("lmplot"),tableOutput("lm1"),tableOutput("lm2")),
      tabPanel("Priors",selectInput("slopeprior","Prior for Slope",choices=c("uniform","normal")),
                          conditionalPanel("input.slopeprior == 'uniform'",
                                      numericInput("slope_min","Min",value=0.25),
                                      numericInput("slope_max","Max",value=1.25),
                                           ),
               conditionalPanel("input.slopeprior == 'normal'",
                                numericInput("slope_mean","Mean",value=0.5),
                                numericInput("slope_sd","SD",value=1),
               ),
               plotOutput("priorslope"),
               selectInput("intprior","Prior for Intercept",choices=c("uniform","normal")),
               conditionalPanel("input.intprior == 'uniform'",
                                numericInput("int_min","Min",value=-100),
                                numericInput("int_max","Max",value=0),
               ),
               conditionalPanel("input.intprior == 'normal'",
                                numericInput("int_mean","Mean",value=0),
                                numericInput("int_sd","SD",value=100),
               ),
               plotOutput("priorint"),
               selectInput("sdprior","Prior for Standard Deviation",choices=c("uniform","normal")),
               conditionalPanel("input.sdprior == 'uniform'",
                                numericInput("sd_min","Min",value=0),
                                numericInput("sd_max","Max",value=10),
               ),
               conditionalPanel("input.sdprior == 'normal'",
                                numericInput("sd_mean","Mean",value=5),
                                numericInput("sd_sd","SD",value=2),
               ),
               plotOutput("priorsd"),
      ),
    tabPanel("Iterate",numericInput("iters","Number of Iterations",value=10000),
             actionButton("Run","Run"),plotOutput("burnplot"),numericInput("burn","Set Burn In Range",value=5000)),
    tabPanel("Posteriors",plotOutput("ModelPost"),
             plotOutput("SlopePost"),plotOutput("InterceptPost"),plotOutput("SDPost"))

      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$data <- renderDataTable({
        data1
    })
    output$lmplot <- renderPlot({
      ggplot(data1,aes(y=Weight_kg,x=Height_cm))+
        geom_point()+
        geom_smooth(method="lm",se=FALSE)+
        xlab("Height (cm)")+
        ylab("Weight (kg)")
    })

    output$lm1<-renderTable({
            tidy(m1)
    })
    output$lm2<-renderTable({
      glance(m1)
    })

    output$priorslope<- renderPlot({

      if(input$slopeprior=="normal"){

        pp1<-data.frame(x=seq(as.numeric(input$slope_mean)-4*as.numeric(input$slope_sd),
                         as.numeric(input$slope_mean)+4*as.numeric(input$slope_sd),length.out = 300))
        pp1$y<-dnorm(x=pp1$x,mean = as.numeric(input$slope_mean),sd=as.numeric(input$slope_sd))

        }
      if(input$slopeprior=="uniform"){

        pp1<-data.frame(x=c(as.numeric(input$slope_min)-(as.numeric(input$slope_max)-as.numeric(input$slope_min))/300,
                            seq(as.numeric(input$slope_min),
                              as.numeric(input$slope_max),length.out = 300),
                            as.numeric(input$slope_max)+(as.numeric(input$slope_max)-as.numeric(input$slope_min))/300))
        pp1$y<-dunif(x=pp1$x,min = as.numeric(input$slope_min),max=as.numeric(input$slope_max))


      }
      ggplot(pp1,aes(y=y,x=x))+
        geom_line()+
        xlab("Slope")+
        ylab("Likelihood")+
        geom_vline(xintercept=coef(m1)[2],col="red")+
        annotate(geom="label",x=coef(m1)[2],y=max(pp1$y),label="frequentist\nestimate")
    })

    output$priorint<- renderPlot({

      if(input$intprior=="normal"){

        pp1<-data.frame(x=seq(as.numeric(input$int_mean)-4*as.numeric(input$int_sd),
                              as.numeric(input$int_mean)+4*as.numeric(input$int_sd),length.out = 300))
        pp1$y<-dnorm(x=pp1$x,mean = as.numeric(input$int_mean),sd=as.numeric(input$int_sd))

      }
      if(input$intprior=="uniform"){

        pp1<-data.frame(x=c(as.numeric(input$int_min)-(as.numeric(input$int_max)-as.numeric(input$int_min))/300,
                            seq(as.numeric(input$int_min),
                                as.numeric(input$int_max),length.out = 300),
                            as.numeric(input$int_max)+(as.numeric(input$int_max)-as.numeric(input$int_min))/300))
        pp1$y<-dunif(x=pp1$x,min = as.numeric(input$int_min),max=as.numeric(input$int_max))


      }
      ggplot(pp1,aes(y=y,x=x))+
        geom_line()+
        xlab("Intercept")+
        ylab("Likelihood")+
        geom_vline(xintercept=coef(m1)[1],col="red")+
        annotate(geom="label",x=coef(m1)[1],y=max(pp1$y),label="frequentist\nestimate")
    })

    output$priorsd<- renderPlot({

      if(input$sdprior=="normal"){

        pp1<-data.frame(x=seq(as.numeric(input$sd_mean)-4*as.numeric(input$sd_sd),
                              as.numeric(input$sd_mean)+4*as.numeric(input$sd_sd),length.out = 300))
        pp1$y<-dnorm(x=pp1$x,mean = as.numeric(input$sd_mean),sd=as.numeric(input$sd_sd))

      }
      if(input$sdprior=="uniform"){

        pp1<-data.frame(x=c(as.numeric(input$sd_min)-(as.numeric(input$sd_max)-as.numeric(input$sd_min))/300,
                            seq(as.numeric(input$sd_min),
                                as.numeric(input$sd_max),length.out = 300),
                            as.numeric(input$sd_max)+(as.numeric(input$sd_max)-as.numeric(input$sd_min))/300))
        pp1$y<-dunif(x=pp1$x,min = as.numeric(input$sd_min),max=as.numeric(input$sd_max))


      }
      ggplot(pp1,aes(y=y,x=x))+
        geom_line()+
        xlab("Standard Deviation")+
        ylab("Likelihood")+
        geom_vline(xintercept=summary(m1)$sigma,col="red")+
        annotate(geom="label",x=summary(m1)$sigma,y=max(pp1$y),label="frequentist\nestimate")
    })


    rv <- reactiveValues(chain=data.frame(Slope=1,Intercept=1))

    observeEvent(input$Run,{

    if(input$slopeprior=="normal"){
      prior1=list(dist=input$slopeprior,mean=as.numeric(input$slope_mean),sd=as.numeric(input$slope_sd))

    }

    if(input$slopeprior=="uniform"){
      prior1=list(dist=input$slopeprior,min=as.numeric(input$slope_min),max=as.numeric(input$slope_max))

    }
    print(input$slopeprior)
    print(prior1)
    if(input$intprior=="normal"){
      prior2=list(dist=input$intprior,mean=as.numeric(input$int_mean),sd=as.numeric(input$int_sd))

    }
    if(input$intprior=="uniform"){
      prior2=list(dist=input$intprior,min=as.numeric(input$int_min),max=as.numeric(input$int_max))

    }

    if(input$sdprior=="normal"){
      prior3=list(dist=input$sdprior,mean=as.numeric(input$sd_mean),sd=as.numeric(input$sd_sd))

    }
    if(input$sdprior=="uniform"){
      prior3=list(dist=input$sdprior,min=as.numeric(input$sd_min),max=as.numeric(input$sd_max))

    }

    startvalue = c(coef(m1)[2],coef(m1)[1],summary(m1)$sigma)


    print("go!")


    print(startvalue)
    print(as.numeric(input$iters))

    rv$chain = data.frame(run_metropolis_MCMC(startvalue, as.numeric(input$iters),
                                              x=data1$Height_cm,y=data1$Weight_kg,prior1=prior1,prior2=prior2,prior3=prior3))

    colnames( rv$chain)=c("Slope","Intercept","SD")
    rv$chain$iter<-1:nrow(rv$chain)


    print( rv$chain[1:10,])

    })



  output$burnplot <- renderPlot({
    rv$chain %>%
        gather("var","value",-iter) %>%
      ggplot(aes(y=value,x=iter))+
        geom_path()+
        xlab("Iteration")+
        ylab("Value")+
        facet_wrap(~var,scales="free",nrow=2)+
        geom_vline(xintercept=as.numeric(input$burn),col="red")
    })




    output$SlopePost<-renderPlot({
      rv$chain %>%
        filter(iter>as.numeric(input$burn)) ->d1
    ggplot(d1,aes(x=Slope))+
      geom_histogram()+
      geom_vline(xintercept=mean(d1$Slope),col="red")+
      geom_vline(xintercept=coef(m1)[2],col="blue")
    })

    output$InterceptPost<-renderPlot({
      rv$chain %>%
        filter(iter>as.numeric(input$burn))->d1
      ggplot(d1,aes(x=Intercept))+
        geom_histogram()+
        geom_vline(xintercept=mean(d1$Intercept),col="red")+
        geom_vline(xintercept=coef(m1)[1],col="blue")
    })


    output$SDPost<-renderPlot({
      rv$chain %>%
        filter(iter>as.numeric(input$burn))->d1
    ggplot(d1,aes(x=SD))+
      geom_histogram()+
      geom_vline(xintercept=mean(d1$SD),col="red")+
      geom_vline(xintercept=summary(m1)$sigma,col="blue")
    })

    output$ModelPost<-renderPlot({
      rv$chain %>%
        filter(iter>as.numeric(input$burn))->d1
    ggplot(data1,aes(y=Weight_kg,x=Height_cm))+
      geom_point()+
      geom_smooth(method="lm",se=FALSE)+
      geom_abline(slope = mean(d1$Slope)
                  ,intercept=mean(d1$Intercept),col="red")
    })


}

# Run the application
shinyApp(ui = ui, server = server)
