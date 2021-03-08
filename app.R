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
data1$Height_centered<-round(data1$Height_cm-mean(data1$Height_cm),2)
m1<-lm(Weight_kg~Height_centered,data1)
STUFF<-0
data1 %>%
  select(Height_cm,Height_centered,Weight_kg,Name,Position) ->data1

source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bayesian Linear Model"),

    # Sidebar with a slider input for number of bins

    mainPanel(
      tabsetPanel(
          tabPanel("Priors",selectInput("slopeprior","Prior for Slope - Average Weight Increase (KG) per 1cm Height",choices=c("uniform","normal")),
                          conditionalPanel("input.slopeprior == 'uniform'",
                                      numericInput("slope_min","Min",value=0.25),
                                      numericInput("slope_max","Max",value=1.25),
                                           ),
               conditionalPanel("input.slopeprior == 'normal'",
                                numericInput("slope_mean","Mean",value=0.5),
                                numericInput("slope_sd","SD",value=1),
               ),
               plotOutput("priorslope"),checkboxInput("prior_freq","Show Frequentist Estimate?",value = FALSE),
               selectInput("intprior","Prior for Intercept - Average Weight (KG) for Someone of Average Height",choices=c("uniform","normal")),
               conditionalPanel("input.intprior == 'uniform'",
                                numericInput("int_min","Min",value=60),
                                numericInput("int_max","Max",value=90),
               ),
               conditionalPanel("input.intprior == 'normal'",
                                numericInput("int_mean","Mean",value=0),
                                numericInput("int_sd","SD",value=100),
               ),
               plotOutput("priorint"),checkboxInput("prior_freq2","Show Frequentist Estimate?",value = FALSE),
               selectInput("sdprior","Prior for Standard Deviation",choices=c("uniform","normal")),
               conditionalPanel("input.sdprior == 'uniform'",
                                numericInput("sd_min","Min",value=0),
                                numericInput("sd_max","Max",value=10),
               ),
               conditionalPanel("input.sdprior == 'normal'",
                                numericInput("sd_mean","Mean",value=5),
                                numericInput("sd_sd","SD",value=2),
               ),
               plotOutput("priorsd"),checkboxInput("prior_freq3","Show Frequentist Estimate?",value = FALSE),
      ),
      tabPanel("Data",DT::dataTableOutput("data")),
      tabPanel("Frequentist Model",plotOutput("lmplot"),tableOutput("lm1"),tableOutput("lm2")),

    tabPanel("Iterate",numericInput("iters","Number of Iterations",value=10000),
             actionButton("Run","Run"),plotOutput("burnplot"),numericInput("burn","Set Burn In Range",value=500)),
    tabPanel("Posteriors",plotOutput("ModelPost"),tableOutput("stattable"),
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
      ggplot(data1,aes(y=Weight_kg,x=Height_centered))+
        geom_point()+
        geom_smooth(method="lm",se=FALSE)+
        xlab("Height (cm) (Centered)")+
        ylab("Weight (kg)")+
        annotate(geom="label",x=-5,y=90,label=paste0("Weight=",round(coef(m1)[1],2),"+",round(coef(m1)[2],2),"*Height"))+
        ggtitle("Weight vs Height for 2020-21 Aston Villa Squad")
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
     p1<- ggplot(pp1,aes(y=y,x=x))+
        geom_line()+
        xlab("Slope")+
        ylab("Likelihood")

      if(input$prior_freq==TRUE){
        p1<-p1+
        geom_vline(xintercept=coef(m1)[2],col="red")+
        annotate(geom="label",x=coef(m1)[2],y=max(pp1$y),label="frequentist\nestimate")
      }
      p1
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
     p1<- ggplot(pp1,aes(y=y,x=x))+
        geom_line()+
        xlab("Intercept")+
        ylab("Likelihood")

      if(input$prior_freq2==TRUE){
       p1<-p1+ geom_vline(xintercept=coef(m1)[1],col="red")+
        annotate(geom="label",x=coef(m1)[1],y=max(pp1$y),label="frequentist\nestimate")
      }
p1
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
     p1<- ggplot(pp1,aes(y=y,x=x))+
        geom_line()+
        xlab("Standard Deviation")+
        ylab("Likelihood")
     if(input$prior_freq3==TRUE){
       p1<-p1+
         geom_vline(xintercept=summary(m1)$sigma,col="red")+
         annotate(geom="label",x=summary(m1)$sigma,y=max(pp1$y),label="frequentist\nestimate")
     }
     p1
    })


    rv <- reactiveValues(chain=data.frame(Slope=-999,Intercept=-999))

    observeEvent(input$Run,{


     if(input$slopeprior=="normal"){
      prior1=list(dist=input$slopeprior,mean=as.numeric(input$slope_mean),sd=as.numeric(input$slope_sd))
      a1 = rnorm(n = 1,mean=as.numeric(input$slope_mean),sd=as.numeric(input$slope_sd))

    }

    if(input$slopeprior=="uniform"){
      prior1=list(dist=input$slopeprior,min=as.numeric(input$slope_min),max=as.numeric(input$slope_max))
      a1 = runif(n = 1, min=as.numeric(input$slope_min),max=as.numeric(input$slope_max))

    }
    print(input$slopeprior)
    print(prior1)
    if(input$intprior=="normal"){
      prior2=list(dist=input$intprior,mean=as.numeric(input$int_mean),sd=as.numeric(input$int_sd))
      b1 = rnorm(n = 1, mean=as.numeric(input$int_mean),sd=as.numeric(input$int_sd))

    }
    if(input$intprior=="uniform"){
      prior2=list(dist=input$intprior,min=as.numeric(input$int_min),max=as.numeric(input$int_max))
      b1 = runif(n = 1, min=as.numeric(input$int_min),max=as.numeric(input$int_max))

    }

    if(input$sdprior=="normal"){
      prior3=list(dist=input$sdprior,mean=as.numeric(input$sd_mean),sd=as.numeric(input$sd_sd))
      c1 = rnorm(n = 1, mean=as.numeric(input$sd_mean),sd=as.numeric(input$sd_sd))

    }
    if(input$sdprior=="uniform"){
      prior3=list(dist=input$sdprior,min=as.numeric(input$sd_min),max=as.numeric(input$sd_max))
      c1 = runif(n = 1, min=as.numeric(input$sd_min),max=as.numeric(input$sd_max))

    }

    startvalue = c(a1,b1,c1)




    rv$chain = data.frame(run_metropolis_MCMC(startvalue, as.numeric(input$iters),
                                              x=data1$Height_centered,y=data1$Weight_kg,prior1=prior1,prior2=prior2,prior3=prior3))

    colnames( rv$chain)=c("Slope","Intercept","SD")
    rv$chain$iter<-1:nrow(rv$chain)


       })



  output$burnplot <- renderPlot({
    if(rv$chain$Slope[1]!=-999){
    rv$chain %>%
        gather("var","value",-iter) %>%
      ggplot(aes(y=value,x=iter))+
        geom_path()+
        xlab("Iteration")+
        ylab("Value")+
        facet_wrap(~var,scales="free",nrow=2)+
        geom_vline(xintercept=as.numeric(input$burn),col="red") ->p1
      print(p1)
    }
    })




  output$stattable<-renderTable({
    if(rv$chain$Slope[1]!=-999){
    rv$chain %>%
      filter(iter>as.numeric(input$burn))->d1

data.frame("Parameter"=c("Intercept","Slope","SD"),
           "Frequentist Estimate"=c(coef(m1),summary(m1)$sigma),
           "95% Confidence Interval"=c(paste(round(confint(m1),2)[1,],collapse=","),paste(round(confint(m1),2)[2,],collapse=","),NA),
           "Bayesian Estimate"=c(round(median(d1$Intercept),2),round(median(d1$Slope),2),round(median(d1$SD),2)),
           "95% Prediction Interval"=c(paste(round(quantile(d1$Intercept,c(0.025,0.975)),2),collapse=", "),
                                       paste(round(quantile(d1$Slope,c(0.025,0.975)),2),collapse=", "),
                                       paste(round(quantile(d1$SD,c(0.025,0.975)),2),collapse=", ")),check.names = FALSE)
}

  })


    output$SlopePost<-renderPlot({
      if(rv$chain$Slope[1]!=-999){
      priortext1<-ifelse(input$slopeprior=="uniform",paste0("Prior Slope = Uniform[",input$slope_min,",",input$slope_max,"]"),
                         paste0("Prior Slope = Normal[",input$slope_mean,",",input$slope_sd,"]"))

      rv$chain %>%
        filter(iter>as.numeric(input$burn)) ->d1
  ggplot(d1,aes(x=Slope))+
      geom_histogram()+
      geom_vline(xintercept=median(d1$Slope),col="red")+
      geom_vline(xintercept=coef(m1)[2],col="blue")+
      annotate(x=median(d1$Slope),y=as.numeric(input$iters)/20,geom="label",label="Bayesian\nEstimate",col="red")+
      annotate(x=coef(m1)[2],y=0,geom="label",label="Frequentist\nEstimate",col="blue")+
      ggtitle("Slope: Posterior Distribution",subtitle=priortext1)  ->p1
  print(p1)

}
    })

    output$InterceptPost<-renderPlot({
      if(rv$chain$Slope[1]!=-999){
      rv$chain %>%
        filter(iter>as.numeric(input$burn))->d1


      priortext2<-ifelse(input$intprior=="uniform",paste0("Prior Intercept = Uniform[",input$int_min,",",input$int_max,"]"),
                         paste0("Prior Intercept = Normal[",input$int_mean,",",input$int_sd,"]")   )


      ggplot(d1,aes(x=Intercept))+
        geom_histogram()+
        geom_vline(xintercept=median(d1$Intercept),col="red")+
        geom_vline(xintercept=coef(m1)[1],col="blue")+
        annotate(x=median(d1$Intercept),y=as.numeric(input$iters)/20,geom="label",label="Bayesian\nEstimate",col="red")+
        annotate(x=coef(m1)[1],y=0,geom="label",label="Frequentist\nEstimate",col="blue")+
        ggtitle("Intercept: Posterior Distribution",subtitle=priortext2)  ->p1
      print(p1)
      }
    })


    output$SDPost<-renderPlot({
      if(rv$chain$Slope[1]!=-999){

      priortext3<-ifelse(input$sdprior=="uniform",paste0("Prior SD = Uniform[",input$sd_min,",",input$sd_max,"]"),
                         paste0("Prior SD = Normal[",input$sd_mean,",",input$sd_sd,"]") )

      rv$chain %>%
        filter(iter>as.numeric(input$burn))->d1
    ggplot(d1,aes(x=SD))+
      geom_histogram()+
      geom_vline(xintercept=median(d1$SD),col="red")+
      geom_vline(xintercept=summary(m1)$sigma,col="blue")+
      annotate(x=median(d1$SD),y=as.numeric(input$iters)/20,geom="label",label="Bayesian\nEstimate",col="red")+
      annotate(x=summary(m1)$sigma,y=0,geom="label",label="Frequentist\nEstimate",col="blue")+
      ggtitle("SD: Posterior Distribution",subtitle=priortext3)  ->p1
    print(p1)
}

    })

    output$ModelPost<-renderPlot({
      if(rv$chain$Slope[1]!=-999){
      rv$chain %>%
        filter(iter>as.numeric(input$burn))->d1

      priortext1<-ifelse(input$slopeprior=="uniform",paste0("Prior Slope = Uniform[",input$slope_min,",",input$slope_max,"]"),
                         paste0("Prior Slope = Normal[",input$slope_mean,",",input$slope_sd,"]")                       )
      priortext2<-ifelse(input$intprior=="uniform",paste0("Prior Intercept = Uniform[",input$int_min,",",input$int_max,"]"),
                         paste0("Prior Intercept = Normal[",input$int_mean,",",input$int_sd,"]")   )
  priortext3<-ifelse(input$sdprior=="uniform",paste0("Prior SD = Uniform[",input$sd_min,",",input$sd_max,"]"),
                                            paste0("Prior SD = Normal[",input$sd_mean,",",input$sd_sd,"]") )

    ggplot(data1,aes(y=Weight_kg,x=Height_centered))+
      geom_point()+
      geom_smooth(method="lm",se=FALSE)+
      geom_abline(slope = mean(d1$Slope)
                  ,intercept=mean(d1$Intercept),col="red")+
      xlab("Height (cm) (Centered)")+
      ylab("Weight (kg)")+
      annotate(geom="label",x=-5,y=90,label=paste0("Frequentist: Weight=",round(coef(m1)[1],2),"+",round(coef(m1)[2],2),"*Height"),col="blue")+
      annotate(geom="label",x=-5,y=85,label=paste0("Bayesian: Weight=",round(median(d1$Intercept),2),"+",round(median(d1$Slope),2),"*Height"),col="red")+
      ggtitle("Weight vs Height for 2020-21 Aston Villa Squad",
              subtitle=paste0(priortext1,"\n",priortext2,"\n",priortext3)) ->p1
    print(p1)
      }
    })


}

# Run the application
shinyApp(ui = ui, server = server)
