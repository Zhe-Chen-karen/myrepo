#' @title function future_value
#' @description compute the future value of an investment
#' @param amount (numeric) rate (numeric) years (numeric)
#' @return the future value of an investment (numeric)
future_value=function(amount,rate,years){
  value=amount*(1+rate)^years
  return(value)
}

#' @title function annuity
#' @description compute the future value of annuity
#' @param contrib (numeric) rate (numeric) years (numeric)
#' @return the future value of annuity (numeric)
annuity=function(contrib,rate,years){
  value=contrib*(((1+rate)^years-1)/rate)
  return(value)
}

#' @title function growing_annuity
#' @description compute the future value of growing annuity
#' @param contrib (numeric) rate (numeric) growth (numeric) years (numeric)
#' @return the future value of growing annuity (numeric)
growing_annuity=function(contrib,rate,growth,years){
  value=contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
  return(value)
}


library(ggplot2)
library(shiny)

ui=fluidPage(
  titlePanel("saving-investing modalities"),
  
  fluidRow(
    column(4, div(class = "option-group",
                  sliderInput("amount", "Initial Amount",value=1000,min=0,max=100000, pre = "$", sep = ",",step=500))),
    
    column(4,div(class = "option-group",
                 sliderInput("return","Return Rate (in %)",
                             value=5,min=0,max=20,step=0.1))),
    
    column(4,div(class = "option-group",
                 sliderInput("years","Years",value=10,min=0,max=50,step=1)))
  ),
  
  fluidRow(
    column(4,div(class = "option-group",
                 sliderInput("contrib","Annual Contribution",
                             value=2000,min=0,max=50000,pre = "$",sep = ",",step=500))),
    
    column(4,div(class = "option-group",
                 sliderInput("growth","Growth Rate (in %)",    value=2,min=0,max=20,step=0.1))),
    
    column(4, selectInput("input_type", "Facet?",c("No","Yes")))
  ),
hr(),
  
  h3("Timelines"),
  plotOutput("plot"),
  h3("Balances"),
  verbatimTextOutput("balances")
)

server<-function(input,output){
  output$plot=renderPlot({
    year=c()
    no_contrib=c()
    fixed_contrib=c()
    growing_contrib=c()
    
    amount=input$amount
    n=input$years+1
    rate=input$return/100
    growth=input$growth/100
    contrib=input$contrib
    
    for(i in 1:n){
      year[i]=i-1
      no_contrib[i]=future_value(amount,rate,i-1)
      fixed_contrib[i]=future_value(amount,rate,i-1)+annuity(contrib,rate,i-1)
      growing_contrib[i]=future_value(amount,rate,i-1)+growing_annuity(contrib,rate,growth,i-1)
    }
    modalities=data.frame(year,no_contrib,fixed_contrib,growing_contrib)
    
    Year=rep(1:n,3)-1
    value=c(no_contrib,fixed_contrib,growing_contrib)
    type=gl(3,n,labels=c("no_contrib","fixed_contrib","growing_contrib"))
    mod=data.frame(Year,value,type)
    with(mod,levels(type))
    if(input$input_type=="No"){
      ggplot(mod)+
        geom_point(aes(Year,value,color=type),size=1.5)+
        geom_line(aes(Year,value,color=type),size=0.8)+
        labs(title="Three modes of investing",x="year",y="value")
    }else{
      ggplot(mod)+
        geom_point(aes(Year,value,color=type),size=1.5)+
        geom_line(aes(Year,value,color=type),size=0.8)+
        geom_area(aes(Year,value,fill=type),alpha=0.5)+
        facet_grid(~type)+
        labs(title="Three modes of investing",x="year",y="value")+
        theme_bw()
    }
  })
  
  output$balances=renderPrint({
    year=c()
    no_contrib=c()
    fixed_contrib=c()
    growing_contrib=c()
    
    amount=input$amount
    n=input$years+1
    rate=input$return/100
    growth=input$growth/100
    contrib=input$contrib
    
    for(i in 1:n){
      year[i]=i-1
      no_contrib[i]=future_value(amount,rate,i-1)
      fixed_contrib[i]=future_value(amount,rate,i-1)+annuity(contrib,rate,i-1)
      growing_contrib[i]=future_value(amount,rate,i-1)+growing_annuity(contrib,rate,growth,i-1)
    }
    modalities=data.frame(year,no_contrib,fixed_contrib,growing_contrib)
    modalities
  })
}

shinyApp(ui=ui,server=server)