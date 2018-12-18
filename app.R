#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(base)
library(zoo)
library(plyr)
library(tidyverse)
library(readxl)
library(readr)
library(benford.analysis)
library(BenfordTests)
library(ggplot2)
library(plotrix)
library(plotly)




########################################################################################################
ui <- dashboardPage( 
  
  
  
  dashboardHeader(
                  title = "Stock Return and Company Value",
                  titleWidth = 400,
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  
                  
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Feedback",
                                 message = "Please give us your comment here"
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = Sys.time()
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = Sys.Date()
                               )
                  )
  ),
  skin = "yellow",
  
  
  
  
  
  
  
  
  
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hello!", tabName = "hello",icon = icon("coffee")),
      menuItem("S&P500 Index", tabName = "SP500INDEX", icon = icon("bar-chart-o")),
      menuItem("S&P500 Component stocks", tabName = "SP500COMPONENT", icon = icon("bar-chart-o"),
               menuSubItem("Component weight", tabName = "component_weight"),startExpanded = TRUE,
               menuSubItem("Component Stock Price", tabName = "component_price")),
      menuItem("Factors", tabName = "factors", icon = icon("th"),startExpanded = TRUE, 
               menuSubItem("Book Value per Share", tabName = "subitem1"),
               menuSubItem("Stock Return", tabName = "subitem2")

               ),
      menuItem("Benford Analysis", tabName = "BA",icon = icon("calendar")),
      menuItem("Source code", icon = icon("file-code-o"), 
               href = "https://github.com/YifuDong3/R-and-Data-visualization/tree/master/individual_stocks_5yr")
      
    ),
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    
    textOutput("res")
  ),
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  dashboardBody(
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML('
                              .skin-purple .main-header .logo {
                              background-color: #3c8dbc;
                              }
                              .skin-purple .main-header .logo:hover {
                              background-color: #3c8dbc;
                              }
                      .box.box-solid.box-primary{
                    border-bottom-color:#666666;
                              border-left-color:#666666;
                              border-right-color:#666666;
                              border-top-color:#666666;
                              }
                              '))),
    
    ###SPS500INDEX----------------------------------------------------------------------------------------
    tabItems(
      tabItem(
        tabName = "SP500INDEX",
        fluidRow(
          box(
            title = "Choose years",
            sliderInput("indexyear", "Year:",
                        min=as.Date("2008-12-15"),
                        max=as.Date("2018-10-31"),
                        value=as.Date("2015-01-01"),step = 30),                    
            solidHeader = T,
            idth = 8,
            width = 10
          ),
          box(
            title = "Choose an indicator",
            width = 5,
            solidHeader = T,
            status = "info",
            collapsible = T,
            collapsed = F,
            selectInput(
              "partone_indicator",
              "Choose an indicator",
              choice = c("Open","High","Low","Close","Volume"),
              selected = "Open"
            )
          ),
          box(
            title = "Add a line?",
            width = 5,
            solidHeader = T,
            status = "info",
            collapsible = T,
            collapsed = F,
            selectInput(
              "addline",
              "Want to add a line?",
              choice = c("TRUE","FALSE"),
              selected = "FALSE"
            )
          ),
          
          box(
            title = "S&P Index",
            width = 12,
            height = 500,
            solidHeader = T,
            status = "warning",
            collapsible = T,
            plotlyOutput("plota"))
          
        
          
          )
      ),
      ###hello ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "hello",
              fluidRow(
                box(
                  title = "To the Readers",
                  width = 12,
                  solidHeader = T,
                  status = "primary",
                  collapsible = T,
                  print("In financial market, people always want to predict the stock return. 
                        There are many things influncing the ups and downs of stock price, such as economical or political emergencies, continuing growth of revenue and profit. 
                        When doing investment, our main job is to make sure the ratio of stock returns to stock price variation, where variation represents the risk. 
                        Thus, it's important to know factors influncing stock returns, as well as stock price variation. Prof. Eugene Fama told us one of those factors that are related with stock return, book value of the company. 
                        So now I'm gonna check whether there is a relationship between stock returns and book value of companies, and whether I can make prediction by looking at the book value of the company.")
                ),
                box(
                  title = "My task",
                  width = 12,
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  print("1. Show whether there is a relationship between stock returns and book value;"),
                  h4(""),
                  print("2. Provide audience a way to explore what will influence stock returns based on data collected;") ,
                  h4(""),
                  print( "3. Show whether the data of stock return follows the Benford's Law.")      
                
                
                ))),
              ########tabitem of hello
      ###Component --------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "component_weight",
        fluidRow(
          box(
            h4(""),
              print("First let's see the weight of each stock") ,
              h4(""),
            title = "Choose an stock",
            width = 12,
            solidHeader = T,
            status = "info",
            collapsible = T,
            collapsed = F,
            selectInput(
              "component_indicator",
              "Choose an company",
              choice = colnames(sandp500[-1]),
              selected = "Open"
            )
          ),
          box(
            title = "S&P 500 Component Weight",
            width = 6,
            height = 460,
            solidHeader = T,
            status = "warning",
            collapsible = T,
            plotlyOutput("plotb1")),
          box(
            title = "S&P 500 Component",
            width = 6,
            height = 460,
            solidHeader = T,
            status = "warning",
            collapsible = T,
            plotlyOutput("plotb2"))
            
          )),
      
    tabItem(
        tabName = "component_price",
        fluidRow(

          box(
            title = "Choose years",
            sliderInput("component_indexyear", "Year:",
                        min=as.Date("2013-02-08"),
                        max=as.Date("2018-02-06"),
                        value=as.Date("2015-01-01"),step = 7),                    
            solidHeader = T,
            idth = 10,
            width = 10
          ),
          box(
            title = "Choose an stock",
            width = 4,
            solidHeader = T,
            status = "info",
            collapsible = T,
            collapsed = F,
            selectInput(
              "component_indicator1",
              "Choose an company",
              choice = colnames(sandp500[-1])
            )
          ),
          box(
            title = "Add a line?",
            width = 4,
            solidHeader = T,
            status = "info",
            collapsible = T,
            collapsed = F,
            selectInput(
              "component_addline",
              "Want to add a line?",
              choice = c("TRUE","FALSE"),
              selected = "FALSE"
            )
          ),
          box(
            title = "S&P Component",
            width = 12,
            height = 500,
            solidHeader = T,
            status = "warning",
            collapsible = T,
            plotlyOutput("plotb3"))
          
          
          
          
        )
        )
      ,
      ####substem1---------------------------------------------------------------------------------------------------
    tabItem(
      tabName = "subitem1",
      fluidRow(
        box(
          title = "Book Value per Share",
          width = 12,
          height = 500,
          solidHeader = T,
          status = "info",
          collapsible = T,
          plotlyOutput("plotc1")),
        box(
          title = "Caption",
          width = 12,
          solidHeader = T,
          status = "warning",
          collapsible = T,
          print("For this part, we only have seasonal data, since Book Value per Share is a accounting indicator, which people calculate it using the balanced sheet of a company."),
          h4(""),
          print("Here we have the Book Value per share of the index") ,
          h4("")    
          
          
        )
        
        )),
     ###subitem2----------------------------------------------------------------------------------------------------
    tabItem(
      tabName = "subitem2",
      fluidRow(
        box(
          title = "Choose years",
          sliderInput("stockreturnyear", "Year:",
                      min=as.Date("2013-02-08"),
                      max=as.Date("2018-02-06"),
                      value=as.Date("2015-01-01"),step = 7),                    
          solidHeader = T,
          idth = 10,
          width = 12
        ),
        box(
          title = "Choose an stock",
          width = 6,
          solidHeader = T,
          status = "info",
          collapsible = T,
          collapsed = F,
          selectInput(
            "stockreturn_stock",
            "Choose an company",
            choice = colnames(sandp500[-1])
          )
        ),
        box(
          title = "Select a time span of stock return",
          width = 6,
          solidHeader = T,
          status = "info",
          collapsible = T,
          collapsed = F,
          selectInput(
            "timespan",
            "Select a time span of stock return",
            choice = c("1 day","1 week", "2 weeks","1 month","3 months")
          )
        ),
        box(
          title = "Stock Return",
          width = 12,
          height = 500,
          solidHeader = T,
          status = "warning",
          collapsible = T,
          plotlyOutput("plotc2"))
        
          
          
        )
        
      ),
    
    
      ###Benford Analysis-------------------------------------------------------------------------------------------  
      tabItem(
        tabName = "BA",
        fluidRow(
          box(
            title = "Ideas",
            width = 10,
            solidHeader = T,
            status = "primary",
            collapsible = T,
            print("The first step is to count the frequencies of the first leading digits from 1 to 9 and calculate the proportion of each digit in the data from the frequencies. Then we compare the actual first digit frequency distribution with the theoretical Benford's one by chi square test and visualize it.")
          ),
          box(
            title = "Benford -- S&P 500 Index Return",
            width = 6,
            solidHeader = T,
            status = "warning",
            collapsible = T,
            plotlyOutput("plotd1")),
          box(
            title = "Benford --  Distribution of return of components stocks",
            width = 6,
            solidHeader = T,
            status = "success",
            collapsible = T,
            plotlyOutput("plotd2"))
          
        )#Fluidrow
      ),#tabitem
      
      tabItem(
        tabName = "Benford Analysis",
        fluidRow(
          box(
            title = "SOURCE DATA",
            width = 12,
            status = "info",
            collapsible = T,
            print("According to UCI Machine Learning Repository, the data is first extracted and used by Zieba, M., Tomczak, S. K., and Tomczak, J. M. (2016). This chapter describes the basic information about the data set that is related to the project.
                  The dataset contains five subsets, each has 64 indicators predicting corporate bankruptcy from the first year of forecasting period to the fifth year of it. The companies classified as 0 in the dataset remained healthy financial status after the forecasting period, while those classified as 1 did not survive successfully. The explanation of 64 indicators is appended in the Appendix chapter. In the project, I extract out the column of X4/Attr 4 as the subject of the study. 
                  Two things about the quality of the data set need to be mentioned. First, the data is heavily imbalanced. There are much more healthy companies existing than the problematic ones, as you can see in the above table. Second, the data is not comprehensive about all the companies operating in the forecasting period, because some information is not public or missing from the original database.")
            )
            ),
        fluidRow(
          tabBox(
            title = "1st-5th year of a forecasting period",
            width = 12,
            height = "500px",
            tabPanel("FirstYear",  dataTableOutput("firstyear")),
            tabPanel("SecondYear", dataTableOutput("secondyear")),
            tabPanel("ThirdYear", dataTableOutput("thirdyear")),
            tabPanel("FourthYear", dataTableOutput("fourthyear")),
            tabPanel("FifthYear", dataTableOutput("fifthyear")))
        )
          )#tabitem
      
    )#tabitems
    
    )#dashbody

)#ui

#######################################################################################################################
# ##function----------------------------------------------------------------------------------------------
indexfunc <- function(indicator,year,addline){
  
  
  
  #return
  index_return <- c(diff(index$Close),0)
  index_return <- index_return/index$Close
  index_return <- c(0,index_return[1:(length(index_return)-1)])
  #mutate
  index <- index%>%mutate("return"=index_return)
  index_sub <- subset(index, as.Date(index$Date)>=as.Date(index$Date[1]) & 
                                as.Date(index$Date)<=as.Date(year))
  if(indicator=="Close"){
    t=index_sub$Close
  }else if(indicator=="Open"){
    t=index_sub$Open
  }else if(indicator=="Volume"){
    t=index_sub$Volume
  }else if(indicator=="High"){
    t=index_sub$High
  }else if(indicator=="Low"){
    t=index_sub$Low
  }
  
  plot1 <- ggplot(data=index_sub,mapping = aes(x=Date , y=t))+
    geom_line(col="orange")+ylab("S&P 500 index")+ggtitle("S&P500 Index")
  plot2 <- ggplot(data=index_sub,mapping = aes(x=Date , y=t))+
    geom_line(col="orange")+ylab("S&P 500 index")+ggtitle("S&P500 Index")+geom_smooth()
  if(addline==TRUE){
    return(plot2)
  }else{ return(plot1)}
  
}



componentfunc <- function(indicator,year,addline){
  t <- noquote(indicator)
  
  
  Component <- data.frame("date"=sandp500$date,"component"=select(sandp500,t))
  Component_sub <- subset(Component, as.Date(Component$date)>=as.Date(Component$date[1]) & 
                        as.Date(Component$date)<=as.Date(year))
  colnames(Component_sub) <- c("date","component")
  plot1 <- ggplot(data=Component_sub,mapping = aes(x=date , y=component))+
    geom_line(col="orange")+ylab("Stock Price")+ggtitle("Stock Price")
  plot2 <- ggplot(data=Component_sub,mapping = aes(x=date , y=component))+
    geom_line(col="orange")+ylab("Stock Price")+ggtitle("Stock Price")+geom_smooth()
  if(addline==TRUE){
    return(plot2)
  }else{ return(plot1)}
  
}
stockreturnfunc <-  function(indicator,year,timespan){
  if(timespan=="1 day"){
    timespan=1
  }else if(timespan=="1 week"){
    timespan=7
  }else if(timespan=="2 weeks"){
    timespan=14
  }else if(timespan=="1 month"){
    timespan=30
  }else if(timespan=="3 months"){
    timespan=90
  }
  
#timespan , indicator
  t <- noquote(indicator)
  Component <- data.frame("date"=sandp500$date,"component"=select(sandp500,t))
  colnames(Component) <- c("date","component")
  t_diff<- c(diff(Component$component,timespan),rep(0,timespan)) #p1-p0
  t_diff <- t_diff/Component$component #get the return
  t_diff1 <- 0
  date <- 0
  for (i in 1:floor((length(Component$date)/timespan)-1)) {
    t_diff1 <- c(t_diff1, t_diff[timespan*i+1])
    date <- c(date,Component$date[timespan*i+1])
  }
  
  component1 <- data.frame("date"=as.Date(date), "return"=t_diff1)
  component1 <- component1%>%filter(date!=0)
  
#year
  
  Component_sub <- subset(component1, as.Date(component1$date)>=as.Date(component1$date[1]) & 
                            as.Date(component1$date)<=as.Date(year))
  colnames(Component_sub) <- c("date","component")
  
#plot
  plot1 <- ggplot(data=Component_sub,mapping = aes(x=date , y=component))+
    geom_line(col="orange")+ylab("Stock Price")+ggtitle("Stock Price")
  return(plot1)
  
}

ID <- 1:9
plotf <- function(data){
  data1 <- data %>% gather(Benford,proportion,key="type",value="numbers")
  plotbase <- ggplot(data1,aes(x=Digit,y=numbers,group=type,color=type))+
    geom_line(size=1.5)+
    geom_point(size=2)+
    scale_x_continuous("Digits", labels = as.character(ID), breaks = ID)+
    labs(y="Proportion")+
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position="none")
  return(plotbase)
}
####################################################################################################################
#server
server <- function(input, output) {

  ###index tab-------------------------------------------------------------------------------------------------
  output$plota <- renderPlotly({
    indexfunc(input$partone_indicator, input$indexyear,noquote(input$addline))
  })
  
  ###index component tab---------------------------------------------------------------------------------------
 output$plotb1 <- renderPlotly({
   w <- weight%>%filter(Symbol==input$component_indicator)
   ww <- w$Weight
   df <- data.frame(slices= c(as.double(ww),as.double(100-ww)),
                    lbls = c(w$Company, "Others"))
  plot_ly(data=df, labels=~lbls, values=~slices, type="pie")
   
   #bp <- ggplot(data=df,aes(x=lbls, y=slices))+
    # geom_bar(width = 1, stat = "identity")
   #bp+coord_polar(theta="y", start=0)
 })
 output$plotb2 <- renderPlotly({
   p <- ggplot(data=weight[1:50,],mapping = aes(x = reorder(Company,Weight),y=Weight))+
     geom_bar(stat="identity",fill="purple") +
     ggtitle("S&P500: TOP50 component weights ")+
     theme(axis.text.y = element_text(angle = 0, hjust = 1,size = 6))+xlab("company")+ylab("weight(%)")
   
   
   p+coord_flip()
   
 })
 
 output$plotb3 <- renderPlotly({
   
   componentfunc(input$component_indicator1, input$component_indexyear,noquote(input$component_addline))
   
 })
 
 output$plotc1 <- renderPlotly({
   ggplot(data=book_value_per_share , mapping = aes(x=Date , y=Value))+
     geom_line(col="purple")+ylab("Index Book Value per Share")+ggtitle("Index Book Value per Share")
   
 })
  
 output$plotc2 <- renderPlotly({
   stockreturnfunc(input$stockreturn_stock, input$stockreturnyear, input$timespan)
 })
  
 output$plotd1 <- renderPlotly({
   index_return <- c(diff(index$Close),0)
   index_return <- index_return/index$Close
   index_return <- c(0,index_return[1:(length(index_return)-1)])
   #mutate
   index <- index%>%mutate("return"=index_return)
   benford_indexreturn <- benford(index$return,number.of.digits = 1)
  Benford <- c(length(which(benford_indexreturn$data[,4]==1 )),length(which(benford_indexreturn$data[,4]==2 )),length(which(benford_indexreturn$data[,4]==3 )),
               length(which(benford_indexreturn$data[,4]==4 )),length(which(benford_indexreturn$data[,4]==5 )),length(which(benford_indexreturn$data[,4]==6 )),
               length(which(benford_indexreturn$data[,4]==7 )),length(which(benford_indexreturn$data[,4]==8 )),length(which(benford_indexreturn$data[,4]==9 )))
   
   numbers <- c(1,2,3,4,5,6,7,8,9)
   Benfordsample <- cbind.data.frame(numbers,Benford)
   ggplot(Benfordsample,aes(y=Benford,x=numbers))+geom_point(color="red")+geom_bar(color="red",stat = "identity")+
     scale_x_continuous("Digits", labels = as.character(ID), breaks = ID)+theme(panel.background = element_blank(),plot.background = element_rect(size=0.2,linetype="solid",color="black"))+ggtitle("S&P 500 Index Return")+labs(y="Frequency Distribution")
   
   
 })
 
 output$plotd2 <- renderPlotly({
   individual <- c(0)
   for (i in 1:505) {
     stock_return <- c(diff(sandp500[,i+1]),0)
     stock_return <- stock_return/sandp500[,i+1]
     stock_return <- c(0,stock_return[1:(length(stock_return)-1)])
     individual <- c(individual, sum(stock_return[-1]))
   }
   
   #construct a dataframe make up of company names and stock returns
   t <- data.frame("company"=colnames(sandp500), "return"=individual) 
   t <- t[-1,]
   
   #benford analysis on stock returns
   benford_indexreturn <- benford(t$return,number.of.digits = 1)
   Benford <- c(length(which(benford_indexreturn$data[,4]==1 )),length(which(benford_indexreturn$data[,4]==2 )),length(which(benford_indexreturn$data[,4]==3 )),
                length(which(benford_indexreturn$data[,4]==4 )),length(which(benford_indexreturn$data[,4]==5 )),length(which(benford_indexreturn$data[,4]==6 )),
                length(which(benford_indexreturn$data[,4]==7 )),length(which(benford_indexreturn$data[,4]==8 )),length(which(benford_indexreturn$data[,4]==9 )))
   
   numbers <- c(1,2,3,4,5,6,7,8,9)
   Benfordsample <- cbind.data.frame(numbers,Benford)
   ggplot(Benfordsample,aes(y=Benford,x=numbers))+geom_point(color="red")+geom_bar(color="red",stat = "identity")+
     scale_x_continuous("Digits", labels = as.character(ID), breaks = ID)+theme(panel.background = element_blank(),plot.background = element_rect(size=0.2,linetype="solid",color="black"))+ggtitle("S&P 500 Index Return")+labs(y="Frequency Distribution")
   
   
 })
  ###factor------------------------------------------------------------------------------------------------------
  output$res <- renderText({
    if (input$sidebarCollapsed) {
      "Sidebar is collapsed"
    } else {
      "Sidebar is expanded.
      "
    }
  })
  
  
  
}


shinyApp(ui = ui, server = server)









