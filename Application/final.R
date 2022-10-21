library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
library(shinythemes)
library(slickR)
library(forecast)
library(dygraphs)
library(plyr)
library(ichimoku)
library(tseries)
library(curl)
library(prophet)
library(dplyr)  # pipe
library(tidyquant)
library(tsfknn)
library(plotly)
library(tidyr)
library(quantmod)
library(tidyverse)
library(highcharter)


#--------------------------------------
#READING FILES
#--------------------------------------
title <- tags$img(src='sidewhite.png',height='50', width='200')
# getting the symbols
df<-tq_get(c("BHARTIARTL.NS","RELIANCE.NS","TCS","AAPl","BAJAJFINSV.NS","WIPRO.NS","TITAN.NS","ONGC.NS","INFY.NS","CIPLA.NS","HCLTECH.NS","TECHM.NS","M&M.NS","BAJAJ-AUTO.NS","HEROMOTOCO.NS","EICHERMOT.NS","MARUTI.NS","LT.NS","ICICIBANK.NS","AXISBANK.NS","KOTAKBANK.NS","INDUSINDBK.NS","BAJFINANCE.NS","HINDUNILVR.NS","NESTLEIND.NS","PIDILITIND.NS","BERGEPAINT.NS","DABUR.NS","COLPAL.NS","GODREJCP.NS","HAVELLS.NS") ,get = "stock.prices", from = "2010-01-01")

#fit the prophet model----
dataset<-data.frame(ds = df$date , y  = as.numeric(df$close))
prophet_1<-prophet(dataset)

#fit the forecast prophet model-----
future<- make_future_dataframe(prophet_1 , periods =30)
forecastprophet <-predict(prophet_1 , future)


# ui
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = title),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "home", icon = icon('home')),
                        selectInput(inputId = "drop" , label = "Choose the symbol" ,list("TCS","AAPl","BHARTIARTL.NS","BAJAJFINSV.NS","WIPRO.NS","TITAN.NS","ONGC.NS","INFY.NS","CIPLA.NS","HCLTECH.NS","RELIANCE.NS","TECHM.NS","M&M.NS","BAJAJ-AUTO.NS","HEROMOTOCO.NS","EICHERMOT.NS","MARUTI.NS","LT.NS","ICICIBANK.NS","AXISBANK.NS","KOTAKBANK.NS","INDUSINDBK.NS","BAJFINANCE.NS","HINDUNILVR.NS","NESTLEIND.NS","PIDILITIND.NS","BERGEPAINT.NS","DABUR.NS","COLPAL.NS","GODREJCP.NS","HAVELLS.NS")),
                        
                        menuItem("Data visualisation" , tabName = "page1",icon = icon("chart-bar")),
                        menuItem("Arima Forecast",tabName = "time-series-1",icon = icon("calendar")),
                        menuItem("Arima Components",tabName = "time-series-2" , icon = icon("chart-line")),
                        menuItem("Let's dive in !",tabName = "time-series-4" , icon = icon("chart-line")),
                        menuItem("Let's be assured !",tabName = "tests",icon = icon("list - alt")),
                        menuItem("Prophet Forecast",tabName = "time-series-3" , icon = icon("calendar")),
                        
                        sliderInput("slider" , "Number of days to be forecasted :" , min=0 , max=180,value=50 , step = 10)
                      )
                    ),
                    dashboardBody(
                      #custom css
                      tags$head(
                        tags$style(
                          HTML(" #compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;}
    				.box.box-solid.box-primary {border: 1px solid #1681c4 !important;}
    				.box.box-solid.box-primary>.box-header { background: #1681c4 !important; background-color: #1681c4 !important; }
    				.sidebar-menu>li {font-size:17px;}")
                        )
                      ),
                      
                      tabItems(
                        tabItem(tabName = "home",
                                #tabPanel(slickROutput("PhotoSlides",height="200",width="500px")),
                                #tags$img(src="1.jpg", height=300, width=600),
                                #----------------------------------------------------------------------
                                #----------------------SLIDER------------------------------------------
                                #-----------------------------------------------------------------------
                                fluidRow(box(width=12,HTML('<html><head><title>Slideshow Images</title>
                                <style>
      * {
        box-sizing: border-box
      }
      body {
        font-family: Verdana, sans-serif;
        margin: 0
      }
      .mySlides {
        display: none
      }
      img {
        vertical-align: middle;
      }
      .slideshow-container {
        max-width: 800px;
        position: relative;
        margin: auto;
      }
      /* Next & previous buttons */
      .prev,
      .next {
        cursor: pointer;
        position: absolute;
        top: 50%;
        width: auto;
        padding: 16px;
        margin-top: -22px;
        color: black;
        font-weight: bold;
        font-size: 18px;
        transition: 1s ease;
        border-radius: 0 3px 3px 0;
        user-select: none;
      }
      /* Position the "next button" to the right */
      .next {
        right: 0;
        border-radius: 3px 0 0 3px;
      }
      /* On hover, add a black background color with a little bit see-through */
      .prev:hover,
      .next:hover {
        background-color: rgba(0, 0, 0, 0.8);
      }
      /* Caption text */
      .text {
        color: #ffffff;
        font-size: 15px;
        padding: 8px 12px;
        position: absolute;
        bottom: 8px;
        width: 100%;
        text-align: center;
      }
      /* Number text (1/3 etc) */
      .numbertext {
        color: #ffffff;
        font-size: 12px;
        padding: 8px 12px;
        position: absolute;
        top: 0;
      }
      /* The dots/bullets/indicators */
      .dot {
        cursor: pointer;
        height: 15px;
        width: 15px;
        margin: 0 2px;
        background-color: white;
        border-radius: 50%;
        display: inline-block;
        transition: background-color 3s ease;
      }
      .active,
      .dot:hover {
        background-color: #111111;
      }
      /* Fading animation */
      .fade {
        -webkit-animation-name: fade;
        -webkit-animation-duration: 10s;
        animation-name: fade;
        animation-duration: 10s;
      }
      @-webkit-keyframes fade {
        from {
          opacity: .4
        }
        to {
          opacity: 1
        }
      }
      @keyframes fade {
        from {
          opacity: .4
        }
        to {
          opacity: 1
        }
      }
      /* On smaller screens, decrease text size */
      @media only screen and (max-width: 300px) {
        .prev,
        .next,
        .text {
          font-size: 11px
        }
      }
    </style>
  </head>
  <body>
    <div class="slideshow-container">
      <div class="mySlides fade">
        <div class="numbertext">1 / 3</div>
        <img src="1.jpg" style="width:100%">
        
      </div>
      <div class="mySlides fade">
        <div class="numbertext">2 / 3</div>
        <img src="2.jpg" style="width:100%">
        
      </div>
      <div class="mySlides fade">
        <div class="numbertext">3 / 3</div>
        <img src="3.jpg" style="width:100%">
        
      </div>
      <a class="prev" onclick="plusSlides(-1)">&#10094;</a>
      <a class="next" onclick="plusSlides(1)">&#10095;</a>
    </div>
    <br>
    <div style="text-align:center">
      <span class="dot" onclick="currentSlide(1)"></span>
      <span class="dot" onclick="currentSlide(2)"></span>
      <span class="dot" onclick="currentSlide(3)"></span>
    </div>
    <script>
      var slideIndex = 0;
      showSlides();
      function showSlides() {
        var i;
        var slides = document.getElementsByClassName("mySlides");
        for(i = 0; i < slides.length; i++) {
          slides[i].style.display = "none";
        }
        slideIndex++;
        if(slideIndex > slides.length) {
          slideIndex = 1
        }
        slides[slideIndex - 1].style.display = "block";
        setTimeout(showSlides, 10000); // Change image every 8 seconds
      }
    </script>
  </body>
</html>

'))),#br(),
                                fluidRow(
                                  column(colspan=2,width=12,tags$h1(align="center","Video Of the Day",style="color:white;font-family:Helvetica")),
                                  #column(width=5,tags$h2(width=5,"Top News"))
                                ),
                                fluidRow(column(colspan=2,width=12),tags$h4("                                                       ")),
                                fluidRow(
                                  box(width=6,HTML('<iframe width="100%" height="430" src="https://www.youtube.com/embed/A7fZp9dwELo" 
                       frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; 
                       gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                                  box(width=6,
                                      HTML('<iframe width="100%" height="430" src="https://www.youtube.com/embed/yRr0_gJ-3mI" 
                       frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; 
                       gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
                        ),
                        tabItem(tabName = "page1",
                                fluidRow(
                                  box(title = "Time Series",
                                      status = "primary", solidHeader = TRUE,width=12,
                                      collapsible = TRUE,
                                      tabBox(width=14,
                                             tabPanel(title="Time Series", plotlyOutput(height = 700, "plot2")),
                                             tabPanel(title="Bar chart of Volume", plotlyOutput(height = 700, "plot4")),
                                             tabPanel(title="Trends In Annual Returns", plotlyOutput(height = 700, "plot6")),
                                             tabPanel(title="MACD", plotlyOutput(height = 700, "plot8"))
                                )
                        ))),
                        
                        
                        
                        tabItem(tabName = "time-series-1",
                                fluidRow(
                                  box(
                                    title = "Data",status = "primary",
                                    collapsible = TRUE,solidHeader = TRUE,
                                    height=850,
                                    width = 4,
                                    
                                    style='width:400px;height:700px;overflow-y: scroll;',
                                    tableOutput("Data")   
                                  ),
                                  box(title = "ARIMA FORECAST" , status = "primary",
                                      collapsible = TRUE,solidHeader = TRUE, width=8,
                                      tabBox(width=12,
                                             tabPanel(title = "AUTO-ARIMA", plotOutput(height = 700,"plot11")),
                                             tabPanel(title = "POINT FORECAST FOR PRICE", plotlyOutput(height = 700,"value_forecast")),
                                             
                                             tabPanel(title = "Decomposition Plot", plotOutput(height = 700,"Decomposition")),
                                             tabPanel(title = "CANDLE-STICK", plotlyOutput(height = 700,"plot15"))
                                      ))
                                  
                                  )),
                                  
                                  
                                
                        tabItem(tabName = "time-series-2",
                                fluidRow(
                                  box(title = "ACF" , status = "primary",
                                      collapsible = TRUE,solidHeader = TRUE,width=12,
                                      tabBox(width=12,
                                             tabPanel(title="ACF", plotOutput(height=700,"acfplot")),
                                             tabPanel(title="PACF", plotOutput(height=700,"plot18")),
                                             tabPanel(title="Stationarity", dygraphOutput(height=700,"Stationary"))
                                      )))),
                       
                        
                        tabItem(tabName = "time-series-3",
                                fluidRow(
                                  box(title = "Time Series-3",status = "primary",
                                      collapsible = TRUE,solidHeader = TRUE,width = 12,
                                      tabBox(width = 12,
                                             tabPanel(title = "Prophet Model", plotOutput(height = 700,"plot12")),
                                             tabPanel(title = "Prophet Plot Components", plotOutput(height = 700,"plot13"))
                                )))),
                        tabItem(tabName = "time-series-4",
                                fluidRow(
                                  box(title = "Time Series",
                                      status = "primary", solidHeader = TRUE,width=12,
                                      collapsible = TRUE,
                                      tabBox(width=14,
                                             tabPanel(title="Ichimoku Graph", dygraphOutput(height=700,"IM")),
                                             tabPanel(title="RSI Plot", dygraphOutput(height=700,"RSI")),
                                             tabPanel(title="Bollinger Bands Plot", plotOutput(height=700,"plot3")),
                                             tabPanel(title="Cross Plot", highchartOutput(height=700,"plot33"))
                                      )
                                  ))),
                        
                        
                        tabItem(tabName = "tests",
                                fluidRow(
                                  box(title = "box_test_I " , status = "primary",
                                      collapsible = TRUE,solidHeader = TRUE, verbatimTextOutput("box_test_I")),
                                  box(title = "box_test_II " , status = "primary",
                                      collapsible = TRUE,solidHeader = TRUE, verbatimTextOutput("box_test_II")),
                                  box(title = "box_test_III " , status = "primary",
                                      collapsible = TRUE,solidHeader = TRUE, verbatimTextOutput("box_test_III")),
                                  box(title = "ADF test " , status = "primary",
                                      collapsible = TRUE,solidHeader = TRUE, verbatimTextOutput("adf"))
                                  
                                  
                                  
                                ))
                                  
                                  
                      )))

# server
# to set the data
server <- function(input, output) {
  
  
  #  function for candlestick plot
  
  candleStick_plot<-function(symbol,from,to){
    options("getSymbols.warning4.0"=FALSE)
    options("getSymbols.stock.prices.warning"=FALSE)
    tq_get(symbol,
           from = from,
           to = to,
           warnings = FALSE) %>% 
      mutate(greenRed=ifelse(open-close>0,
                             "Red",
                             "Green")) %>% 
      ggplot()+
      geom_segment(aes(x = date,
                       xend=date,
                       y =open,
                       yend =close,
                       colour=greenRed),
                   size=0.1)+
      theme_tq()+
      geom_segment(aes(x = date,
                       xend=date,
                       y =high,
                       yend =low,
                       colour=greenRed))+
      scale_color_manual(values=c("Forest Green","Red"))+
      ggtitle(paste0(symbol," (",from," - ",to,")"))+
      theme(legend.position ="none",
            axis.title.y = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title= element_text(hjust=0.5))->uu
    return(ggplotly(uu))
  }
  
  
  output$plot1<-renderPlot({
    df<-filter(df, symbol==input$drop)
    df %>% 
      ggplot(aes(x=date , y=close))+
      geom_line(color = palette_dark()[[1]])+
      scale_y_log10()+
      geom_smooth(method = "lm")+
      labs(y = "price"  , x = "")+
      theme_minimal()
  })
  
  # Time Series
  output$plot2<-renderPlotly({
    df<-filter(df, symbol==input$drop)
    df %>% 
      ggplot(aes(x=date , y=close))+
      geom_line()+xlab("Date")+theme_minimal()+ggtitle(input$drop)-> b
    
    return(b)
  })
  
  #Bollinger Bands
  output$plot3<-renderPlot({
    df<-filter(df, symbol==input$drop)
    df %>% tail(100) %>% 
      ggplot(aes(x = date, y = close, open = open,
                 high = high, low = low, close = close)) +
      geom_candlestick() +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20 ,linetype = 5) +
      labs(y = "Closing Price", x = "Month") + 
      theme_minimal()+ggtitle(input$drop)
    
  })
  
  #Golden Cross
  output$plot33<-renderHighchart({
    df<-filter(df, symbol==input$drop)
    df<-getSymbols(input$drop, auto.assign = FALSE, from = '2010-01-01', to = lubridate::today())
    highchart(type="stock")%>%
      hc_add_series(df)%>%
      hc_add_series(SMA(na.omit(Cl(df)),n=50),name="SMA(50)")%>%
      hc_add_series(SMA(na.omit(Cl(df)),n=200),name="SMA(200)")
  })
  
  #Bar Chart Of Volume
  output$plot4<-renderPlotly({
    df<-filter(df, symbol==input$drop)
    df %>% 
      ggplot(aes(x = date , y = volume))+
      geom_segment(aes(xend = date , yend = 0 , color = volume))+
      geom_smooth(method = "loess" ,se = F)+
      labs(y="volume" , x="Date")+
      scale_color_gradient(low = "#249cd4" , high="#0c7829")+
      theme_minimal()+
      theme(legend.position = "none")+ggtitle(input$drop)->c
    return(ggplotly(c))
  })
  
  #Trends In Annual Returns 
  output$plot6<-renderPlotly({
    df<-filter(df, symbol==input$drop)
    apple_returns_1<-df %>% 
      tq_transmute(
        select = close,
        mutate_fun = periodReturn,
        period = "yearly",
        type = "log",
        col_rename ="yearly_return" 
      )
    apple_returns_1 %>% 
      ggplot(aes(x = year(date) , y=yearly_return))+
      geom_hline(yintercept = 0  , color = palette_light()[[1]])+
      geom_point(size = 3 , color = palette_light()[[3]])+
      geom_line(size = 1 , color = palette_light()[[3]])+
      geom_smooth(method = "lm" , se = F)+
      labs(x = "Date" , y = "annual returns")+
      theme_minimal()+ggtitle(input$drop)->e
    return(ggplotly(e) )
  })
  
  #MACD  
  output$plot8<-renderPlotly({
    df<-filter(df, symbol==input$drop)
    appl_macd<-df %>% tail(300) %>% 
      tq_mutate(
        select = close,
        mutate_fun = MACD , 
        nFast = 15,
        nSlow = 25,
        nSig = 9,
        maType = SMA
      ) %>% 
      mutate(diff = macd - signal) %>% 
      select(-(open:volume))
    
    appl_macd %>% 
      ggplot(aes(x = date))+
      geom_hline(yintercept = 0 , color = palette_light()[[1]])+
      geom_line(aes(y=macd ))+
      geom_line(aes(y = signal) , color  ="red" , linetype = 2)+
      geom_bar(aes(y = diff) ,stat = "identity" , color = palette_dark()[[1]])+
      labs(y = "MACD" , x = "Date")+
      theme_minimal()+
      scale_color_tq()+ggtitle(input$drop)->f
    return(ggplotly(f) )
  })
  
  #AUTO-ARIMA
  output$plot11<-renderPlot({
    df<-filter(df, symbol==input$drop)
    auto_arima<-auto.arima(df$close, lambda = "auto") 
    autoplot(forecast(auto_arima ,input$slider))+theme_minimal()
    
  })
  output$Data<-renderTable({
    df<-filter(df, symbol==input$drop)
    pi<-auto.arima(df$close, lambda = "auto")
    q<- forecast(pi, h=input$slider)
    print(q)
    
  })
  
  
  output$value_forecast <- renderPlotly({
    df<-filter(df, symbol==input$drop)
    y_ =ts(df$close, frequency = 12)
    NonSt <- forecast(auto.arima(y_,ic="aic",trace = TRUE, lambda = "auto"),level = c(95),h = input$slider)
    ds <- NA
    ds$Date_ <- NA
    ds$Close <- NA
    Date_ <- c(seq( as.Date(Sys.Date()), by=1, len = input$slider))
    Close <- c(as.numeric(NonSt$mean))
    ds <- data.frame(Date_, Close)
    plog <- ggplot(ds, aes(x = Date_, y = Close, group = input$drop) )+ geom_line(alpha = 0.5, size=0.7) + theme(legend.text=element_text(size=16))
    plog1 <- ggplotly(plog)
    return(plog1)
    
  })
  
  #  Tests
  # ADF
  output$adf <- renderPrint({
    df <- filter(df, symbol == input$drop)
    y_ = ts(na.approx(df$close), frequency = 12)
    adf_test<-adf.test(y_)
    adf_test
    
  })
  
  # Box Linguee Test for different
  output$box_test_I <- renderPrint({
    df <- filter(df, symbol == input$drop)
    y_ = ts(df$close, frequency = 12)
    auto_arima<-auto.arima(df$close, lambda = "auto")
    Box_test_I<-Box.test(resid(auto_arima),type="Ljung",lag=1)
    Box_test_I
  })
  output$box_test_II <- renderPrint({
    df <- filter(df, symbol == input$drop)
    y_ = ts(df$close, frequency = 12)
    auto_arima<-auto.arima(df$close, lambda = "auto")
    Box_test_II<-Box.test(resid(auto_arima),type="Ljung",lag=2)
    Box_test_II
  })
  output$box_test_III <- renderPrint({
    df <- filter(df, symbol == input$drop)
    y_ = ts(df$close, frequency = 12)
    auto_arima<-auto.arima(df$close, lambda = "auto")
    Box_test_III<-Box.test(resid(auto_arima),type="Ljung",lag=3)
    Box_test_III
  })
  
  
  
  
  #Candle stick
  
  output$plot15<-renderPlotly({
    
    candleStick_plot(input$drop,from = '2018-01-01',to = lubridate::today()) 
    
  })
  #Prophet Model  
  output$plot12<-renderPlot({
    df<-filter(df, symbol==input$drop)
    dataset<-data.frame(ds = df$date , y  = as.numeric(df$close))
    prophet_1<-prophet(dataset)
    future<- make_future_dataframe(prophet_1 , periods =input$slider)
    forecastprophet <-predict(prophet_1 , future)
    plot(prophet_1 ,forecastprophet , xlab="time" , ylab="")
    
  })
  
  #Prophet Plot Components
  output$plot13<-renderPlot({
    df<-filter(df, symbol==input$drop)
    prophet_plot_components(prophet_1 , forecastprophet)+theme_minimal()
  })
  
  #acf 
  output$acfplot <- renderPlot({
    df<-filter(df, symbol==input$drop) 
    ggAcf(df$close)+theme_minimal()+labs(title = "acf plot")
    
  })
  output$plot18 <- renderPlot({
    df<-filter(df, symbol==input$drop)
    ggPacf(df$close , main = "pacf")+theme_minimal()
  })
  
  output$Stationary <- renderDygraph({
    df<-filter(df, symbol==input$drop)
    y=ts(df, f=1)
    # Seasonal Differencing
    y_seasdiff <- diff(y, lag=frequency(y), differences=1) # seasonal differencing
    dygraph(y_seasdiff, main="Seasonally Differenced") # still not stationary!
    # Make it stationary
    ndiffs(y_seasdiff) # number of differences need to make it stationary
    #> 1
    stationaryTS <- diff(y_seasdiff, differences= 1)
    dygraph(stationaryTS) # appears to be stationary
  })
  
  output$Decomposition <- renderPlot({
    df<-filter(df, symbol==input$drop)
    y1=ts(na.omit(df$close), f = 12)
    decompose__ <- decompose(y1,"multiplicative")
    decompose__
    plot_ <- autoplot(decompose__)
    return(plot_)
  })
  
  output$IM <- renderDygraph({
    df<-filter(df, symbol==input$drop)
    cloud <- ichimoku(df,ticker="TKR")
    dygraph(cloud)
  })
  
  output$RSI <- renderDygraph({
    df<-filter(df, symbol==input$drop)
    y1=ts(na.omit(df$close), f = 12)
    print(y1)
    rsi <- RSI(y1, n=12)
    dygraph(rsi)
  })
  
  
  
}

# combining UI with server
shinyApp(ui=ui, server = server)


