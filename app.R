# app.R#
library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(gridExtra)
library(plotly)

options(digits = 2)

DATA = read_excel("AFWPS Visualisation DATA.xlsx", sheet = "Graph1")
DATA2 = read_excel("AFWPS Visualisation DATA.xlsx", sheet = "Graph2")
DATA3 = read_excel("AFWPS Visualisation DATA.xlsx", sheet = "Graph3")
DATA4 = read_excel("AFWPS Visualisation DATA.xlsx", sheet = "Graph4")

ui <- dashboardPage(skin='green',
  dashboardHeader(title='Australian Forest and Wood Products Statistics (AFWPS) Data Visualisation App',titleWidth = 800),
  dashboardSidebar(
    sidebarMenu(#menuItem('Charts',icon=icon('fas fa-chart-bar')),
                menuItem('Raw data', href='https://github.com/LynnBai01/Data-visualisation-project-is-killing-me/blob/master/AFWPS%20Visualisation%20DATA.xlsx', icon=icon('fal fa-table')),
                menuItem('Souce code for app', href='https://github.com/LynnBai01/Data-visualisation-project-is-killing-me/blob/master/app.R', icon=icon('github')),
                menuItem('Instruction video', href='https://youtu.be/Y5yDFr4yFyU', icon=icon('fab fa-youtube'))
  )),
  dashboardBody(
    verticalLayout(
      titlePanel("Plantation"),
      plotlyOutput("Plot1"),
      plotlyOutput("Plot11"),
      plotlyOutput("Plot12"),
      titlePanel("Harvest"),
      plotOutput("Plot2"),
      wellPanel(
        sliderInput("year2",
                    "Year",
                    min = min(DATA2$Year),
                    max = max(DATA2$Year),
                    value = min(DATA2$Year),
                    animate = T)
      ),
      titlePanel("Trade and Consumption"),
      plotOutput("Plot3"),
      wellPanel(
        selectInput('x', 'X Variable',
                    c('Imports by volume' = 'I_volume',
                      'Imports by value($m)' = 'I_value' )),
        selectInput('y', 'Y Variable',
                    c('Emports_by_volume' = 'E_volume',
                      'Exports by value($m)' = 'E_value')),
        selectInput('size', 'Bubble size Variable',
                    c('Volume of wood production by product type' = 'production',
                      'Apparent consumption' = 'consumption')),
        sliderInput("year3",
                    "Year",
                    min = min(DATA3$Year),
                    max = max(DATA3$Year),
                    value = min(DATA3$Year),
                    animate = T)
      ),
      titlePanel("Employment and Wages"),
      plotOutput("Plot4"),
      wellPanel(
        sliderInput("year4",
                    "Year",
                    min = min(DATA4$Year),
                    max = max(DATA4$Year),
                    value = min(DATA4$Year),
                    animate = T)
      )
    )
  )
)

server <- function(input, output) {
  
  output$Plot1 <- renderPlotly({
    a = ggplot(DATA) + geom_line(aes(Year,consumption,col = Type)) + 
      ylab('Gross roundwood equivalent of consumption by wood products(000 m3)')
    ggplotly(a)

  })
  
  output$Plot11 <- renderPlotly({
    b = ggplot(DATA) +  geom_line(aes(Year,Plantation_areas,col = Type)) +  
        ylab('Plantation areas (000ha)')
    ggplotly(b)
  
    
  })
  
  output$Plot12 <- renderPlotly({
    c = ggplot(DATA) +  geom_line(aes(Year,Plantation_estab,col = Type)) +  
      ylab('Plantation established (000ha)')
    ggplotly(c)
  })
  
  output$Plot2 <- renderPlot({
    
    DATA1 = DATA2[DATA2$Year == input$year2,]
    a = ggplot(DATA1,aes(Type,GV_h)) + geom_col(fill = 'darkred') +  ylim(0,2600) + 
      ylab('Gross value of logs harvested ($m)') + theme(axis.text.x=element_text(angle=20,size=7)) +
      geom_text(aes(label = round(GV_h,2)), nudge_y = 100) 
    b = ggplot(DATA1,aes(Type,V_h)) +  geom_col(fill = 'darkgreen') +  ylim(0,34000) + 
      ylab('Volume of logs harvested (000 m3)') + theme(axis.text.x=element_text(angle=20,size=7))+
      geom_text(aes(label = round(V_h,2)), nudge_y = 1000) 
    c = ggplot(DATA1,aes(Type,pi)) +  geom_col(fill = 'darkblue') +  ylim(0,180) + 
      ylab('Log price index') + theme(axis.text.x=element_text(angle=20,size=7))+
      geom_text(aes(label = round(pi,2)), nudge_y = 5) 
    grid.arrange(a,b,c,nrow=1)
  })
  
  
  output$Plot3 <- renderPlot({
    
    DATA1 = DATA3[DATA3$Year == input$year3,c('Year','Type',input$x,input$y,input$size)]
    names(DATA1) = c('Year','Type','x','y','size')
    ggplot(DATA1,aes(x,y,col = Type, size = size)) + geom_point() + ylim(0,2300) +  xlim(0,2300) + xlab('') + ylab('') + 
      geom_text(aes(label = paste('x:',round(x,2),'y:',round(y,2),'size:',round(size,2),sep=' ')), nudge_y = 60,size = 3) 
  })
  
  
  output$Plot4 <- renderPlot({
    
    DATA1 = DATA4[DATA4$Year == input$year4,]
    a = ggplot(DATA1,aes(Type,Employment)) + geom_col(fill = 'darkred') +  ylim(0,30) + 
      ylab('Employment (000)') + theme(axis.text.x=element_text(angle=20,size=7)) +
      geom_text(aes(label = round(Employment,2)), nudge_y = 1) 
    b = ggplot(DATA1, aes(Type,Wages)) +  geom_col(fill = 'darkgreen') +  ylim(0,1200) + 
      ylab('Wages ($m)') + theme(axis.text.x=element_text(angle=20,size=7)) +
      geom_text(aes(label = round(Wages,2)), nudge_y = 50) 
    grid.arrange(a,b,nrow=1)
  })
}
shinyApp(ui, server)