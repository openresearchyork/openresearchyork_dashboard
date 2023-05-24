#### LOAD PACKAGES ####

library(ggplot2)#pretty (static) plots

library(dplyr)#handy data manipulation tools
options(dplyr.summarise.inform = FALSE)#suppress messages to console from summarise function

library(shiny)#web applications

library(DT)#interactive tables

library(plotly)#interactive plots

library(shinythemes)#layout/themes for app

library(shinyWidgets)#fancy interactive buttons


#### LOAD AND PREPARE DATA ####

OA<-read.csv("Publications_at_the_University_of_York_SciVal.csv", header=T, skip=15)

OA <- OA[1:(nrow(OA)-2),]#update dataframe to delete last rows containing metadata

OA$Year<-as.factor(OA$Year)#column 'year' should be a factor ('category') for grouping data

OA$Open.Access2<-as.factor(OA$Open.Access)#create new column based on 'open access' (should be a factor)

levels(OA$Open.Access2)<-c("Not Open Access", "Bronze", "Green", "Gold", "Gold", "Green", "Hybrid Gold", "Hybrid Gold")# rename some of the levels

OA$generalOA<-ifelse(OA$Open.Access2=="Not Open Access" | OA$Open.Access2=="Bronze", "Closed Access", "Open Access")#create a new variable that groups into 'closed' and 'open access'

OA1<-OA %>%
  #create summary stats by year and OA format
  group_by(Year, Open.Access2, generalOA)%>%
  summarise(number_Publications=n())%>%
  group_by(Year)%>%
  mutate(number_Publications_per_Year=sum(number_Publications), 
         prop=round(number_Publications/number_Publications_per_Year, digits=3))

version<-read.csv("Publications_at_the_University_of_York_SciVal.csv", header=FALSE, skip=9, nrows=1)#retrieve metadata

info_text<-HTML(paste("Data retrieved from Unpawall.com via SciVal. All publications affiliated with the University of York indexed on Scopus are included, data last updated ", version[,2], ". A short definition of the open access formats are below.<br/> Green = Self-archived in repository<br/> Gold = Available through fully open-access journal under creative commons licence<br/> Hybrid Gold = Option to publish open-access in a subscription journal<br/> Bronze = Free to read on the publisher page, but no clear license", sep=""))#create info text to be displayed in app

#### Create Custom Slider Options ####

css_slider <- "
#customSlider .irs-bar {
    border-top: 1px solid #ddd;
    border-bottom: 1px solid #ddd;
    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
}
#customSlider .irs-bar-edge {
    border: 1px solid #ddd;
    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
}
#customSlider .irs-line {
    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
    border: 1px solid #ddd;
}
"

#### USER INTERFACE ####

#sets up the layout of the user interface
ui <- fluidPage(
  #add a title
  titlePanel("Open Access at the University of York"),
  theme=shinytheme("lumen"),
  
  #arrange ui in two panels next to one another
  sidebarLayout(
    
    #Side Panel with Controls and Buttons
    sidebarPanel(
      awesomeCheckboxGroup(
        inputId = "access",
        label = "Choose Open Access Format", 
        choices = levels(OA1$Open.Access2),
        selected = levels(OA1$Open.Access2)),
      
      tags$style(type='text/css', css_slider), #add css style from above definition
      div(id = "customSlider",
        sliderInput(
          inputId = "year", 
          label = "Select Year", 
          min = 2017,
          max = 2022,
          value = 2017, 
          sep="")),
      
      actionButton("show_help", "Further information")),
    
    # Main Panel with Plot and Table in Tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotly::plotlyOutput('plot_OA')),
        tabPanel("Table",
                 DT::DTOutput('table_OA'))
      )
    )
  )
)


#### SERVER ####

server <- function(input, output, session){
  #open info text at click of button
  observeEvent(input$show_help, {showModal(modalDialog(info_text))
  })
  
  #reactive conductor to speed up the app (calculations for plot and table done only once)
  rval_OAfiltered<-reactive({
    # Filter for the selected year and access (inputId in ui)
    subset(OA1, Year ==input$year & Open.Access2 %in% input$access)
  })
  
  #add a plot
  output$plot_OA<-plotly::renderPlotly({
    # Plot selected year and access
    rval_OAfiltered() %>% 
      mutate(Open.Access2 = factor(Open.Access2, levels = c("Hybrid Gold", "Gold", "Green", "Bronze", "Not Open Access")))%>%
      ggplot(aes(x = generalOA, y = prop, fill=Open.Access2)) +
      geom_col(color="black", size=0.1)+
      scale_fill_manual(values=c("khaki3","goldenrod", "palegreen4", "coral3", "gray50"))+
      scale_y_continuous(labels = scales::percent, limits=c(0,0.8))+
      labs(x="", y="Publications in selected year [%]", fill="Open Access Format")+
      theme_classic(base_size=12)
  })
  
  #add a table
  output$table_OA <-  DT::renderDT({
    # Table of selected year and access
    rval_OAfiltered()
  })
}


#### APP - Pulls UI and Server together ####

shinyApp(ui = ui, server = server)#launch app

