#### LOAD PACKAGES ####

#install all packages when using R for first time (uncomment below)
#install.packages("readxl", "ggplot2", "tidyverse", "shiny", "DT", "plotly", "shinythemes", "shinyWidgets")

library(readxl)#read excel files

library(ggplot2)#pretty (static) plots

library(tidyverse)#handy data manipulation tools
options(dplyr.summarise.inform = FALSE)#suppress messages to console from summarise function

library(shiny)#web applications

library(DT)#interactive tables

library(plotly)#interactive plots

library(shinythemes)#layout/themes for app

library(shinyWidgets)#fancy interactive buttons


#### LOAD AND PREPARE DATA ####

## Open Access data from SciVal ##
OA<-read.csv("Publications_at_the_University_of_York_SciVal.csv", header=T, skip=15)

OA <- OA[1:(nrow(OA)-1),]#update dataframe to delete last row containing metadata

OA$Year<-as.factor(OA$Year)#column 'year' should be a factor ('category') for grouping data

OA$`Open Access`<-as.factor(OA$Open.Access)#create new column based on 'open access' (should be a factor)

levels(OA$`Open Access`)<-c("Not Open Access", "Bronze", "Green", "Gold", "Gold", "Green", "Hybrid Gold", "Hybrid Gold")# rename some of the levels

OA$`Access`<-ifelse(OA$`Open Access`=="Not Open Access" | OA$`Open Access`=="Bronze", "Closed Access", "Open Access")#create a new variable that groups into 'closed' and 'open access'

OA1<-OA %>%
  #create summary stats by year and OA format
  filter(!Publication.type %in% c("Erratum", "Retracted", "Article in Press"))%>%
  rename(`Publication Type` = Publication.type)%>%
  group_by(Year, `Open Access`, `Access`, `Publication Type`)%>%
  summarise(`Number of Publications`=n())

version<-read.csv("Publications_at_the_University_of_York_SciVal.csv", header=FALSE, skip=9, nrows=1)#retrieve metadata for OA

## Transitional agreements and corresponding author data ## 

#Data from Scopus was downloaded in chunks of 2000 publications 
#(every year split in publlications containing and not containing 'human' as a keyword)
#the below pipe combines the individual download csvs
scopusCA<-list.files(path=".", pattern="human.csv$", recursive = T) %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows %>%
  select("Title", "Year", "Source title", "DOI", "Funding Details", "Correspondence Address", "Publisher", "Abbreviated Source Title", "Document Type", "Open Access")%>%
  mutate(across(everything(), tolower))%>%
  rename(Journal=`Source title`)

#add new variable 'york'
scopusCA$york<-grepl("york", scopusCA$`Correspondence Address`)

#list of publications related to TAs, filtered for only those made OA under TA Deal
TA <- read_xlsx(path = "OA_TA_publication_list.xlsx", sheet = "Articles", range = cell_cols("A:H"))%>%
  filter(`Made OA under deal?`=="Y")%>%
  mutate(across(everything(), tolower))%>%
  mutate(across(everything(), str_trim))

#find overlap between list of TA publications and scopus data based on DOI
linkDOI<-right_join(TA, scopusCA, by="DOI", suffix=c(".TA", ".scopus"))

TAprop<-as.data.frame(with(linkDOI, table(!is.na(Title.TA), Year, `Document Type`, york)))%>%
  rename(TA=Var1, `Publication Type`=Document.Type, `Number of Publications`=Freq)%>%
  filter(Year !="2023", `Publication Type`!="j. appl. econom.")%>%
  droplevels()

versionTA <- read_xlsx(path = "OA_TA_publication_list.xlsx", sheet = "Metadata", range = cell_cols("A"))

info_text<-HTML(paste("Data on open access formats (left) retrieved from Unpawall.com via SciVal. All publications affiliated with the University of York indexed on Scopus are included, data last updated ", version[,2], ". A short definition of the open access formats are below.<br/> Green = Self-archived in repository<br/> Gold = Available through fully open-access journal under creative commons licence (usually paid)<br/> Hybrid Gold = Option to publish open-access in a subscription journal (usually paid)<br/> Bronze = Free to read on the publisher page, but no clear license<br/> <br/>Data on transformative agreements (right) collected by the Open Research team (University of York), crosslinked with data from Scopus. Data last updated ",versionTA, ". Currently, only corresponding authors from the University of York can use our transformative agreements (see filter option). Correspondence address in Scopus was used as a proxy for corresponding author affiliation.<br/> <br/> If you have any questions, please contact the Open Research Team (lib-open-research@york.ac.uk).", sep=""))#create info text to be displayed in app  

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
        inputId = "pubtype",
        label = "Choose Publication Type", 
        choices = unique(OA1$`Publication Type`),
        selected = unique(OA1$`Publication Type`)),
      
      prettySwitch(
        inputId = "yorkCA",
        label = "TA: include only corresponding authors\nfrom York", 
        value = TRUE
      ),
      
      tags$style(type='text/css', css_slider), #add css style from above definition
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),#suppress minor ticks
      div(id = "customSlider",
        sliderInput(
          inputId = "year", 
          label = "Select Year", 
          min = 2017,
          max = 2022,
          value = 2020, 
          sep="",
          width='80%')),
      
      actionButton("show_help", "Further information")),
    
    # Main Panel with Plot and Table in Tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Visualisations",
                 fluidRow(
                   splitLayout(cellWidths = c("60%", "40%"), 
                               plotly::plotlyOutput('plot_OA'), 
                               plotly::plotlyOutput('plot_TA'))
                 )),
        tabPanel("Open Access Data",
                 DT::DTOutput('table_OA')),
        tabPanel("Transformative Agreement Data",
                 DT::DTOutput('table_TA'))
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
    subset(OA1, Year ==input$year & `Publication Type` %in% input$pubtype)%>%
      group_by(Year)%>%
      mutate(`Publication Volume per Year`=sum(`Number of Publications`), 
             `Proportion of all`=round(`Number of Publications`/`Publication Volume per Year`, digits=3))
    
  })
  rval_TAfiltered<-reactive({
    # Filter for the selected year and access (inputId in ui)
    subset(TAprop, Year ==input$year & `Publication Type` %in% tolower(input$pubtype))%>%
      filter(case_when(input$yorkCA==TRUE ~  york == TRUE,#filter york CA when input switch is 'TRUE'
                       input$yorkCA==FALSE ~ york == TRUE | york == FALSE))
  })
    
  #add OA plot
  output$plot_OA<-plotly::renderPlotly({
    # Plot selected year and access
    rval_OAfiltered() %>%
      #change order of OA levels for order of stacked bars
      mutate(`Open Access` = factor(`Open Access`, levels = c("Hybrid Gold", "Gold", "Green", "Bronze", "Not Open Access")))%>%
      #summarise the data
      mutate(`Access` = factor(`Access`))%>%
      group_by(`Access`, `Open Access`) %>%
      summarize(`Proportion of all` = sum(`Proportion of all`))%>%
      #plot the data
      ggplot(aes(x = `Access`, y = `Proportion of all`, fill=`Open Access`)) +
      geom_bar(color="black", stat="identity", size=0.1)+
      scale_fill_manual(values=c("khaki3","goldenrod", "palegreen4", "coral3", "gray50"))+
      scale_y_continuous(labels = scales::percent, limits=c(0,0.9))+
      labs(x="", y="Publications in selected year [%]", fill="Open Access Format")+
      theme_classic(base_size=12)
  })
  
  #add TA plot
  output$plot_TA<-plotly::renderPlotly({
    rval_TAfiltered()%>%
      mutate(TA = factor(TA, labels = c("Publication type (and <br>corresponding author<br>address) applicable to TA,<br><b>but no TA deal</b>", 
                                        "Open access<br><b>under TA Deal</b>")))%>%
      plot_ly(values=~`Number of Publications`,labels=~factor(TA),
                      marker = list(colors = c("#4D4D4D", "#CD9B1D"),
                                    line = list(color = "black", width = 0.5)),
                      type="pie", hole=0.3,
                      insidetextfont = list(color = '#FFFFFF')) %>% 
      layout(margin=list(l=100, r=100, b = 50, t = 180, pad = 0),
             legend=list(title=list(text="Transformative Agreement (TA)\n by University"), 
                         xanchor = "center", # use center of legend as anchor
                         x=0.5,
                         yanchor='top',
                         y=2), # put legend in center of x-axis and on top
             font=list(size = 12.5))
  })
      
  #add OA table
  output$table_OA <-  DT::renderDT({
    # Table of selected year and access
    rval_OAfiltered()
  }, rownames=FALSE)
  
  #add TA table
  output$table_TA <-  DT::renderDT({
    # Table of selected year and access
    rval_TAfiltered()%>%
      group_by(TA, `Publication Type`)%>%
      summarise(`Number of Publications`=sum(`Number of Publications`))
  }, rownames=FALSE)
}


#### APP - Pulls UI and Server together ####

shinyApp(ui = ui, server = server)#launch app

