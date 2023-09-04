#### LOAD PACKAGES ####

#install all packages when using R for first time (uncomment below)
#install.packages("readxl", "ggplot2", "tidyverse", "shiny", "DT", "plotly", "shinythemes", "shinyWidgets")

library(readxl)#read excel files

library(ggplot2)#pretty (static) plots

library(tidyverse)#handy data manipulation tools
options(dplyr.summarise.inform = FALSE)#suppress messages to console from summarise function

library(fuzzyjoin)#stringdist fuzzy join

library(shiny)#web applications

library(DT)#interactive tables

library(plotly)#interactive plots

library(shinythemes)#layout/themes for app

library(shinyWidgets)#fancy interactive buttons

library(ggalluvial)# for alluvial ggplots

#### LOAD AND PREPARE DATA ####

## Open Access data ##
scopusCA<-list.files(path=".", pattern="^scopusUoY", recursive = T) %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows %>%
  #select("Title", "Year", "Source title", "DOI", "Correspondence Address", "Publisher", "Abbreviated Source Title", "Document Type", "Open Access")%>%
  mutate(across(everything(), tolower))%>%
  rename(Journal=`Source title`)

#add new variable 'york' based on corresponding author address
scopusCA$york<-grepl("york", scopusCA$`Correspondence Address`)

scopusCA$`Open Access`[is.na(scopusCA$`Open Access`)]<-"Not Open Access"

scopusCA$`Open Access`<-as.factor(scopusCA$`Open Access`)

levels(scopusCA$`Open Access`)<-c("Bronze", "Green", "Gold", "Gold", "Green", "Hybrid Gold", "Hybrid Gold", "Not Open Access")

scopusCA$Access<-ifelse(scopusCA$`Open Access`=="Not Open Access" | scopusCA$`Open Access`=="Bronze", "Closed Access", "Open Access")#create a new variable that groups into 'closed' and 'open access'

OAscopus<-scopusCA %>%
  #create summary stats by year and OA format
  rename(`Publication Type` = `Document Type`)%>%
  group_by(Year, `Open Access`, `Access`, `Publication Type`, york)%>%
  summarise(`Number of Publications`=n())


## Transitional agreements, YOAF and corresponding author data ## 

#list of publications related to TAs, filtered for only those made OA under TA Deal
TA <- read_xlsx(path = "OA_TA_publication_list.xlsx", sheet = "Articles", range = cell_cols("A:H"))%>%
  filter(`Made OA under deal?`=="Y")%>%
  mutate(across(everything(), tolower))%>%
  mutate(across(everything(), str_trim))%>%#removes white space from start and end of string
  mutate(TA="TA")

#list of publications related to York Open Access fund
YOAF<-read.csv("YOAF_publications_202308.csv")[,2:8]%>%
  mutate(across(everything(), tolower))%>%
  mutate(across(everything(), str_trim))%>%#removes white space from start and end of string
  mutate(YOAF="YOAF")

#find overlap between list of TA publications, YOAF publications and scopus data based on DOI
linkDOI<-scopusCA%>%
  left_join(TA, by="DOI", suffix=c(".scopus", ".TA"))%>%
  left_join(YOAF, by="DOI", suffix=c(".scopus", "YOAF"))

#error in stringdist when NA in column that is to be matched
linkDOI$Journal.scopus[is.na(linkDOI$Journal.scopus)]<-"empty_string"

#overlap based on title-journal combo wherever DOI matching failed - 5 char difference allowed (fuzzy match)
linktitle<-linkDOI%>%
  filter(is.na(TA) & is.na(YOAF))%>%
  stringdist_inner_join(YOAF,
                        by =c('Title.scopus'='Title.of.article', 
                              'Journal.scopus'='Journal'), 
                        max_dist=5, ignore_case=TRUE)%>%
  stringdist_left_join(TA,
                       by =c('Title.scopus'='Title', 
                             'Journal.scopus'='Journal'), 
                       max_dist=5, ignore_case=TRUE)

#combine DOI matched and journal-title matched publication lists
linkall<-linkDOI%>%
  left_join(linktitle, by=c("Title.scopus", "Journal.scopus"))%>%
  unite(., col="YOAF1", YOAF, YOAF.x, YOAF.y, na.rm=T, remove=T)%>%
  unite(., col="TA1", TA, TA.x, TA.y, na.rm=T, remove=T)

#calculate number of publications per Year, Document Type, OA route etc.
TAYOAFprop<-as.data.frame(with(linkall, table(TA1, YOAF1, Year.x, `Document Type.x`, york.x, `Open Access.x`)))%>%
  rename(TA=TA1,YOAF=YOAF1, `Publication Type`=Document.Type.x, `Number of Publications`=Freq, york=york.x, Year=Year.x, `Open Access`=Open.Access.x)%>%
  unite(., col="Route", TA, YOAF, na.rm=T, remove=T, sep="")%>%
  filter(Year !="2023", `Publication Type`!="j. appl. econom.", Route!="TAYOAF")%>%
  droplevels()

TAYOAFprop$Route[TAYOAFprop$Route==""]<-"other"

versionTA <- read_xlsx(path = "OA_TA_publication_list.xlsx", sheet = "Metadata", range = cell_cols("A"))

info_text<-HTML(paste("Data on open access formats (left) retrieved from Unpawall.com via Scopus. All publications affiliated with the University of York indexed on Scopus are included, data last updated 31 August 2023. A short definition of the open access formats are below.<br/> <br/> Green = Self-archived in repository<br/> Gold = Available through fully open-access journal under creative commons licence (usually paid)<br/> Hybrid Gold = Option to publish open-access in a subscription journal (usually paid)<br/> Bronze = Free to read on the publisher page, but no clear license<br/> <br/>Data on transformative agreements and York Open Access Fund (right) are collected by the Open Research team (University of York) and enriched with data from Scopus. Data last updated ",versionTA, ". Currently, only corresponding authors from the University of York can use our transformative agreements (see filter option). Correspondence address in Scopus was used as a proxy for corresponding author affiliation.<br/> <br/> Please <a href='mailto:lib-open-research@york.ac.uk'> let us know (lib-open-research@york.ac.uk)</a> how you are using the visualisations and data. All data and code is available in our <a href='https://github.com/openresearchyork/openresearchyork_dashboard'> github repository</a>.", sep=""))#create info text to be displayed in app  

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
        choices = unique(str_to_title(OAscopus$`Publication Type`)),
        selected = 'Article'),
      
      prettySwitch(
        inputId = "yorkCA",
        label = "Only publications with corresponding authors from UoY", 
        value = FALSE
      ),
      
      h5("(Our transformative open access publishing agreements are only available for corresponding authors from UoY)"),
      
      tags$style(type='text/css', css_slider), #add css style from above definition
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),#suppress minor ticks
      div(id = "customSlider",
          sliderInput(
            inputId = "year", 
            label = "Select Year", 
            min = 2017,
            max = 2022,
            value = 2022, 
            sep="",
            width='80%')),
      
      actionButton("show_help", "Further information")),
    
    # Main Panel with Plot and Table in Tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Visualisations",
                 plotOutput('plot_OA')),
        tabPanel("Open Access Format Data",
                 DT::DTOutput('table_OA')),
        tabPanel("Open Access Route Data",
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
    subset(OAscopus, Year ==input$year & str_to_title(`Publication Type`) %in% input$pubtype)%>%
      group_by(Year)%>%
      filter(case_when(input$yorkCA==TRUE ~  york == TRUE,#filter york CA when input switch is 'TRUE'
                       input$yorkCA==FALSE ~ york == TRUE | york == FALSE))%>%
      mutate(`Publication Volume per Year`=sum(`Number of Publications`), 
             `Proportion of all`=round(`Number of Publications`/`Publication Volume per Year`, digits=3))
  })
  rval_TAYOAFfiltered<-reactive({
    # Filter for the selected year and access (inputId in ui)
    subset(TAYOAFprop, Year ==input$year & str_to_title(`Publication Type`) %in% input$pubtype)%>%
      filter(case_when(input$yorkCA==TRUE ~  york == TRUE,#filter york CA when input switch is 'TRUE'
                       input$yorkCA==FALSE ~ york == TRUE | york == FALSE))
  })
  
  #add OA plot
  output$plot_OA<-renderPlot({
    # Plot selected year and access
    rval_TAYOAFfiltered() %>%
      #change order of OA levels for order of stacked bars
      group_by(`Open Access`, Route)%>%
      summarise(`Number of Publications`=sum(`Number of Publications`))%>%
      mutate(`Open Access` = factor(`Open Access`, levels = c("Hybrid Gold", "Gold", "Green", "Bronze", "Not Open Access")),
             Route = factor(Route, levels = c("YOAF", "TA", "other")))%>%
      ggplot(aes(axis1=`Open Access`, axis2=Route, y=`Number of Publications`))+
      geom_alluvium(aes(fill=`Open Access`))+
      geom_stratum(aes(fill=`Open Access`))+
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c("Open Access Format", "Open Access Route"), expand = c(.05, .05)) +
      scale_fill_manual(values=c("coral3", "goldenrod", "palegreen4", "khaki3", "lightgray"))+
      theme_minimal(base_size = 16)+
      theme(legend.position = "none")
  })
  
  #add OA table
  output$table_OA <-  DT::renderDT(
    # Table of selected year and access
    DT::datatable(
      {rval_OAfiltered()%>%
          group_by(`Open Access`, `Publication Type`)%>%
          summarise(`Number of Publications`=sum(`Number of Publications`))},
      extensions = 'Buttons',
      
      options = list(
        paging = FALSE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      
      class = "display", 
      rownames = FALSE
    ))
  
  #add TA table
  output$table_TA <-  DT::renderDT(
    # Table of selected year and access
    DT::datatable(
      {rval_TAYOAFfiltered()%>%
          group_by(Route, `Publication Type`)%>%
          summarise(`Number of Publications`=sum(`Number of Publications`))},
      extensions = 'Buttons',
      
      options = list(
        paging = FALSE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      
      class = "display", 
      rownames = FALSE
    ))
}


#### APP - Pulls UI and Server together ####

shinyApp(ui = ui, server = server)#launch app
