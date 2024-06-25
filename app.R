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

library(data.table)# fast aggregation of large data

#### LOAD AND PREPARE DATA ####

## Open Access data ##
#combine all files that start with 'scopusUoY' and end with 'updated2024.csv'
scopusCA<-list.files(path=".", pattern="^scopusUoY.*updated2024.csv$", recursive = T) %>%
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows %>%
  #select("Title", "Year", "Source title", "DOI", "Correspondence Address", "Publisher", "Abbreviated Source Title", "Document Type", "Open Access")%>%
  mutate(across(everything(), tolower))%>%
  rename(Journal=`Source title`)

#add new variable 'york' based on corresponding author address
scopusCA$york<-grepl("york", scopusCA$`Correspondence Address`)

scopusCA$`Open Access`[is.na(scopusCA$`Open Access`)]<-"Not Open Access"

scopusCA$`Open Access`<-as.factor(scopusCA$`Open Access`)

levels(scopusCA$`Open Access`)<-c("Not Open Access", "Green", "Gold", "Gold", "Green", "Hybrid Gold", "Hybrid Gold", "Not Open Access")

OAscopus<-scopusCA %>%
  #create summary stats by year and OA format
  rename(`Publication Type` = `Document Type`)%>%
  group_by(Year, `Open Access`, `Publication Type`, york)%>%
  summarise(`Number of Publications`=n())


## Transitional agreements, YOAF and corresponding author data ## 

#list of publications related to TAs, filtered for only those made OA under TA Deal
TA <- read_xlsx(path = "OA_TA_publication_list2024.xlsx", sheet = "Articles", range = cell_cols("A:H"))%>%
  filter(`Made OA under deal?`=="Y")%>%
  mutate(across(everything(), tolower))%>%
  mutate(across(everything(), str_trim))%>%#removes white space from start and end of string
  mutate(TA="TA")

#list of publications related to York Open Access fund
YOAF<-read.csv("YOAF_publications_Jan_2016-Jun_2024.csv")[,2:8]%>%
  mutate(across(everything(), tolower))%>%
  mutate(across(everything(), str_trim))%>%#removes white space from start and end of string
  mutate(YOAF="YOAF")

#find overlap between list of TA publications, YOAF publications and scopus data based on DOI
if (file.exists("TAYOAF_OAformat.csv")) {
  TAYOAFprop<-read.csv("TAYOAF_OAformat.csv")%>%
    mutate(`Publication Type`=as.factor(Publication.Type), `Open Access`=factor(Open.Access, levels=c("Not Open Access", "Green", "Gold", "Hybrid Gold")), york=as.factor(york), Year=as.factor(Year), `Number of Publications`=Number.of.Publications)
  
} else {
  #run lines within else {} and write TAYOAFprop to csv with write.csv(TAYOAFprop, "TAYOAF_OAformat.csv", rownames=F) whenever changes to scopus data, TA or YOAF data are made
  linkDOI<-scopusCA%>%
    left_join(TA, by="DOI", suffix=c(".scopus", ".TA"))%>%
    left_join(YOAF, by="DOI", suffix=c(".scopus", "YOAF"))

  #error in stringdist when NA in column that is to be matched
  linkDOI$Journal.scopus[is.na(linkDOI$Journal.scopus)]<-"empty_string"

  #overlap based on title-journal combo wherever DOI matching failed - 5 char difference   allowed (fuzzy match)
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

  #move TA and YOAF publications marked as green or closed to hybrid gold (unpaywall data wrong)
  linkall<-linkall%>%
    mutate(`Open Access`=case_when(`Open Access.x`=="Green" & TA1=="TA" ~ "Hybrid Gold",
                                   `Open Access.x`=="Green" & YOAF1=="YOAF" ~ "Hybrid Gold",
                                   `Open Access.x`=="Not Open Access" & TA1=="TA" ~ "Hybrid Gold",
                                   `Open Access.x`=="Not Open Access" & YOAF1=="YOAF" ~ "Hybrid Gold",
                                   TRUE ~ `Open Access.x`))#else keep value

  #calculate number of publications per Year, Document Type, OA route etc.
  TAYOAFprop<-as.data.frame(with(linkall, table(TA1, YOAF1, Year.x, `Document Type.x`, york.x, `Open Access`)))%>%
    rename(TA=TA1,YOAF=YOAF1, `Publication Type`=Document.Type.x, `Number of Publications`=Freq, york=york.x, Year=Year.x, `Open Access`=Open.Access)%>%
    unite(., col="Route", TA, YOAF, na.rm=T, remove=T, sep="")%>%
    filter(Year !="2024", Route!="TAYOAF")%>%
    droplevels()

TAYOAFprop$Route[TAYOAFprop$Route==""]<-"other"

write.csv(TAYOAFprop, "TAYOAF_OAformat.csv", row.names = F)
}

versionTA <- read_xlsx(path = "OA_TA_publication_list2024.xlsx", sheet = "Metadata", range = cell_cols("A"))

info_text<-HTML(paste0("Data on open access formats (left) retrieved from Unpawall.com via Scopus. All publications affiliated with the University of York indexed on Scopus are included, data last updated 24 June 2024. A short definition of the open access formats are below.<br/> <br/> Green = Self-archived in repository<br/> Gold = Available through fully open-access journal under creative commons licence (usually paid)<br/> Hybrid Gold = Option to publish open-access in a subscription journal (usually paid)<br/> Bronze = Free to read on the publisher page, but no clear license<br/> <br/>Data on transformative agreements and York Open Access Fund (right) are collected by the Open Research team (University of York) and enriched with data from Scopus. Data last updated ",versionTA$`Data last updated`, ". Currently, only corresponding authors from the University of York can use our transformative agreements (see filter option). Correspondence address in Scopus was used as a proxy for corresponding author affiliation.<br/> <br/> Please <a href='mailto:lib-open-research@york.ac.uk'> let us know (lib-open-research@york.ac.uk)</a> how you are using the visualisations and data. All data and code is available in our <a href='https://github.com/openresearchyork/openresearchyork_dashboard'> github repository</a>.", sep=""))#create info text to be displayed in app  

#function definition: transform data to expected format for sunburst plot
as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "UoY\nPublications"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]

  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }

  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}

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
          max = 2023,
          value = 2022, 
          sep="",
          width='80%')),
      
      actionButton("show_help", "Further information")),
    
    # Main Panel with Plot and Table in Tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Visualisations",
                 fluidRow(
                   tags$head(
                     tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:20px;
                margin-top:10px;
                font-size:13px;
                line-height:20px;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0);
                border-radius:4px;
                color:#4D4D4D;
                font-family: var(--bs-body-font-family);
            }"))),
                   splitLayout(cellWidths = c("70%", "30%"), 
                               plotly::plotlyOutput('plot_OA'), 
                               code("Publications are made open access in a variety of formats (Gold, Hybrid Gold and Green, see also 'further information' button on left). Many of our publications are made open access through:", span(style = "color:black", tags$a(href="https://www.york.ac.uk/library/research-creativity/open-access/","York Open Access Fund (YOAF) and transformative open access publishing agreements (TA).")))
                               ),
                 )),
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
  
  rval_TAYOAFfiltered<-reactive({
    # Filter for the selected year and access (inputId in ui)
    subset(TAYOAFprop, Year ==input$year & str_to_title(`Publication Type`) %in% input$pubtype)%>%
    filter(case_when(input$yorkCA==TRUE ~  york == TRUE,#filter york CA when input switch is 'TRUE'
                       input$yorkCA==FALSE ~ york == TRUE | york == FALSE))
  })
  
  rval_TAYOAFsunburstfiltered<-reactive({
    rval_TAYOAFfiltered()%>%
      group_by(`Open Access`, Route)%>%
      summarise(`Number of Publications`=sum(`Number of Publications`))
  })
    
  #add OA plot
  output$plot_OA<-plotly::renderPlotly({
    # Plot selected year and access
      plot_ly(data =   as.sunburstDF(rval_TAYOAFsunburstfiltered(), value_column = "Number of Publications", add_root=TRUE), 
              ids = ~ids, labels= ~labels, parents = ~parents, values= ~values, 
              marker = list(colors = c("#FFFFFF", "#4D4D4D", "#548b54", "#CD9B1D", "#cdc673")),
              type='sunburst', branchvalues = 'total', sort=FALSE, rotation=180)
  })
      
  #add OA table
  output$table_OA <-  DT::renderDT(
    # Table of selected year and access
    DT::datatable(
      {rval_TAYOAFfiltered()%>%
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

