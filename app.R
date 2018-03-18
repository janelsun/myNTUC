library(shiny)
library(DT)
library(shinyjs)
library(mongolite)
library(htmltools)
library(bootstrap)
library(readxl)
library(jsonlite)
library(xlsx)
library(rpart)
library(rpart.plot)
#library(rattle)
#library(RColorBrewer)
library(party)
library(partykit)
#library(GGally)
#library(network)
#library(sna)
#library(ggplot2)

source("daterangeinput_function.R")
source("grant_visualisation_function0.R")
source("grant_visualisation_function1.R")
source("grant_visualisation_function2.R")
source("htmlTemplate_usr.R")

# DB parameters
HOST = "localhost"
PORT = 27017
DB_NAME = "ODPRT"

# Basic authentication
Logged = TRUE;
my_username <- "test"
my_password <- "test"

# set maximum upload size to 300MB
options(shiny.maxRequestSize=300*1024^2)

# === Generic DB operations === #

# Connect to MongoDB
connect <- function(Collection){
  con <- mongo(Collection, url=paste("mongodb://", HOST, ":", PORT, "/", DB_NAME, sep=""))
  con
}

# save a list of NUS researchers
staff_names = connect("staff")$find()$NUS_Researcher

connect_researchers <- function(){
  con <- mongo("visTest", url=paste("mongodb://", HOST, ":", PORT, "/", DB_NAME, sep=""))
  con
}

saveData <- function(data, Collection) {
  connect(Collection)$insert(data)
}

loadData <- function(Collection, ...){
  connect(Collection)$find(...)
}

dropCollection <- function(Collection){
  # if collection exists
  if (nrow(connect(Collection)$find(limit=1)) > 0){
    connect(Collection)$drop()
  }
}

# === Specific queries === #

# loadData.researcher_name <- function(name){
#   connect("staff")$find(
#     query = paste0('{"NUS_Researcher":{"$regex":".*', name,'.*", "$options":"i"}}')
#   )
# }

loadUniData <- function(faculty) {
  connect("uni_collaboration")$find(query = paste('{"Faculty": "', faculty, '"}', sep=""))
}

loadFacData <- function(){
  connect("fac_collaboration")$find()
}

# Get uni_collaboration data
connect_uni_collaboration <- function(){
  con <- mongo("uni_collaboration", url=paste("mongodb://", HOST, ":", PORT, "/", DB_NAME, sep=""))
  con
}

# Get fac_collaboration data
connect_fac_collaboration <- function(){
  con <- mongo("fac_collaboration", url=paste("mongodb://", HOST, ":", PORT, "/", DB_NAME, sep=""))
  con
}

# DEPRECATED
# loadData_id <- function(id) {
#   connect("publications")$find(
#     query = paste('{"EID": "', id, '"}', sep=""),
#     fields = '{"Reference":false,"Publication-type":false,"Scopus affiliation names":false,"Scopus Source title":false,"_id":false,"EID":false}'
#   )
# }
# 
# 
# list2str <- function(lst){
#   paste0('["', do.call(paste, append(lst, list(sep='", "'))), '"]')
# }
# 
# loadData_id_lst <- function(id_lst,start,end) {
#   connect("publications")$find(
#     query = paste0('{"EID": {"$in": ', list2str(id_lst), '},"Year": {"$gt": ', start-1, '},"Year": {"$lt": ', end+1, '}}'),
#     fields = '{"Reference":false,"Publication-type":false,"Scopus affiliation names":false,"Scopus Source title":false,"_id":false,"EID":false}',
#     limit=5
#   )
# }

# loadData_download3 <- function(id_lst,start,end) {
#   connect("publications")$find(
#     query = paste0('{"EID": {"$in": ', list2str(id_lst), '},"Year": {"$gt": ', start-1, '},"Year": {"$lt": ', end+1, '}}')
#   )
# }
# 
# loadData_download4 <- function(id_lst) {
#   connect("publications")$find(
#     query = paste0('{"EID": {"$in": ', list2str(id_lst), '}}')
#   )
# }
# 
# loadData_download5 <- function(id_lst) {
#   connect("publications")$find(
#     query = paste0('{"EID": {"$in": ', list2str(id_lst), '}}'),
#     fields = '{"Reference":false,"Publication-type":false,"Scopus affiliation names":false,"Scopus Source title":false,"_id":false,"EID":false,"Country":false}'
#   )
# }

# === UI === #

login_page <- function(){
  ui <- fluidPage(
    ### Head Section ###
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
    ),
    
    ### Body Section ###
    tags$body(
      img(src="assets/NUS Logo.svg", id="logo"),
      navbarPage("")
    ),
    tagList(
      div(id = "login",
          wellPanel(textInput("userName", "Username"),
                    passwordInput("passwd", "Password"),
                    br(),actionButton("Login", "Log in"))),
      tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
    )
  )
}


logged_in <- function() {
  ui <- fluidPage(
    ### Head Section ###
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
      tags$script(src="/lib/d3.v4.3.0.js"),
      tags$script(src="/dist/dimple.v2.3.0.min.js")
    ),
    
    ### Body Section ###
    tags$body(
      img(src="assets/NUS Logo.svg", id="logo", onclick="$('[data-value=\"Home\"]').click()"),
      navbarPage("",
               tabPanel("Home", uiOutput('Home')),
               tabPanel("Collaboration Analysis", uiOutput('CollaborationAnalysis')),
               tabPanel("Grant Analysis", uiOutput('GrantAnalysis')),
               tabPanel("Export", uiOutput('Export')),
               tabPanel("Import", uiOutput('Import')),
               tabPanel("Help", uiOutput('Help')),
               tabPanel("About", uiOutput('About')),
               tabPanel("Logout", login_page()),
               id="navbar"
      )
    )
  )
}

ui <- htmlOutput("page")

home_page <- {
  fluidPage(
    htmlTemplate("www/templates/home.html"),
    htmlTemplate("www/templates/footer.html")
  )
}

collaboration_analysis_page <- {
  fluidPage(
    htmlTemplate("www/templates/collaboration/index.html",
                 ############ Cross Faculty Collaboration ############
                 network = plotOutput(outputId = "network"),
                 ############ Cross University Collaboration ############
                 faculty.input = selectInput("faculty", "Choose Faculty:",
                                             list("School of Design and Environment" = "DESIGN & ENVIRONMENT",
                                                  "Faculty of Engineering" = "ENGINEERING",
                                                  "Yong Loo Lin School of Medicine" = "YONG LOO LIN SCH OF MEDICINE",
                                                  "Business School" = "BUSINESS",
                                                  "Faculty of Science" = "SCIENCE",
                                                  "Faculty of Arts and Social Science" = "ARTS & SOC SC",
                                                  "School of Computing" = "COMPUTING"
                                             )
                 ),
                 
                 layer.input = selectInput("layer", "Choose Number of Layers:",
                                           list("2" = 2,
                                                "3" = 3,
                                                "4" = 4,
                                                "5" = 5,
                                                "6" = 6,
                                                "7" = 7
                                           )
                 ),
                 
                 out = plotOutput(outputId = "distPlot")
    ),
    
    htmlTemplate("www/templates/footer.html")
  )
}

# grant_analysis_page <- {
#   fluidPage(
#     htmlTemplate("www/templates/grant_vis/grant_individual.html",
#                  # researcherName = textInput(
#                  #   inputId = "researcherName",
#                  #   label = "Enter Staff Name: ",
#                  #   value = "",
#                  #   placeholder = "Please input the name of a NUS Researcher"
#                  # ),
#                  # researcherName_submit = actionButton(
#                  #   inputId = "researcherName_submit",
#                  #   label = "Submit"
#                  # ),
#                  researcherName = selectizeInput('researcherName', 'Choose a reseacher', choices = staff_names, options = list(create = TRUE)),
#                  
#                  researcherName_submit = actionButton(
#                    inputId = "researcherName_submit",
#                    label = "Submit"
#                  )
#                  
#                  #researcher.name.vis = {}
#     ),
#     #textOutput('researcher.name', inline=TRUE)),
#     htmlTemplate("www/templates/footer.html")
#   )
# }

grant_analysis_page <- {
  fluidPage(
    tabsetPanel(
      tabPanel("By Researcher", htmlTemplate("www/templates/grant_vis/grant_vis_1.html",
                                             researcherName = selectizeInput('researcherName', 'Choose a reseacher', choices = staff_names, options = list(create = TRUE)),
                                             
                                             researcherName_submit = actionButton(
                                               inputId = "researcherName_submit",
                                               label = "Submit"
                                             )
                                             
      )),
      
      tabPanel("By Faculty", htmlTemplate("www/templates/grant_vis/grant_vis_2.html")),
      id = "grant_vis_nav"
    ),
  
    
    htmlTemplate("www/templates/footer.html")
  )
}

# DEPRECATED
# export.html <- htmlTemplate.usr("www/templates/export.html", vars=list(
#   ############ Export Publications by names ############
#   #researcher.input = textInput("text", "Search", value = "Type in a staff's name..."),
#   #name.submit = actionButton("submit", "Submit"),
#   
#   researcher.input = selectizeInput('pubs_by_researcher_name', 'Choose a reseacher', choices = staff_names, options = list(create = TRUE)),
#   
#   table3 = DT::dataTableOutput('pubs_by_researcher'),
#   
#   # Export
#   name.export = downloadLink("export1", "Export"),
#   
#   ############ Export Productive Staff List ############
#   # Input Faculty
#   faculty.productive = selectInput("faculty1", "Choose Faculty:",
#                                    list("Saw Swee Hock School of Public Health" = "SSHSPH",
#                                         "School of Design and Environment" = "DESIGN & ENVIRONMENT",
#                                         "Faculty of Engineering" = "ENGINEERING",
#                                         "Yong Loo Lin School of Medicine" = "YONG LOO LIN SCH OF MEDICINE",
#                                         "Yong Siew Toh Conservatory of Music" = "YONG SIEW TOH CONSV OF MUSIC",
#                                         "Business School" = "BUSINESS",
#                                         "Faculty of Science" = "SCIENCE",
#                                         "Faculty of Arts and Social Science" = "ARTS & SOC SC",
#                                         "Yale-NUS College" = "YALE-NUS COLLEGE",
#                                         "Faculty of Law" = "LAW",
#                                         "School of Computing" = "COMPUTING",
#                                         "Faculty of Dentistry" = "DENTISTRY",
#                                         "University Scholars Programme (USP)" = "USP"
#                                    )
#   ),
#   
#   #Input Length
#   length.input = selectInput("length","Choose Length:",
#                              list("10" = 10,
#                                   "20" = 20,
#                                   "50" = 50)
#   ),
#   
#   table1 = DT::dataTableOutput('top_researchers'),
#   
#   # Export
#   staff.export = downloadLink("export2", "Export"),
#   
#   
#   ############ Export publications by faculty and year range ############
#   # Input: Faculty
#   faculty.publications = selectInput("faculty2", "Choose Faculty:",
#                                      list("Saw Swee Hock School of Public Health" = "SSHSPH",
#                                           "School of Design and Environment" = "DESIGN & ENVIRONMENT",
#                                           "Faculty of Engineering" = "ENGINEERING",
#                                           "Yong Loo Lin School of Medicine" = "YONG LOO LIN SCH OF MEDICINE",
#                                           "Yong Siew Toh Conservatory of Music" = "YONG SIEW TOH CONSV OF MUSIC",
#                                           "Business School" = "BUSINESS",
#                                           "Faculty of Science" = "SCIENCE",
#                                           "Faculty of Arts and Social Science" = "ARTS & SOC SC",
#                                           "Yale-NUS College" = "YALE-NUS COLLEGE",
#                                           "Faculty of Law" = "LAW",
#                                           "School of Computing" = "COMPUTING",
#                                           "Faculty of Dentistry" = "DENTISTRY",
#                                           "University Scholars Programme (USP)" = "USP"
#                                      )
#   ),
#   
#   table2 = DT::dataTableOutput('pubs_by_faculty'),
#   
#   # Input: Select Year Range
#   year.range3 = dateRangeYearsInput("date3", "Date range", start = 1950, end = Sys.Date()+3, 
#                                     min = "1970", max = Sys.Date()+3, format = "yyyy", startview = "year",weekstart = 1, language = "en", separator = " to "),
#   
#   
#   # Export
#   faculty.export = downloadLink("export3", "Export")
#   
# ))

export_page <- {
  fluidPage(
    tabsetPanel(
      tabPanel("Publications by Name", fluidPage(
        style = "padding: 55px; padding-top: 15px;",
        selectizeInput('pubs_by_researcher_name', 'Choose a reseacher', choices = staff_names, options = list(create = TRUE)),
        DT::dataTableOutput('pubs_by_researcher'),
        downloadLink("export1", "Export")
      )),
      
      tabPanel("Productive Staff List", fluidPage(
        style = "padding: 55px; padding-top: 15px;",
        selectInput("faculty1", "Choose Faculty:",
                                         list("Saw Swee Hock School of Public Health" = "SSHSPH",
                                              "School of Design and Environment" = "DESIGN & ENVIRONMENT",
                                              "Faculty of Engineering" = "ENGINEERING",
                                              "Yong Loo Lin School of Medicine" = "YONG LOO LIN SCH OF MEDICINE",
                                              "Yong Siew Toh Conservatory of Music" = "YONG SIEW TOH CONSV OF MUSIC",
                                              "Business School" = "BUSINESS",
                                              "Faculty of Science" = "SCIENCE",
                                              "Faculty of Arts and Social Science" = "ARTS & SOC SC",
                                              "Yale-NUS College" = "YALE-NUS COLLEGE",
                                              "Faculty of Law" = "LAW",
                                              "School of Computing" = "COMPUTING",
                                              "Faculty of Dentistry" = "DENTISTRY",
                                              "University Scholars Programme (USP)" = "USP"
                                         )
        ),
        
        #Input Length
        selectInput("length","Choose Length:",
                                   list("10" = 10,
                                        "20" = 20,
                                        "50" = 50)
        ),
        
        DT::dataTableOutput('top_researchers'),
        
        downloadLink("export2", "Export")
      )),
      
      tabPanel("Publications by Faculty", fluidPage(
        style = "padding: 55px; padding-top: 15px;",
        
        selectInput("faculty2", "Choose Faculty:",
                                           list("Saw Swee Hock School of Public Health" = "SSHSPH",
                                                "School of Design and Environment" = "DESIGN & ENVIRONMENT",
                                                "Faculty of Engineering" = "ENGINEERING",
                                                "Yong Loo Lin School of Medicine" = "YONG LOO LIN SCH OF MEDICINE",
                                                "Yong Siew Toh Conservatory of Music" = "YONG SIEW TOH CONSV OF MUSIC",
                                                "Business School" = "BUSINESS",
                                                "Faculty of Science" = "SCIENCE",
                                                "Faculty of Arts and Social Science" = "ARTS & SOC SC",
                                                "Yale-NUS College" = "YALE-NUS COLLEGE",
                                                "Faculty of Law" = "LAW",
                                                "School of Computing" = "COMPUTING",
                                                "Faculty of Dentistry" = "DENTISTRY",
                                                "University Scholars Programme (USP)" = "USP"
                                           )
        ),
        
        DT::dataTableOutput('pubs_by_faculty'),
        
        dateRangeYearsInput("date3", "Date range", start = 1950, end = Sys.Date()+3, 
                                          min = "1970", max = Sys.Date()+3, format = "yyyy", startview = "year",weekstart = 1, language = "en", separator = " to "),
        
        downloadLink("export3", "Export")
      ))
    ),
    
    htmlTemplate("www/templates/footer.html")
  )
}

import_page <- {
  fluidPage(
    htmlTemplate("www/templates/import.html",
                 ############ Import Single Entry ############
                 # *** Publications *** #
                 pubs.title = textInput("Title", "Title", ""),
                 pubs.authors = textInput("Authors", "Authors", ""),
                 pubs.num.authors = textInput("Number of Authors", "Number of Authors", ""),
                 pubs.year = textInput("Year", "Year", ""),
                 pubs.source.title = textInput("Scopus Source title", "Scopus Source title", ""),
                 pubs.sjr = textInput("SJR", "SJR", ""),
                 pubs.citations = textInput("Citations", "Citations", ""),
                 pubs.fwci = textInput("Field-Weighted Citation Impact", "Field-Weighted Citation Impact", ""),
                 pubs.reference = textInput("Reference", "Reference", ""),
                 pubs.pub.type = radioButtons("Publication-type", "Publication-type", choices = c("Article"="Article", "Conference Paper"="Conference Paper", "Book Chapter"="Book Chapter"), selected = "Article"),
                 pubs.eid = textInput("EID", "EID", ""),
                 pubs.instituitions = textInput("Institutions", "Institutions", ""),
                 pubs.affiliation = textInput("Scopus affiliation names", "Scopus affiliation names", ""),
                 pubs.country = textInput("Country", "Country", ""),
                 pubs.submit = actionButton("import", "Submit"),
                 # *** Staff Listing *** #
                 # *** Grant Information *** #
                 
                 ############ Batch Import ############
                 import.file = fileInput("import.file", "Choose Excel/JSON File",
                                             accept = c(".xlsx", ".xls", ".json"),
                                              buttonLabel="Browse", placeholder="or drag your file here"),
                 
                 # 
                 import.collection = radioButtons("import.collection", "Choose Collection",
                                                  choices = c(Publications = "Publications",
                                                              Staff = "Staff",
                                                              Grants = "Grants"),
                                                  selected = "Publications"),
                 
                 import.submit = actionButton("import.submit", "Finish")
                 
                 # *** Staff Listing *** #
                 # *** Grant Information *** #
    ),
    
    htmlTemplate("www/templates/footer.html")
  )
}

help_page <- {
  fluidPage(
    div("Opps, this page is currently under construction.", class="alert alert-primary"),
    htmlTemplate("www/templates/footer.html")
  )
}

about_page <- {
  fluidPage(
    tags$h2("About the ______"),
    br(),
    tags$p("Created by the ______ team, _____________ is an analytics-ready academic repository built on RShiny and supported with MongoDB. 
           This repository will store 3 different datasets namely:"),
    tags$ul(
      tags$li("Publications Data"),
      tags$li("Acad Listing"),
      tags$li("Grant Information")
    ),
    tags$p("The development of this system is meant to achieve the following goals:"),
    tags$ul(
      tags$li("To improve accuracy in data storage by storing all data in one avenue"),
      tags$li("To produce accurate and useful analytics, with the aid of visualisation, that can be used to tackle adhoc queries such as:"),
      tags$ul(
        tags$li("Does collaboration among NUS faculties or among NUS and other universities lead to higher research productivity?"),
        tags$li("How do the outcomes of grant applications affect the productivity of the researcher?")
      )
    ),
    tags$h2("Questions?"),
    tags$p(tags$span("Head over to the "), tags$a(onclick="$('[data-value=\"Help\"]').click();", href="#", "Help"), tags$span("page!")),  
    htmlTemplate("www/templates/footer.html")
  )
}


server = function(input, output, session) {
  
  # === AUTHENTICATION === #
  
  USER <- reactiveValues(Logged = Logged)
  
  observeEvent(input$navbar, {
    if (!is.null(input$navbar)){
      if (USER$Logged){
        if (input$navbar == "Logout"){
          USER$Logged <- F
        }
      }
    }
  })
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",login_page())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        div(class="outer",logged_in())
      })
    }
  })
  
  observeEvent(input$logout, {
    USER$Logged <- F
  })
  
  # === AUTHENTICATION END === #
  
  # === UI CONTROLLERS === #
  
  output$Home <- renderUI(home_page)
  output$CollaborationAnalysis <- renderUI(collaboration_analysis_page)
  output$CrossFaculty <- renderUI(cross_faculty_page)
  output$CrossUniversity <- renderUI(cross_university_page)
  output$GrantAnalysis <- renderUI(grant_analysis_page)
  output$Export <- renderUI(export_page)
  output$Import <- renderUI(import_page)
  output$Help <- renderUI(help_page)
  output$About <- renderUI(about_page)
  
  # === UI CONTROLLERS END === #
  
  # === Collaboration === #
  output$distPlot <- renderPlot({
    input$faculty
    print(input$faculty)
    fac_data = loadUniData(input$faculty)
    fwci = as.numeric(fac_data$FWCI)
    fac_data$FWCI = NULL
    model = rpart(fwci~., data = fac_data, control = rpart.control(maxsplit = 20, cp = 0))
    prp(model)
    len = as.integer(input$layer)
    #len = 2^(len)
    index = c(2^len:2^(len+1))
    #print(len)
    model2 = snip.rpart(model,toss=index)
    prp(model2)
    fancyRpartPlot(model2, sub = "Cross University Collaboration Decision Tree")
  })
  
  output$network <- renderPlot({
    if(input$navbar=="Collaboration Analysis"){
      faculty = loadFacData()
      faculty = faculty[, colSums(faculty != 0) > 0]
      fwci = as.numeric(faculty$FWCI)
      faculty$FWCI = NULL
      model = lm(fwci~., data = faculty)
      coeff = model$coefficients
      coeff = as.data.frame(coeff)
      #fac_list = rownames(coeff)
      faculty_list = character()
      num_of_publications = numeric()
      header = colnames(faculty)
      for (elem in header){
        if(grepl(".and.", elem) == F){
          faculty_list = c(faculty_list, elem)
        }
      }
      for (fac in faculty_list){
        pub = sum(faculty[[fac]])
        num_of_publications = c(num_of_publications, pub)
      }
      
      
      
      adj_matrix = data.frame(matrix(0, ncol = length(faculty_list), nrow = length(faculty_list)),row.names = faculty_list)
      
      colnames(adj_matrix) = faculty_list
      pairs = rownames(coeff)
      for (pair in pairs){
        if(grepl(" and ", pair)){
          fac_pair = strsplit(pair, " and ")
          coefficient = round(coeff[c(pair),],2)
          fac_1 = substr(fac_pair[[1]][[1]], start=2,stop=nchar(fac_pair[[1]][[1]]))
          fac_2 = substr(fac_pair[[1]][[2]], start=1, stop=nchar(fac_pair[[1]][[2]])-1)
          adj_matrix[c(fac_1), c(fac_2)] = abs(coefficient)
          adj_matrix[c(fac_2), c(fac_1)] = abs(coefficient)
        }
      }
      
      network = network(adj_matrix,
                        matrix.type = "adjacency",
                        ignore.eval = FALSE,
                        names.eval = "weights")
      output = ggnet2(network, color = "#66ffff", size = num_of_publications,max_size = 40,label = TRUE, label.size = 7,edge.label = "weights", edge.size=1, edge.color = "#3E588F",edge.label.size = 6, legend.position = 'none')
      plot(output)
      
    }
  })
  # === Collaboration END === #
  
  # === Grants === #
  # vis 1
  observeEvent(
    eventExpr <- input[["researcherName_submit"]],
    handleExpr <- {
      researcherName <- (input$researcherName)
      final_output_json <- grant_visualisation_function1(researcherName) #json file to send into d3.js
      print(final_output_json);
      final_output_json3 <- grant_visualisation_function0(researcherName)
      session$sendCustomMessage(type="jsondata", final_output_json)
      session$sendCustomMessage(type="jsondata3", final_output_json3)
    }
  )
  
  # vis 2
  observeEvent(input$grant_vis_nav,{
    if (!is.null(input$grant_vis_nav)){
      if (USER$Logged){
        if (input$grant_vis_nav == "By Faculty"){
          final_output_json2 <- grant_visualisation_function2()
          session$sendCustomMessage(type="jsondata2", final_output_json2)
        }
      }
    }
  })
  # === Grants END === #
  
  # === EXPORT === #
  
  # Reactive values to store dataframes for export
  EXPORT  <- reactiveValues(
    pubs_by_researcher = NULL,
    top_researchers = NULL,
    pubs_by_faculty = NULL
  )
  
  # ========= #
  
  pubs_by_researcher_input <- reactive({
    df <- connect('publications')$find(
      query = paste0('{"NUS Researcher": "', input$pubs_by_researcher_name , '"}'),
      fields = '{"_id": false}'
    )
    
    EXPORT$pubs_by_researcher <- df[df$`NUS Researcher` == input$pubs_by_researcher_name, ]
    EXPORT$pubs_by_researcher
  })
  
  output$pubs_by_researcher <- DT::renderDataTable({
    datatable(pubs_by_researcher_input(), filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE
    ))
  })
  
  # ========= #
  
  top_researchers_input <- reactive({
    df <- connect("staff")$find(
      query = paste('{"Faculty": "', input$faculty1, '"}', sep=""),
      field = '{"Publications_EIDs_List": false,"_id":false}',
      limit = as.integer(input$length),
      sort = '{"H-index": -1}'
    )
    EXPORT$top_researchers <- df
    EXPORT$top_researchers
  })
  
  output$top_researchers <- DT::renderDataTable({
    datatable(top_researchers_input(), filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE
    ))
  })
  
  # ========= #
  
  pubs_by_faculty_input <- reactive({
    df <- connect('publications')$find(
      query = paste0('{"Faculty": "', input$faculty2, '"}'),
      fields = '{"_id": false, "NUS Researcher": false}'
    )
    
    EXPORT$pubs_by_faculty <- df[!duplicated(df[c('EID')]),]
    EXPORT$pubs_by_faculty
  })
  
  output$pubs_by_faculty <- DT::renderDataTable({
    datatable(pubs_by_faculty_input(), filter = 'top', options = list(
      pageLength = 5, autoWidth = TRUE
    ))
  })
  
  # ========= #
  
  output$export1 <- downloadHandler(
    filename = function(){
      paste0(input$pubs_by_researcher_name, ".xlsx")
    },
    content = function(file){
      write.xlsx(EXPORT$pubs_by_researcher,file)
    }
  )
  
  output$export2 <- downloadHandler(
    filename = function(){
      paste0(input$faculty1, "_staff.xlsx")
    },
    content = function(file){
      write.xlsx(EXPORT$top_researchers, file)
    }
  )
  
  output$export3 <- downloadHandler(
    filename = function(){
      paste0(input$faculty2, ".xlsx")
    },
    content = function(file){
      write.xlsx(EXPORT$pubs_by_faculty, file)
    }
  )
  
  # === EXPORT END === #
  
  # === Import === #
  observeEvent(input$import.submit, {
    req(input$import.file, input$import.collection)
    
    session$sendCustomMessage(type = 'showimportloader', message = list())
    
    if (grepl(".json",input$import.file$name)){
      data <- fromJSON(input$import.file$datapath, flatten=TRUE)
    } else {
      data <- read_excel(input$import.file$datapath)
    }
    
    dropCollection(tolower(input$import.collection))
    
    if (input$import.collection == "Grants"){
      data[['_id']] = data[['Proposal ID']]
    }
    
    saveData(data, tolower(input$import.collection))
    
    output$Import <- renderUI(import_page)
  })
  
  # === Import END === #
  
  # === IMPORT SINGLE ENTRY - TODO === #
  formData <- reactive({
    fields <- c("Title", "Authors", "Number of Authors", "Year", "Scopus Source title", "SJR", "Citations", "Field-Weighted Citation Impact", "Reference", "Publication-type", "EID", "Institutions", "Scopus affiliation names", "Country")
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  observeEvent(input$import, {
    #saveData(as.data.frame(t(formData())))
  })
  
  # === IMPORT SINGLE ENTRY END === #
}


shinyApp(ui = ui, server = server)