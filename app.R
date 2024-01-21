#how to deploy
#rsconnect::deployApp('C:/Users/Adil/Documents/Rfiles/Shiny/bcom4933')
#authentication code was adopted from http://stackoverflow.com/questions/28987622/starting-shiny-app-after-password-input by Pork Chop

rm(list = ls())
Logged = FALSE;
my_username <- c("test2016","admin1","admin2")
my_password <- c("test2016","admin1","admin2")
ui1 <- function(){
  tagList(
    
    div(id = "login",
    
          box(title = "Please Enter Username & Password", background ="aqua", status = "primary", solidHeader = TRUE, width = 12,
          textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in")
        )  #closing box
          ),
        
    tags$style(type="text/css", "#login {font-size:16px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  
  )
  }

ui2 <- function(){tagList(tabItem("Results"),
                          
                          tags$h4(icon("user", lib="glyphicon"),tags$a("You have been logged in successfully to the admin area")),
                          tags$h3("Please wait for the server to load Results....Please Be Patient"),
                          
                          #general Stat
                          box(width = 12, height = NULL,
                              fluidRow(
                                infoBoxOutput("size"),
                                infoBoxOutput("fema"),
                                infoBoxOutput("male")
                                
                              )),
                          box(title = "Distribution of Education by Gender", status = "primary", solidHeader = TRUE, width = 6, height = NULL,
                              plotOutput("responsesGender")),
                          
                          box(title = "Distribution of Income by Gender", status = "success", solidHeader = TRUE, width = 6, height = NULL,
                              plotOutput("responsesAge2")),
                          
                          box(title = "Relationship between Income and Age on the Success of Tanfeendh", status = "warning", solidHeader = TRUE, width = 6, height = NULL,  
                              plotOutput("overco")),
                          
                          box(title = "Age Difference towards Tanfeedh", status = "danger", solidHeader = TRUE, width = 6, height = NULL,
                              plotOutput("dens")),
                          
                          box(title = "The Success/Failure of Tanfeedh", status = "primary", solidHeader = TRUE, width = 6, height = NULL,
                              plotOutput("overco2")),
                          
                          box(title = "How many Individuals are Familar", status = "warning", solidHeader = TRUE, width = 6, height = NULL,
                              plotOutput("familiar")),
                          
                          box(title = "Are Well Educated Individuals more familiar with Tanfeedh", status = "info", solidHeader = TRUE, width = 6, height = NULL,
                              plotOutput("circular2")),
                          
                          downloadButton("downloadBtn", "Download responses"),
                          
                          DT::dataTableOutput("responsesTable")
                          
                          
                          )}
#################

library(shiny)
library(shinydashboard)
library(DT)
library(shiny)
library(shinyjs)
library(dplyr)
library(digest)
library(ggplot2)
library(googlesheets)
library(gsheet)
library(fmsb)
library(circlize)

#-----Global Functions----#

#the following function makes some items mandatory
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#the following CSS gives * of mandatory items a red color
appCSS <-
  ".mandatory_star { color: red; }
#error { color: red; }"

#The following defines mandatory items
fieldsMandatory <- c("gender", "age") #to make mandatory fields

#the following defines items to be saved, in which directory, and a function of timestap var
fieldsAll <- c("gender", "age", "age2","income","education", "employment",
               "aware","overcome","company","solution1","solution2",
               "solution3","solution4")

epochTime <- function() {
  as.integer(Sys.time())
}

#the following will save data in googlesheets
table <- "responses"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
  #Sys.sleep(.3)
}

#the following will load data
loadData <- function() {
  # Grab the Google Sheet
  #sheet <- gs_title(table)
  # Read the data
  #gs_read_csv(sheet)
  #as.data.frame(gs_read_csv(sheet))
  mydata <- gsheet2tbl('docs.google.com/spreadsheets/d/1m_Y2v8n2xtyHiFYLOM0L_6Y5Qpac-At3h-FZdpcVvFw')
  mydata2 <- as.data.frame(mydata)
}

##humantime so we if 2 submit sametime we don't lose data
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
#-----End of Global functions---#

header <-  dashboardHeader(title = "Questionnaire")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    h4(menuItem("Questionnaire", tabName = "survey",icon = icon("pencil", lib="glyphicon"))), 
    
    h4(menuItem("Results", tabName = "results",icon = icon("stats", lib="glyphicon"))), 
    
    h4(menuItem("About Me", tabName = "about", icon = icon("user", lib="glyphicon")))
  )
)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
body <- dashboardBody(
  #~~~~~~~~~~~~~~~~~~~~~~~~~~  
  tabItems( #opening tabItems
    
    ################################## 
    #-----Tab1 Questionnaire------#
    ######################################
    
    
    
    tabItem(tabName = "survey",
            fluidRow(
              box(title = "Questionnaire Starts Here", status = "success", background= "navy", solidHeader = TRUE, width = 8, height = NULL,       
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  h2(titlePanel("The National Program for Enhancing Economic Diversification (Tanfeedh)")),
                  #DT::dataTableOutput("responsesTable"),
                  
                  div(
                    id = "form",
                    
                    radioButtons("gender", inline=TRUE,selected = character(0),labelMandatory("Your Gender:"),
                                 c("Male","Female")),
                    
                    radioButtons("age",selected = character(0), inline=TRUE, labelMandatory("Your Age:"),
                                 c("18-24 Years Old",
                                   "25-34 Years Old",
                                   "35-44 Years Old",
                                   "45-54 Years Old",
                                   "55-64 Years Old",
                                   "65 Years or Older")),
                    
                    sliderInput("age2", "Your Actual Age", min=0, max= 80, 25, ticks = TRUE),
                    sliderInput("income", "Your Income in Omani Rial", min=0, max= 5000, 700, ticks = TRUE),
                    
                    
                    radioButtons("education",selected = character(0), "Highest Level of Education:",
                                 c("High School",
                                   "Technical/Vocational Training",
                                   "College Diploma-2 Year",
                                   "Bachelor's Degree",
                                   "Master's Degree",
                                   "PhD/DBA")),
                    
                    radioButtons("employment", selected = character(0),"Employment Status:",
                                 c("A Student",
                                   "A Job Seeker",
                                   "Employed-Private Sector",
                                   "Employed-Public Sector",
                                   "Employed-Public/Private Sector",
                                   "Self Employed/Business Owner")),
                    
                    
                    radioButtons("aware", inline=FALSE,selected = character(0),"I am ...... with Tanfeedh Program:",
                                 c("Not at All Familiar",
                                   "Slightly Familiar",
                                   "Somewhat Familiar",
                                   "Moderately Familiar",
                                   "Extremely Familar")),
                    
                    radioButtons("overcome", inline=FALSE,selected = character(0),"Tanfeed will overcome the obstacles facing economic diversification",
                                 c("Extremely Unlikely",
                                   "Unlikely",
                                   "Nueutral",
                                   "Likely",
                                   "Extremely Likely")),
                    
                    
                    textInput("company", "Number of Companies You Own", ""),
                    
                    tags$b("The government should do the following to make Oman great again:"),
                    checkboxInput("solution1", "Top officials should be banned from doing local business", FALSE),
                    checkboxInput("solution2", "Top officials should not have multiple leadership roles", FALSE),
                    checkboxInput("solution3", "Top officials should be routinley replaced", FALSE),
                    checkboxInput("solution4", "Top officials should all open channels of communication with publics", FALSE),
                    
                    actionButton("submit", "Submit", class = "btn-primary"),
                    
                    shinyjs::hidden(
                      span(id = "submit_msg", "Submitting..."),
                      div(id = "error",
                          div(br(), tags$b("Error: "), span(id = "error_msg"))
                      )
                    )
                  ),
                  
                  #the following for thank you message
                  shinyjs::hidden(
                    div(
                      id = "thankyou_msg",
                      h3("Thanks, your response was submitted successfully!"),
                      actionLink("submit_another", "Submit another response")
                    )
                  ) 
                  
              ))),
    
    
    ################################## 
    #-----Tab2 Results------#
    ######################################
    
    tabItem(tabName = "results",
           #tags$h3("Authentication"),
           #tags$h4("Please Provide Your Credentials"),
            htmlOutput("page")
            ),
            

    
    ################################## 
    #-----Tab3 About Me------#
    ######################################
    
    tabItem(tabName = "about",
            tags$h2("Adil Al-Busaidi"),
            tags$h3("Assistant Professor at Sultan Qaboos University"),
            tags$h4("aalbusaidi@gmail.com"),
            tags$br(),
            
            "Many thanks to Dean Attali for his", a(href= "http://deanattali.com/2015/06/14/mimicking-google-form-shiny/","article"), "on mimicking a google form with a shiny app"
            
            
    )
    #~~~~~~
  ) #Closing tabItems()
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
) #closing dashboardBody(

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
ui <- dashboardPage(skin = "purple",
                    header, sidebar, body)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
server = function(input, output, session) {
  ###################################  
  ###login staff
  USER <- reactiveValues(Logged = Logged)
  
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
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        div(class="outer",do.call(tabItem,c(inverse=TRUE,title = "Successfully Logged Into Admin Area!",ui2())))
      })
      print(ui)
    }
  })
  ######################################
  ###################################  
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })    
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  #---------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #displaying data in table
  output$responsesTable <- DT::renderDataTable(
    loadData(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  ) 
  
  output$responsesGender <- renderPlot({
    data <- loadData()
    # qplot(income,factor(gender, labels = c("male","female")), data=data, geom = c("boxplot", "jitter"))
    g <- ggplot(data=data, aes(gender))
    # Number of cars in each class:
    
    g + geom_bar(aes(fill = education))
  })
  
  output$responsesAge2 <- renderPlot({
    data <- loadData()
    #edu <- factor(data$education, labels =  c("High School","Technical/Vocational Training","College Diploma-2 Year", "Bachelor's Degree","Master's Degree", "PhD/DBA","Missing"))
    p <- qplot(education, income, data = data, geom = c("boxplot", "jitter"), fill = education)+
      ggtitle("Income Distribution by Gender")
    p+ geom_text(aes(label=income)) #to display values
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #to download data
  output$downloadBtn <- downloadHandler(
    filename = function() { 
      sprintf("tanfeedh_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #spiderChart
  output$circular2 <- renderPlot({
    mydata <- loadData()
    dat <- mydata[,c(5,7)]
    chordDiagram(as.data.frame(dat), transparency = 0.5,diffHeight = 0.01)
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$dens <- renderPlot({
    mydata <- loadData()
    p  <- ggplot(mydata, aes(age2, colour=overcome, fill=overcome))
    p  <- p + geom_density(alpha=0.55) +
      ggtitle("Will Tanfeedh Overcome Economic obstacles according to age")
    p
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$overco <- renderPlot({
    mydata <- loadData()
    # First we do a ggplot with several graphs
    p <- ggplot(data = mydata, aes(x = age2, y = income)) +
      #geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
      geom_smooth(aes(colour = overcome, fill = overcome)) + facet_wrap(~ overcome)+
      ggtitle("The relation btw age&income and whether Omanis belive Tanfeedh will fail or pass")
    
    # Then we make them interactive with ggplotly
    
    p
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$overco2 <- renderPlot({
    mydata <- loadData()
    ggplot(data = mydata) + 
      geom_bar(mapping = aes(x = overcome, fill = overcome))+
      ggtitle("Tanfeed will overcome the obstacles facing economic diversification")
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$familiar <- renderPlot({
    mydata <- loadData()
    ggplot(data = mydata) + 
      geom_bar(mapping = aes(x = aware, fill = aware), width = 1) + 
      coord_polar()+
      ggtitle("To what extent people are familiar with tanfeedh")
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$size <- renderInfoBox({
    mydata <- loadData()
    
    infoBox(
      #count sample size
      "Sample Size", paste("N:",nrow(mydata), " "),
      icon = icon("user", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$fema <- renderInfoBox({
    mydata <- loadData()
    #count female participants  
    infoBox(
      "Female Participants", paste("N:",nrow(mydata[mydata$gender=="Female",]), " "),
      icon = icon("hand-right", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  output$male <- renderInfoBox({
    mydata <- loadData()
    # count male participants
    infoBox(
      #count reTweets
      "Male Participants", paste("N:",nrow(mydata[mydata$gender=="Male",]), " "),
      icon = icon("hand-right", lib = "glyphicon"),fill = TRUE, color = "yellow")
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  ###################################  
} #to close server

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

shinyApp(ui, server)