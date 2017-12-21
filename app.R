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
library(dplyr)
library(rdrop2)
library(sodium)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = 'MPRT SETUP'),
  dashboardSidebar(),
  dashboardBody(# Application title

    # Sidebar with a slider input for number of bins
    tabsetPanel(
      tabPanel(
        "Admin sign up",
        h2('Admin sign up'),
        textInput('adminemail', 'Email', 'example@domain.com'),
        textInput('adminproject', 'Project', 'Project name'),
        textInput('admingroup', 'Company', 'example'),
        passwordInput('adminpassword', 'Password'),
        passwordInput('adminpasswordrepeat', 'Repeat Password'),
        textOutput('saveresponse'),
        br(),
        br(),
        actionButton('saveadmin', 'SIGN UP'),
        br(),
        verbatimTextOutput('test11'),
        # OUTPUT FOR TESTING #11
        
        hr(),
        h2('Admin sign in'),
        textInput('adminemailin', 'Email'),
        textInput('adminprojectin', 'Project'),
        passwordInput('adminpasswordin', 'Password'),
        br(),
        textOutput('signinresponse'),
        br(),
        actionButton('signadmin', 'SIGN IN'),
        verbatimTextOutput('test12')     # OUTPUT FOR TESTING #12
        
      ),
      tabPanel(
        "Add people",
        h2('Invites'),
        textInput('addemail', 'Email'),
        # ..........................................................
        # ..................... LIST OF ROLES ......................
        # ..........................................................
        selectInput(
          'addrole',
          'Role',
          c(
            'Admin' = 'admin',
            'Specialist' = 'specialist',
            'Guest' = 'guest'
          )
        ),
        # ..........................................................
        # ..........................................................
        # ..........................................................
        helpText(
          'You are the current admin of the project. You can transfer this position to another user later in the project'
        ),
        hr(),
        dataTableOutput('invites'),
        actionButton('sendinvites', 'Send Invites')
        #contents
      ),
      tabPanel("Status",
               h2('Status table'),
               dataTableOutput('status')
               #contents)
      ))))


server <- function(input, output) {
  library(stringr)
  library(sodium)
  library(stringi)
  source('C:/Testes/Shiny/Login/dropboxsl.R')
  
  
  
  # TAB PANEL 1 - ADMIN SIGN IN/UP ......................................................................................................
  auth <- FALSE
  
  ## ACTION BUTTON: SAVE ADMIN
  
  
  observeEvent(input$saveadmin,{
    regex <- "^[[:alnum:].-_]+@[[:alnum:].-]+$"
    email_ver <- is.na(str_match(input$adminemail,regex)) # returns FALSE for valid email
    password_ver <- identical(input$adminpassword,input$adminpasswordrepeat) # returns TRUE for valid passwords
    passwordlength_ver <- nchar(input$adminpassword) > 5
    group_ver <- identical(input$admingroup,'') # returns FALSE for valid group names
    project_ver <- identical(input$adminproject,'') # returns FALSE for valid project names
    
    # Create name for project file
    filename <<- paste(input$adminemail,input$adminproject,sep = '_')
    
    project_exist <<- DROPBOX_checkNewProject(paste(filename,'.csv',sep='')) # TRUE IF PROJECT NAME IS BEING USED
    if (project_exist == TRUE){
      output$saveresponse <- renderText('PROJECT NAME ALREADY IN USE WITH THIS COMPANY')
    }    
    if (email_ver == TRUE){
      output$saveresponse <- renderText('Invalid email!')
    }
    if (group_ver == TRUE){
      output$saveresponse <- renderText('Invalid company/group name!')
    }
    if (project_ver == TRUE){
      output$saveresponse <- renderText('Invalid project name!')
    }
    if (password_ver == FALSE){
      output$saveresponse <- renderText('Passwords does not match!')
    }
    if (passwordlength_ver == FALSE){
      output$saveresponse <- renderText('Weak password, please use 6 or more characteres!')
    }
    if (email_ver == FALSE & group_ver == FALSE & project_ver == FALSE & password_ver == TRUE & passwordlength_ver == TRUE & project_exist == FALSE){
      # AUTORIZE PROJECT CREATION!!!
      output$saveresponse <- renderText('User created!')
      # Hash the password
      hash <- password_store(input$adminpassword)
      # Create name for admin file
      fileID <- paste(input$adminemail,sprintf("%s", stri_rand_strings(1, 8, '[A-Z]')),input$adminproject,sep = '_')
      # Select data for project file
      export <<- c(input$adminemail,input$adminproject,input$admingroup,'admin',hash,fileID)
      exportdf <- data.frame(t(export))
      names(exportdf) <- c('EMAIL','PROJECT','GROUP','ROLE','HASH','FILEID')
      # Save file to /shiny/users DROPBOX FOLDER
      DROPBOX_SaveLoginData(exportdf,filename)
      projectfile <- filename
      projectdata <- exportdf
    }
    
  })
  
  ## ACTION BUTTON: SIGN IN ADMIN
  
  observeEvent(input$signadmin,{
    regex <- "^[[:alnum:].-_]+@[[:alnum:].-]+$"
    email_ver <- is.na(str_match(input$adminemailin,regex)) # returns FALSE for valid email
    project_ver <- identical(input$adminprojectin,'') # returns FALSE for valid project names
    password_ver <- identical(input$adminpasswordin,'') # returns FALSE for valid password input. WILL NOT CHECK FOR CORRECT PASSWORD YET
    
    if (email_ver == TRUE){
      output$signinresponse <- renderText('Invalid email!')
    }
    if (project_ver == TRUE){
      output$signinresponse <- renderText('Invalid project name!')
    }
    if (password_ver == TRUE){
      output$signinresponse <- renderText('Invalid password')
    }
    
    
    if (email_ver == FALSE & project_ver == FALSE & password_ver == FALSE){
      # look for file with the project name
      filesearch <<- paste(input$adminemailin,input$adminprojectin,sep = '_')
      filesearch <<- paste(filesearch,'.csv',sep='')
      
      project_exist <<- DROPBOX_checkNewProject(filesearch) # TRUE IF PROJECT NAME IS BEING USED
      
      if (project_exist == TRUE){
        # CHECK FOR PASSWORD VALIDATION
        pw <- (input$adminpasswordin)
        auth <<- DROPBOX_autorizeLogin(filesearch, pw)
        output$test12 <- renderText(paste(auth, filesearch,sep = ' '))
        if (auth == TRUE){
          output$signinresponse <- renderText('Signed in!')
          projectfile <- filesearch
          projectdata <- DROPBOX_loadData(projectfile)
          output$test12 <- renderPrint(projectdata)
        }
        if (auth == FALSE) {
          output$signinresponse <- renderText('Invalid password!')
        }
      } 
      if (project_exist == FALSE) {
        output$signinresponse <- renderText('Project not found! Check email or project name!')
        auth <- FALSE
      }
      
      
    }
    
    
  })
  
  
  
  # TAB PANEL 2 - INVITES       ......................................................................................................
  
}

# Run the application 
shinyApp(ui = ui, server = server)