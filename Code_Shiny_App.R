# clean environment
rm(list = ls(all = TRUE))

# Load packages, functions and final models
library(shiny)
# library(magrittr) # for the pipe-operator (%>%)
# library(htmltools) # for the HTML function
library(censReg) # for tobit model with random effects
library(plm) # because the censReg object was estimated with random effects
library(SAEforest) # for random forest
source("predict.censReg_tobit.R")

load("06_Final_Model.RData")
load("levels.RData")

# Create user interface (UI)
u <- tagList(
  navbarPage(
    # UI for title page with instructions 
    title = "",
    id = "Mapping_App",
    tabPanel("Introduction",
             # Define tag styles                          
             tags$head(tags$style(HTML(".not_bold label {font-weight:normal;}"))), 
             tags$head(tags$style(HTML(".subheading label {font-weight:bold;font-style:italic}"))),
             fluidRow(
               column(11, offset = 0,  
                      br(), 
                      h4("Introduction"),
                      p("This web application maps SIBDQ answers to EQ-5D-5L utility index scores as described in [cite Mapping paper].\n
                     You can choose between three different sets of covariates:"),
                      br(),
                      p("1) Mapping the SIBDQ without any other covariates (Model 3.1)"),
                      br(),
                      p("2) Mapping the SIBDQ with BMI, smoking status and disease type (Model 3.3')"),
                      br(),
                      p("3) Mapping the SIBDQ with BMI, smoking status, disease type and SIBDQ subscales (Model 7.5')"),
                      br(),
                      p("We recommend using the most complex Model 7.5' for mapping if all corresponding covariates are available. 
                     Model 3.3' is recommended when the general SIBDQ score, BMI, smoking status (i.e. currently smoking vs. not smoking) and disease type (i.e., Crohn's disease vs. ulcerative colitis or indeterminate colitis), but no SIBDQ subscales are known.
                     We recommend Model 3.1 when not all of the other covariates required for Model 3.3' are available."),
                      actionButton('jumpToModelSelection', 'Next')))),
    
    # UI to choose the model
    tabPanel("Model selection",
             fluidRow(
               column(11, offset = 0,  
                      br(), 
                      h4("Model selection"),
                      p("Choose the model you want to apply to your data.\n
                         Afterwards you will be asked to enter the required variables."),
                      br(),
                      selectInput("Model","Choose the model",
                                  choices = c("Model 3.1 (general SIBDQ score only)",
                                              "Model 3.3' (general SIBDQ score, BMI, smoking status, disease type)",
                                              "Model 7.5' (general SIBDQ score, BMI, smoking status, disease type, SIBDQ subscales)")),
                      actionButton('BackToIntro', 'Back'),
                      actionButton('jumpToInputs', 'Next')
               ))), 
    # UI for covariate inputs
    tabPanel("Model 3.1",
             # UI for covariate inputs
             fluidRow(
               column(11, offset = 0,  
                      br(), 
                      h4("Model 3.1"),
                      p("Enter the required variables for Model 3.1 into the sidebar panel."),
                      sidebarPanel(
                        strong("General SIBDQ score"),
                        div(textInput('str_SIBDQ3_1', 'The model requires mean scores as input (i.e. range 1-7). 
                                      If values are entered as sum scores, they will automatically be transformed into mean scores.', "",
                                      placeholder = "5.6, 6.7, 4.1"), class="not_bold"),
                        p('Enter values in comma delimited format.'),
                        actionButton('BackToModelSelection3_1', 'Back'),
                        actionButton('jumpToModelOutput3_1', 'Run')),
                      mainPanel(
                        h4('You entered'),
                        verbatimTextOutput("oid_SIBDQ3_1"))))),
    tabPanel(title = "Model 3.3'", value = "Model 3.3'",
             fluidRow(
               column(11, offset = 0,  
                      br(), 
                      h4("Model 3.3'"),
                      p("Type or paste the BMI into the sidebar panel. Single observations need to be separated by a comma."),
                      sidebarPanel(
                        strong("General SIBDQ score"),
                        div(textInput('str_SIBDQ3_3', 'The model requires mean scores as input (i.e. range 1-7). 
                                      If values are entered as sum scores, they will automatically be transformed into mean scores.', "",
                                      placeholder = "5.6, 6.7, 4.1"), class="not_bold"),
                        strong("BMI"),
                        div(textInput('str_BMI3_3', 'Enter BMI values here.', "",
                                      placeholder = "22.5, 17, 24.5"), class="not_bold"),
                        strong("Smoking status"),
                        div(textInput('str_Smoker3_3', 'Type or paste the smoking status (i.e. whether a person is currently smoking or not) into the sidebar panel. 
                        Single observations need to be separated by a comma. The value 1 corresponds to a person who is currently smoking and 0 corresponds to a person who is currently not smoking.', "",
                                      placeholder = "0, 1, 1, 0"), class="not_bold"),
                        strong("Disease type"),
                        div(textInput('str_CD3_3', "Type or paste the IBD type (i.e. Crohn's disease vs. ulcerative colitis or indeterminate colitis) into the sidebar panel. 
                                              Single observations need to be separated by a comma.
                                              The value 1 corresponds to a person who is diagnosed with Crohn's disease and 0 corresponds to a person who is diagnosed with ulcerative colitis or indeterminate colitis.","",
                                      placeholder = "0, 1, 1, 0"), class="not_bold"),
                        actionButton('BackToModelSelection3_3', 'Back'),
                        actionButton('jumpToModelOutput3_3', 'Run')),
                      mainPanel(
                        h4('You entered'),
                        verbatimTextOutput("oid_SIBDQ3_3"),
                        verbatimTextOutput("oid_BMI3_3"),
                        verbatimTextOutput("oid_Smoker3_3"),
                        verbatimTextOutput("oid_CD3_3"))))),
    tabPanel("Model 7.5' (1)",
             fluidRow(
               column(11, offset = 0,  
                      br(), 
                      h4("Model 7.5' (1)"),
                      p("Type or paste the individual scores of the SIBDQ subscales into the sidebar panel. Please separate single observations by a comma. The placeholders in the text boxes show exemplarily in which form the values have to be entered."),
                      sidebarPanel(
                        strong("General SIBDQ score"),
                        div(textInput('str_SIBDQ7_5', 'The model requires mean scores as input (i.e. range 1-7). 
                                      If values are entered as sum scores, they will automatically be transformed into mean scores.', "",
                                      placeholder = "5.6, 6.7, 4.1, 7"), class="not_bold"),
                        strong("BMI"),
                        div(textInput('str_BMI7_5', 'Enter BMI values here.', "",
                                      placeholder = "22.5, 17, 24.5, 20"), class="not_bold"),
                        strong("Smoking status"),
                        div(textInput('str_Smoker7_5', 'Type or paste the smoking status (i.e. whether a person is currently smoking or not) into the sidebar panel. 
                        Single observations need to be separated by a comma. The value 1 corresponds to a person who is currently smoking and 0 corresponds to a person who is currently not smoking.', "",
                                      placeholder = "0, 1, 1, 0"), class="not_bold"),
                        strong("Disease type"),
                        div(textInput('str_CD7_5', "Type or paste the IBD type (i.e. Crohn's disease vs. ulcerative colitis or indeterminate colitis) into the sidebar panel. 
                                              Single observations need to be separated by a comma.
                                              The value 1 corresponds to a person who is diagnosed with Crohn's disease and 0 corresponds to a person who is diagnosed with ulcerative colitis or indeterminate colitis.","",
                                      placeholder = "0, 1, 1, 0"), class="not_bold"),
                        
                        actionButton('BackToModelSelection7_5', 'Back'),
                        actionButton('jumpToInput7_5_2', 'Next')),
                      mainPanel(
                        h4('You entered'),
                        verbatimTextOutput("oid_SIBDQ7_5"),
                        verbatimTextOutput("oid_BMI7_5"),
                        verbatimTextOutput("oid_Smoker7_5"),
                        verbatimTextOutput("oid_CD7_5")
                      )))),
    tabPanel("Model 7.5' (2)",
             fluidRow(
               column(11, offset = 0,  
                      br(), 
                      h4("Model 7.5' (2)"),
                      p("Type or paste the individual scores of the SIBDQ subscales into the sidebar panel. Please separate single observations by a comma. The placeholders in the text boxes show exemplarily in which form the values have to be entered."),
                      sidebarPanel(
                        strong("SIBDQ Subscales"),
                        p("Type or paste the scores of the SIBDQ subscales into the sidebar panel. Single observations need to be separated by a comma.
                        The model needs mean scores (i.e. range 1-7) as input parameters. If you enter sum scores, they will automatically be transformed into mean scores."),
                        div(textInput('str_SIBDQ_Bowel', 'Bowel\n', "",
                                      placeholder = "5.6, 6.7, 4.1"), class = "subheading"),
                        conditionalPanel(condition = "output.SIBDQ_Bowel_buttons", # Conditional panel that is shown when the range cannot unambiguously determined
                                         radioButtons("rangeBowel", "Ambiguous range. 
                                 Please choose the theoretical range for the SIBDQ Bowel:",
                                                      c("1-7" = "Bowelmean",
                                                        "3-21" = "Bowelsum"),
                                                      inline = TRUE)),
                        div(textInput('str_SIBDQ_Emotional', 'Emotional\n', "",
                                      placeholder = "5.6, 6.7, 4.1"), class = "subheading"),
                        conditionalPanel(condition = "output.SIBDQ_Emotional_buttons", # Conditional panel that is shown when the range cannot unambiguously determined
                                         radioButtons("rangeEmotional", "Ambiguous range. 
                                 Please choose the theoretical range for the SIBDQ Emotional:",
                                                      c("1-7" = "Emotionalmean",
                                                        "3-21" = "Emotionalsum"),
                                                      inline = TRUE)),
                        div(textInput('str_SIBDQ_Social', 'Social\n', "",
                                      placeholder = "5.6, 6.7, 4.1"), class = "subheading"),
                        conditionalPanel(condition = "output.SIBDQ_Social_buttons", # Conditional panel that is shown when the range cannot unambiguously determined
                                         radioButtons("rangeSocial", "Ambiguous range. 
                                 Please choose the theoretical range for the SIBDQ Social:",
                                                      c("1-7" = "Socialmean",
                                                        "2-14" = "Socialsum"),
                                                      inline = TRUE)),
                        div(textInput('str_SIBDQ_Systemic', 'Systemic\n', "",
                                      placeholder = "5.6, 6.7, 4.1"), class = "subheading"),
                        conditionalPanel(condition = "output.SIBDQ_Systemic_buttons", # Conditional panel that is shown when the range cannot unambiguously determined
                                         radioButtons("rangeSystemic", "Ambiguous range. 
                                 Please choose the theoretical range for the SIBDQ Systemic:",
                                                      c("1-7" = "Systemicmean",
                                                        "2-14" = "Systemicsum"),
                                                      inline = TRUE)),
                        actionButton('BackToInput7_5_1', 'Back'),
                        actionButton('jumpToModelOutput7_5', 'Run')),
                      mainPanel(
                        h4('You entered'),
                        verbatimTextOutput("oid_SIBDQ_Bowel"),
                        verbatimTextOutput("oid_SIBDQ_Emotional"),
                        verbatimTextOutput("oid_SIBDQ_Social"),
                        verbatimTextOutput("oid_SIBDQ_Systemic"))))),
    # UI for mapping output
    tabPanel("Mapping output",
             fluidRow(
               column(11, offset = 0,
                      br(),
                      h4("Mapping output"),
                      tags$div("The output is the predicted EQ-5D-5L index value.
                               An EQ-5D-5L index value of <0 corresponds to health states that are perceived as worse than death. An index value of 1 is defined as perfect health."),
                      htmlOutput("output_text_ui"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          h4('Your input'),
                          verbatimTextOutput("oid_input", placeholder = TRUE), 
                          width = 5,
                          actionButton('BackToModelSelection', 'Back to model selection')),
                        mainPanel(
                          h4('Your Output'),
                          downloadButton("Downloadtable", label = "Download output as .csv file"),
                          tableOutput("oid_output"),
                          width = 7)
                      ))))))

# Define server output
s <- shinyServer(function(input, output, session) {
  
  # Update tab when button is pressed (if conditions are fulfilled) ################################################################################################
  
  observeEvent(input$jumpToModelSelection, {
    updateTabsetPanel(session, "Mapping_App",
                      selected = "Model selection")
  })
  
  observeEvent(input$jumpToInputs, {
    if(input$Model == "Model 3.1 (general SIBDQ score only)"){
      updateTabsetPanel(session, "Mapping_App",
                        selected = "Model 3.1")
    }else{
      if(input$Model == "Model 3.3' (general SIBDQ score, BMI, smoking status, disease type)"){
        updateTabsetPanel(session, "Mapping_App",
                          selected = "Model 3.3'")
      }else{
        updateTabsetPanel(session, "Mapping_App",
                          selected = "Model 7.5' (1)")
        
      }}})
  
  observeEvent(input$BackToIntro, {
    updateTabsetPanel(session, "Mapping_App",
                      selected = "Introduction")
  })
  
  observeEvent(input$jumpToInput7_5_2, {
    if(input$str_SIBDQ7_5 == ""){ # entered any value?
      showNotification("Please enter at least one value.", type = "error")
    }else{ # do all input vectors have the same length?
      if(any(sapply(list(nobs_SIBDQ(),nobs_BMI(),nobs_Smoker(),nobs_CD()), function(x) x != nobs_SIBDQ()))){
        showNotification("The number of observations is not equal across all entries.", type = "error")  
      }else{
        updateTabsetPanel(session, "Mapping_App",
                          selected = "Model 7.5' (2)")
      }}})
  
  observeEvent(input$jumpToModelOutput3_1, {
    if(input$str_SIBDQ3_1 == ""){ # entered any value?
      showNotification("Please enter at least one value.", type = "error")
    }else{ 
      updateTabsetPanel(session, "Mapping_App",
                        selected = "Mapping output")
    }})
  
  observeEvent(input$BackToModelSelection3_1, {
    updateTabsetPanel(session, "Mapping_App",
                      selected = "Model selection")
  })
  
  observeEvent(input$jumpToModelOutput3_3, {
    if(input$str_SIBDQ3_3 == ""){ # entered any value?
      showNotification("Please enter at least one value.", type = "error")
    }else{ # do all input vectors have the same length?
      if(any(sapply(list(nobs_SIBDQ(),nobs_BMI(),nobs_Smoker(),nobs_CD()), function(x) x != nobs_SIBDQ()))){
        showNotification("The number of observations is not equal across all entries.", type = "error")  
      }else{
        updateTabsetPanel(session, "Mapping_App",
                          selected = "Mapping output")
      }}})
  
  observeEvent(input$BackToModelSelection3_3, {
    updateTabsetPanel(session, "Mapping_App",
                      selected = "Model selection")
  })
  
  observeEvent(input$BackToModelSelection7_5, {
    updateTabsetPanel(session, "Mapping_App",
                      selected = "Model selection")
  })
  
  observeEvent(input$jumpToModelOutput7_5, {
    if(any(sapply(list(nobs_SIBDQ_Bowel(), nobs_SIBDQ_Emotional(), nobs_SIBDQ_Social(), nobs_SIBDQ_Systemic()), function(x) x != nobs_SIBDQ_Bowel()))){
      showNotification("The number of observations is not equal across all entries.", type = "error")  
    }else{
      if(any(sapply(list(nobs_SIBDQ_Bowel(),nobs_SIBDQ_Emotional(),nobs_SIBDQ_Social(),nobs_SIBDQ_Systemic()), function(x) x != nobs_SIBDQ()))){
        showNotification("The number of observations must be equal to your previous entries.", type = "error")
      }else{
        if(!isTRUE(all.equal(num_SIBDQ2()*10,(num_SIBDQ_Systemic2()*2 + num_SIBDQ_Social2()*2 + num_SIBDQ_Bowel2()*3 + num_SIBDQ_Emotional2()*3)))){
          showNotification("Calculating the SIBDQ general score based on the SIBDQ subscales results in different values than your entries.
                           Please check if you entered values incorrectly.", type = "error")
        }else{
          updateTabsetPanel(session, "Mapping_App",
                            selected = "Mapping output")
        }}}})
  
  observeEvent(input$BackToInput7_5_1, {
    updateTabsetPanel(session, "Mapping_App",
                      selected = "Model 7.5' (1)")
  })
  
  observeEvent(input$BackToModelSelection, {
    updateTabsetPanel(session, "Mapping_App",
                      selected = "Model selection")
  })
  
  # Predict EQ-5L-5L values
  pred_3_1 <- eventReactive(input$jumpToModelOutput3_1, {
    (predict.censReg_tobit(full_model3_1, pdata.frame(data.frame(sIBDQ_mean = num_SIBDQ2(),
                                                      id  = seq(1:length(num_SIBDQ2())),
                                                      time = rep(1,length(num_SIBDQ2()))),
                                                      index = c("id", "time")))-1)*(-1)
  })
  
  pred_3_3 <- eventReactive(input$jumpToModelOutput3_3, {
    (predict.censReg_tobit(full_model3_3, pdata.frame(data.frame(sIBDQ_mean = num_SIBDQ2(),
                                                                 BMI = num_BMI(),
                                                                 Smoker = num_Smoker(),
                                                                 CD =  num_CD(),
                                                                 id  = seq(1:length(num_SIBDQ2())),
                                                                 time = rep(1,length(num_SIBDQ2()))),
                                                      index = c("id", "time")))-1)*(-1)
  })
  
  pred_7_5 <- eventReactive(input$jumpToModelOutput7_5, {
    predict(full_model7_5, data.frame(sIBDQ_mean = num_SIBDQ2(),
                                      BMI = num_BMI(),
                                      Smoker = num_Smoker(),
                                      CD =  num_CD(),
                                      sIBDQ_subsc_Bow = num_SIBDQ_Bowel2(),
                                      sIBDQ_subsc_Emo = num_SIBDQ_Emotional2(), 
                                      sIBDQ_subsc_Soc = num_SIBDQ_Social2(),
                                      Pseudo_Iomtech = factor(x = rep("J101349",length(num_SIBDQ2())),
                                                               levels = levels)))
  })
  
  pred_7_5_se <- eventReactive(input$jumpToModelOutput7_5, {
    predict(full_model7_5$Forest, data.frame(sIBDQ_mean = num_SIBDQ2(),
                                             BMI = num_BMI(),
                                             Smoker = num_Smoker(),
                                             CD =  num_CD(),
                                             sIBDQ_subsc_Bow = num_SIBDQ_Bowel2(),
                                             sIBDQ_subsc_Emo = num_SIBDQ_Emotional2(),
                                             sIBDQ_subsc_Soc = num_SIBDQ_Social2(),
                                             Pseudo_Iomtech = factor(x = rep("J101349",length(num_SIBDQ2())),
                                                                     levels = levels)),
            type = "se",
            se.method = 'jack')
  })
  
  
  
  # Create download file
  
  Download_Table_reactive <- reactive(
    if(input$Model == "Model 3.1 (general SIBDQ score only)"){
      
      data.frame("Observation Nr." = seq(1:length(num_SIBDQ2())),
                 "Predicted EQ-5D-5L index value" = as.numeric(pred_3_1()),
                 check.names = FALSE)
    }else{
      if(input$Model == "Model 3.3' (general SIBDQ score, BMI, smoking status, disease type)"){
        data.frame("Observation Nr." = seq(1:length(num_SIBDQ2())),
                   "Predicted EQ-5D-5L index value" = as.numeric(pred_3_3()),
                   check.names = FALSE)
      }else{
        data.frame("Observation Nr." = seq(1:length(num_SIBDQ2())),
                   "Predicted EQ-5D-5L index value" = as.numeric(pred_7_5()),
                   "Standard Error" = round(as.numeric(pred_7_5_se()$se + 0.07936), digits = 4),
                   "95% Confidence Interval" = paste(round(as.numeric(pred_7_5())-1.96*pred_7_5_se()$se + 0.07936, digits = 4), 
                                                     round(as.numeric(pred_7_5())+1.96*pred_7_5_se()$se + 0.07936, digits = 4), sep = ";"),
                   check.names = FALSE)
      }}
  )
  
  # Update values for variables entered in different text fields and estimate models #####################################################################
  
  observeEvent(input$jumpToModelOutput3_1 , {
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_SIBDQ3_3",
                    value = input$str_SIBDQ3_1)
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_SIBDQ7_5",
                    value = input$str_SIBDQ3_1)
    # Server output for mapped EQ5D-5L values
    output$oid_input <- renderPrint({
      cat("Model 3.1 (general SIBDQ score only)\n")
      cat("Covariate inputs:\n")
      z <- data.frame("SIBDQ general" = num_SIBDQ2(), check.names = FALSE)
      print(z)
    }) 
    output$oid_output <-  renderTable({
      a <- as.numeric(pred_3_1())
      x <- paste("<strong>",a,sep = "")
      x <- paste(x,"</strong>",sep="")
      # y <- as.numeric(pred_3_1()$variance.estimates)
      z <- data.frame("Observation Nr." = seq(1:length(a)), 
                      "Predicted EQ-5D-5L index value" = x, 
                      # "Standard Error" = sqrt(y), 
                      # "95% Confidence Interval" = paste(a-1.96*sqrt(y), a+1.96*sqrt(y), sep = ";"), 
                      check.names = FALSE)
      z
    },sanitize.text.function=function(x){x}, align = "c") 
  })
  
  observeEvent(input$jumpToModelOutput3_3 , {
    # update SIBDQ entries
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_SIBDQ3_1",
                    value = input$str_SIBDQ3_3)
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_SIBDQ7_5",
                    value = input$str_SIBDQ3_3)
    # update BMI entries
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_BMI7_5",
                    value = input$str_BMI3_3)
    # update Smoker entries
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_Smoker7_5",
                    value = input$str_Smoker3_3)
    # update disease type entries
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_CD7_5",
                    value = input$str_CD3_3)
    # Server output for mapped EQ5D-5L values
    output$oid_input <- renderPrint({
      cat("Model 3.3' (general SIBDQ score, BMI, smoking status, disease type)\n")
      cat("Covariate inputs:\n")
      z <- data.frame("SIBDQ general" = num_SIBDQ2(),
                      "BMI" = num_BMI(),
                      "Smoking status" = num_Smoker(),
                      "Disease type" = num_CD(),
                      check.names = FALSE) 
      print(z)
    }) 
    output$oid_output <-  renderTable({
      a <- as.numeric(pred_3_3())
      x <- paste("<strong>",a,sep = "")
      x <- paste(x,"</strong>",sep="")
      # y <- as.numeric(pred_3_3()$variance.estimates)
      z <- data.frame("Observation Nr." = seq(1:length(a)), 
                      "Predicted EQ-5D-5L index value" = x, 
                      # "Standard Error" = sqrt(y), 
                      # "95% Confidence Interval" = paste(a-1.96*sqrt(y), a+1.96*sqrt(y), sep = ";"), 
                      check.names = FALSE)
      z
    },sanitize.text.function=function(x){x}, align = "c")
  }) 
  
  
  observeEvent(input$jumpToModelOutput7_5 , {
    # update SIBDQ entries
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_SIBDQ3_1",
                    value = input$str_SIBDQ7_5)
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_SIBDQ3_3",
                    value = input$str_SIBDQ7_5)
    # update BMI entries
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_BMI3_3",
                    value = input$str_BMI7_5)
    # update Smoker entries
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_Smoker3_3",
                    value = input$str_Smoker7_5)
    # update disease type entries
    updateTextInput(session = getDefaultReactiveDomain(),
                    inputId = "str_CD3_3",
                    value = input$str_CD7_5)
    # Server output for mapped EQ5D-5L values
    output$oid_input <- renderPrint({
      cat("Model 7.5' (general SIBDQ score, BMI, smoking status, disease type, SIBDQ subscales)\n")
      cat("Covariate inputs:\n")
      z <- data.frame("SIBDQ general" = num_SIBDQ2(),
                      "BMI" = num_BMI(),
                      "Smoking status" = num_Smoker(),
                      "Disease type" = num_CD(),
                      "SIBDQ Bowel" = num_SIBDQ_Bowel2(),
                      "SIBDQ Emotional" = num_SIBDQ_Emotional2(),
                      "SIBDQ Social" = num_SIBDQ_Social2(),
                      check.names = FALSE) 
      print(z)
    }) 
    output$oid_output <- renderTable({
      a <- as.numeric(pred_7_5())
      x <- paste("<strong>",a,sep = "")
      x <- paste(x,"</strong>",sep="")
      sd <- as.numeric(pred_7_5_se()$se)
      var <- sd^2
      z <- data.frame("Observation Nr." = seq(1:length(a)),
                      "Predicted EQ-5D-5L index value" = x, 
                      "Standard Error" = sqrt(var + 0.07936^2),
                      "95% Confidence Interval" = paste(round(a-1.96*sqrt(var + 0.07936^2), digits = 4), 
                                                        round(a+1.96*sqrt(var + 0.07936^2), digits = 4), sep = ";"),
                      check.names = FALSE)
      z
    }, sanitize.text.function=function(x){x}, align = "c")
  })
  
  
  output$Downloadtable <- downloadHandler(
    filename = "Mapping_output.csv",
    content = function(file) {
      write.csv(Download_Table_reactive(), file, 
                row.names = FALSE)
    })
  
  
  # Define reactive expressions for covariates
  num_SIBDQ <- reactive(if(input$Model == "Model 3.1 (general SIBDQ score only)"){
    as.numeric(unlist(strsplit(input$str_SIBDQ3_1,",")))}else{
      if(input$Model == "Model 3.3' (general SIBDQ score, BMI, smoking status, disease type)"){
        as.numeric(unlist(strsplit(input$str_SIBDQ3_3,",")))
      }else{
        as.numeric(unlist(strsplit(input$str_SIBDQ7_5,",")))
      }})
  num_SIBDQ_Bowel <- reactive(as.numeric(unlist(strsplit(input$str_SIBDQ_Bowel,","))))
  # reactive value for conditional panel
  output$SIBDQ_Bowel_buttons <- reactive({
    if(input$str_SIBDQ_Bowel != "" &
       range(num_SIBDQ_Bowel())[1] > 2 &
       range(num_SIBDQ_Bowel())[2] < 8){
      1}else{
        0}})
  
  outputOptions(output, "SIBDQ_Bowel_buttons", suspendWhenHidden = FALSE)  
  
  num_SIBDQ_Emotional <- reactive(as.numeric(unlist(strsplit(input$str_SIBDQ_Emotional,","))))
  
  # reactive value for conditional text in the mapping output
  output_text <- reactive({
    if(input$Model == "Model 7.5' (general SIBDQ score, BMI, smoking status, disease type, SIBDQ subscales)"){
      "Standard errors and 95% prediction intervals were estimated as follows:
     The standard deviation of the fixed part was estimated with the jackknife-after-bootstrap approach by 
     <a href='https://jmlr.org/papers/v15/wager14a.html'>Wager et al., (2014)</a>.
     To approximate the total standard error, the estimated standard deviation of the fixed part of the model and the standard deviation of the random effects were added together."
    } else {
      ''
    }
  })
  # Render the text so that it is available in the UI
  output$output_text_ui <- renderText(output_text())
  
  # tags$a(href="https://jmlr.org/papers/v15/wager14a.html","Wager et al. (2014)."),



#  output$output_wager_ui <- renderUI({tags$a(href="https://jmlr.org/papers/v15/wager14a.html","(Wager et al., 2014)")})

    
  # reactive value for conditional panel
  output$SIBDQ_Emotional_buttons <- reactive({
    if(input$str_SIBDQ_Emotional != "" &
       range(num_SIBDQ_Emotional())[1] > 2 &
       range(num_SIBDQ_Emotional())[2] < 8){
      1}else{
        0}})
  
  outputOptions(output, "SIBDQ_Emotional_buttons", suspendWhenHidden = FALSE)  
  
  num_SIBDQ_Social <- reactive(as.numeric(unlist(strsplit(input$str_SIBDQ_Social,","))))
  
  output$SIBDQ_Social_buttons <- reactive({
    if(input$str_SIBDQ_Social != "" &
       range(num_SIBDQ_Social())[1] > 1 &
       range(num_SIBDQ_Social())[2] < 8){
      1}else{
        0}})
  
  outputOptions(output, "SIBDQ_Social_buttons", suspendWhenHidden = FALSE)  
  
  num_SIBDQ_Systemic <- reactive(as.numeric(unlist(strsplit(input$str_SIBDQ_Systemic,","))))
  
  output$SIBDQ_Systemic_buttons <- reactive({
    if(input$str_SIBDQ_Systemic != "" &
       range(num_SIBDQ_Systemic())[1] > 1 &
       range(num_SIBDQ_Systemic())[2] < 8){
      1}else{
        0}})
  
  outputOptions(output, "SIBDQ_Systemic_buttons", suspendWhenHidden = FALSE)  
  
  
  # Divide SIBDQ general score by 10 if it was entered as a sum score
  num_SIBDQ2 <- reactive(if(range(num_SIBDQ())[2] <= 7){num_SIBDQ()}else{num_SIBDQ()/10})
  
  # Divide SIBDQ Bowel by 3 if it was entered as a sum score
  num_SIBDQ_Bowel2 <- reactive(
    if(input$rangeBowel == "Bowelsum" | range(num_SIBDQ_Bowel())[2] >7){
      num_SIBDQ_Bowel()/3}else{
        num_SIBDQ_Bowel()})
  
  # Divide SIBDQ Emotional by 3 if it was entered as a sum score
  num_SIBDQ_Emotional2 <- reactive(if(input$rangeEmotional == "Emotionalsum" | range(num_SIBDQ_Emotional())[2] > 7){
    num_SIBDQ_Emotional()/3}else{
      num_SIBDQ_Emotional()})
  
  # Divide SIBDQ Social by 2 if it was entered as a sum score
  num_SIBDQ_Social2 <- reactive(if(input$rangeSocial == "Socialsum" | range(num_SIBDQ_Social())[2] > 7){
    num_SIBDQ_Social()/2}else{
      num_SIBDQ_Social()})
  
  # Divide SIBDQ Systemic by 2 if it was entered as a sum score
  num_SIBDQ_Systemic2 <- reactive(if(input$rangeSystemic == "Systemicsum" | range(num_SIBDQ_Systemic())[2] > 7){
    num_SIBDQ_Systemic()/2}else{
      num_SIBDQ_Systemic()})
  
  num_BMI <- reactive(
    if(input$Model == "Model 3.3' (general SIBDQ score, BMI, smoking status, disease type)"){
      as.numeric(unlist(strsplit(input$str_BMI3_3,",")))
    }else{
      as.numeric(unlist(strsplit(input$str_BMI7_5,",")))
    })
  num_Smoker <- reactive(
    if(input$Model == "Model 3.3' (general SIBDQ score, BMI, smoking status, disease type)"){
      as.numeric(unlist(strsplit(input$str_Smoker3_3,",")))
    }else{
      as.numeric(unlist(strsplit(input$str_Smoker7_5,",")))
    })
  num_CD <- reactive(
    if(input$Model == "Model 3.3' (general SIBDQ score, BMI, smoking status, disease type)"){
      as.numeric(unlist(strsplit(input$str_CD3_3,",")))
    }else{
      as.numeric(unlist(strsplit(input$str_CD7_5,",")))
    })
  # 
  # # Define reactive expression for number of observations
  nobs_SIBDQ <- reactive(length(num_SIBDQ())) # number of observations is defined as the length of the num_ vector
  nobs_SIBDQ_Bowel <- reactive(length(num_SIBDQ_Bowel())) # number of observations is defined as the length of the num_ vector
  nobs_SIBDQ_Emotional <- reactive(length(num_SIBDQ_Emotional())) # number of observations is defined as the length of the num_ vector
  nobs_SIBDQ_Social <- reactive(length(num_SIBDQ_Social())) # number of observations is defined as the length of the num_ vector
  nobs_SIBDQ_Systemic <- reactive(length(num_SIBDQ_Systemic())) # number of observations is defined as the length of the num_ vector
  nobs_BMI <- reactive(length(num_BMI())) # number of observations is defined as the length of the num_ vector
  nobs_Smoker <- reactive(length(num_Smoker())) # number of observations is defined as the length of the num_ vector
  nobs_CD <- reactive(length(num_CD())) # number of observations is defined as the length of the num_ vector
  # 
  #
  # Server output for covariates
  output$oid_SIBDQ3_1 <- output$oid_SIBDQ3_3 <- output$oid_SIBDQ7_5 <- renderPrint({
    cat("SIBDQ general score:\n")
    cat(nobs_SIBDQ(),"observations\n")
    print(num_SIBDQ2())
  })
  
  output$oid_BMI3_3 <- output$oid_BMI7_5  <- renderPrint({
    cat("BMI:\n")
    cat(nobs_BMI(),"observations\n")
    print(num_BMI())
  })
  
  output$oid_Smoker3_3 <- output$oid_Smoker7_5 <- renderPrint({
    cat("Smoker:\n")
    cat(nobs_Smoker(),"observations\n")
    print(num_Smoker())
  })
  
  output$oid_CD3_3 <- output$oid_CD7_5 <- renderPrint({
    cat("CD:\n")
    cat(nobs_CD(),"observations\n")
    print(num_CD())
  })
  
  output$oid_SIBDQ_Bowel <- renderPrint({
    cat("SIBDQ subscale Bowel:\n")
    cat(nobs_SIBDQ_Bowel(),"observations\n")
    print(num_SIBDQ_Bowel2())})
  
  
  output$oid_SIBDQ_Emotional <- renderPrint({
    cat("SIBDQ subscale Emotional:\n")
    cat(nobs_SIBDQ_Emotional(),"observations\n")
    print(num_SIBDQ_Emotional2())})
  
  
  output$oid_SIBDQ_Social <- renderPrint({
    cat("SIBDQ subscale Social:\n")
    cat(nobs_SIBDQ_Social(),"observations\n")
    print(num_SIBDQ_Social2())})
  
  
  output$oid_SIBDQ_Systemic <- renderPrint({
    cat("SIBDQ subscale Systemic:\n")
    cat(nobs_SIBDQ_Systemic(),"observations\n")
    print(num_SIBDQ_Systemic2())})
  
  
})

# Create the Shiny app 
shinyApp(u, s)
