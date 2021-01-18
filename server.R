library(shiny)
library(sqldf)
library(dplyr)
library(rlist)
library(shinythemes)


shinyServer <- function(input, output, session) {
  data<- read.csv("join_together_2.csv", header = T)
  
  # Revenue ------------------------------------------------------------------
  ############################################################################
  
  output$for_revenue_column <- renderUI({
    if (is.null(input$revenue_cat))
      return()
    
    source("RevenueList.R")
    
    selectInput( width = '70%', inputId = "revenue_column", 
                 label = tags$div("Select the specific type of revenue you would like to see:",
                                  h5("Note: You can select more than one.", style = "color:black"), style = "color:blue"),
                choices = revenueList[input$revenue_cat], multiple = TRUE)
    
    
  })
  
  # Expenses ------------------------------------------------------------------
  #############################################################################
  
  output$for_expenses_column <- renderUI({
    if (is.null(input$expenses_cat))
      return()
    
    source("ExpensesList.R")
    
    selectInput(width = '70%', inputId = "expenses_column", 
                label = tags$div("Select the specific type of expenditure you would like to see:",
                                 h5("Note: You can select more than one.", style = "color:black"), style = "color:blue"),
                choices = expensesList[input$expenses_cat], multiple = TRUE)
    
    
  })
  
  # Case Services ------------------------------------------------------------------
  ##################################################################################
  output$for_case_cat1 <- renderUI({
    if (is.null(input$case_cat1))
      return()
    
    source("CaseServicesSubcatList.R")
    
    selectInput( width = '70%', inputId = "case_subcat", label = tags$div("Choose a subcategory (optional):", style = "color:blue"),
                 choices = subcatList[input$case_cat1], multiple = FALSE)
    
    
  })
  
  
  intersect_all <- function(...){
    Reduce(intersect, list(...))
  }
  
  source("CaseServicesList2.R")
  
  v <- reactiveValues(choices = c())
  
  observeEvent(input$choose, {
    
    v$choices <- append(v$choices, 
                        intersect_all(list.ungroup(List_problem_cat[input$problem_cat], level = 1), 
                                      list.ungroup(List_PAI_cat[input$PAI_cat], level = 1),
                                      list.ungroup(List_subcat[input$case_subcat], level = 1)
                                      ))
    
  })
  
  
  observeEvent(input$reset, {
    v$choices <- NULL
  })  
  
  output$for_caseSer_column <- renderUI({
    
    selectInput( width = '70%', inputId = "caseSer_column", label = "Case services columns already generated:",
                choices = v$choices, selected = v$choices , multiple = TRUE)
    
    
  })
  
  # Demographics ------------------------------------------------------------------
  ##################################################################################
  
  
  ethnicity_age <- reactive({
    if(input$age_cat == "Blank"){if(input$ethnicity_cat == "Blank"){return()}
      else if(input$ethnicity_cat == "Asian"){return("Asian")}
      else if(input$ethnicity_cat == "Black"){return("Black")}
      else if(input$ethnicity_cat == "Hispanic"){return("Hispanic")}
      else if(input$ethnicity_cat == "Native_American"){return("Native_American")}
      else if(input$ethnicity_cat == "White"){return("White")}
      else if(input$ethnicity_cat == "Other_ethnicity"){return("Other_ethnicity")}
      else{return("Unknown_ethnicity")}
    }
    else if(input$age_cat == "age_0_17"){if(input$ethnicity_cat == "Blank"){return("age_0_17")}
      else if(input$ethnicity_cat == "Asian"){return("Asian_0_17")}
      else if(input$ethnicity_cat == "Black"){return("Black_0_17")}
      else if(input$ethnicity_cat == "Hispanic"){return("Hispanic_0_17")}
      else if(input$ethnicity_cat == "Native_American"){return("Native_American_0_17")}
      else if(input$ethnicity_cat == "White"){return("White_0_17")}
      else if(input$ethnicity_cat == "Other_ethnicity"){return("Other_0_17")}
      else{return("Unknown_0_17")}
    }
    else if(input$age_cat == "age_18_59"){if(input$ethnicity_cat == "Blank"){return("age_18_59")}
      else if(input$ethnicity_cat == "Asian"){return("Asian_18_59")}
      else if(input$ethnicity_cat == "Black"){return("Black_18_59")}
      else if(input$ethnicity_cat == "Hispanic"){return("Hispanic_18_59")}
      else if(input$ethnicity_cat == "Native_American"){return("Native_American_18_59")}
      else if(input$ethnicity_cat == "White"){return("White_18_59")}
      else {return("Other_18_59")}
    }
    else if(input$age_cat == "age_18_35"){if(input$ethnicity_cat == "Blank"){return("age_18_35")}
      else if(input$ethnicity_cat == "Asian"){return("Asian_18_35")}
      else if(input$ethnicity_cat == "Black"){return("Black_18_35")}
      else if(input$ethnicity_cat == "Hispanic"){return("Hispanic_18_35")}
      else if(input$ethnicity_cat == "Native_American"){return("Native_American_18_35")}
      else if(input$ethnicity_cat == "White"){return("White_18_35")}
      else if(input$ethnicity_cat == "Other_ethnicity"){return("Other_18_35")}
      else{return("Unknown_18_35")}
    }
    else if(input$age_cat == "age_36_59"){if(input$ethnicity_cat == "Blank"){return("age_36_59")}
      else if(input$ethnicity_cat == "Asian"){return("Asian_36_59")}
      else if(input$ethnicity_cat == "Black"){return("Black_36_59")}
      else if(input$ethnicity_cat == "Hispanic"){return("Hispanic_36_59")}
      else if(input$ethnicity_cat == "Native_American"){return("Native_American_36_59")}
      else if(input$ethnicity_cat == "White"){return("White_36_59")}
      else if(input$ethnicity_cat == "Other_ethnicity"){return("Other_36_59")}
      else{return("Unknown_36_59")}
    }
    else if(input$age_cat == "age_60_and_Over"){if(input$ethnicity_cat == "Blank"){return("age_60_and_Over")}
      else if(input$ethnicity_cat == "Asian"){return("Asian_60_and_Over")}
      else if(input$ethnicity_cat == "Black"){return("Black_60_and_Over")}
      else if(input$ethnicity_cat == "Hispanic"){return("Hispanic_60_and_Over")}
      else if(input$ethnicity_cat == "Native_American"){return("Native_American_60_and_Over")}
      else if(input$ethnicity_cat == "White"){return("White_60_and_Over")}
      else if(input$ethnicity_cat == "Other_ethnicity"){return("Other_60_and_Over")}
      else{return("Unknown_60_and_Over")}
    }
    else{if(input$ethnicity_cat == "Blank"){return("Unknown_age")}
      else if(input$ethnicity_cat == "Asian"){return("Asian_Unknown")}
      else if(input$ethnicity_cat == "Black"){return("Black_Unknown")}
      else if(input$ethnicity_cat == "Hispanic"){return("Hispanic_Unknown")}
      else if(input$ethnicity_cat == "Native_American"){return("Native_American_Unknown")}
      else if(input$ethnicity_cat == "White"){return("White_Unknown")}
      else if(input$ethnicity_cat == "Other_ethnicity"){return("Other_Unknown")}
      else{return("Unknown_Unknown")}
    }
  })
  
  d <- reactiveValues(choices = c())
  s <- reactiveValues(choices = c())
  
  observeEvent(input$choose_D, {
    d$choices <- append(d$choices, ethnicity_age())
    
    updateSelectInput(session, inputId = "ethnicity_age_cat",
                      label = "Age and ethnicity type columns already chosen:",
                      choices = d$choices, selected = d$choices)
  })
  
  observeEvent(input$reset_D, {
    d$choices <- NULL
    updateSelectInput(session, inputId = "ethnicity_age_cat",
                      label = "Age and ethnicity type columns already chosen:",
                      choices = s, selected = s)
    
  })
  
  # Output Revenue Table ------------------------------------------------------------------
  ########################################################################################
  tableInput_revenue <- reactive({
    #Common columns
    year <- input$year
    state_1 <- input$state
    service_area_1 <- input$service_area
    service_area_type_1 <- input$service_area_type
    
    #Revenue columns
    revenue_column_1 <- input$default_Rev_cat
    revenue_column_2 <- input$revenue_column
    
    #Subset data
    data[which(data$grant_year %in% year & 
                 (data$state %in% state_1 | data$service_area %in% service_area_1) & 
                 data$service_area_type %in% service_area_type_1),
         
         c("org_name", "grant_year", "state", "service_area", "service_area_type",
           revenue_column_1, revenue_column_2)]
    
  })
  
  output$Table_revenue <- renderTable({
    tableInput_revenue()
  })
  
  # Output Expenses Table ------------------------------------------------------------------
  ########################################################################################
  tableInput_expenses <- reactive({
    #Common columns
    year <- input$year
    state_1 <- input$state
    service_area_1 <- input$service_area
    service_area_type_1 <- input$service_area_type
    
    #Expenses columns
    expenses_column_1 <- input$default_exp_cat
    expenses_column_2 <- input$expenses_column
    
    #Subset data
    data[which(data$grant_year %in% year & 
                 (data$state %in% state_1 | data$service_area %in% service_area_1) & 
                 data$service_area_type %in% service_area_type_1),
         
         c("org_name", "grant_year", "state", "service_area", "service_area_type",
           expenses_column_1, expenses_column_2)]
    
  })
  
  output$Table_expenses <- renderTable({
    tableInput_expenses()
  })
  
  
  # Output Case Services Table ------------------------------------------------------------------
  ########################################################################################
  tableInput_problem <- reactive({
    #Common columns
    year <- input$year
    state_1 <- input$state
    service_area_1 <- input$service_area
    service_area_type_1 <- input$service_area_type
    
    #Case services columns
    caseSer_column_1 <- input$default_cat
    caseSer_column_2 <- input$caseSer_column
    
    #Subset data
    data[which(data$grant_year %in% year & 
                 (data$state %in% state_1 | data$service_area %in% service_area_1) & 
                 data$service_area_type %in% service_area_type_1),
         
         c("org_name", "grant_year", "state", "service_area", "service_area_type",
           caseSer_column_1, caseSer_column_2)]
    
  })
  
  output$Table_problem <- renderTable({
    tableInput_problem()
  })
  
  
  # Output Demographics Table ------------------------------------------------------------------
  ########################################################################################
  tableInput_Demo <- reactive({
    #Common columns
    year <- input$year
    state_1 <- input$state
    service_area_1 <- input$service_area
    service_area_type_1 <- input$service_area_type
    
    #Demigraphics columns
    gender1 <- input$default_client_cat
    gender2 <- input$gender_cat
    veteran <- input$veteran_cat 
    
    #Subset data
    data[which(data$grant_year %in% year & 
                 (data$state %in% state_1 | data$service_area %in% service_area_1) & 
                 data$service_area_type %in% service_area_type_1),
         
         c("org_name", "grant_year", "state", "service_area", "service_area_type",
           gender1, gender2, veteran, input$ethnicity_age_cat)]
    
  })
  
  output$Table_demo <- renderTable({
    tableInput_Demo()
  })
  
  # Output Table ------------------------------------------------------------------
  #################################################################################
  
  output$Table <- renderTable({
    tableInput()
  })
  
  
  tableInput <- reactive({
    #Common columns
    year <- input$year
    state_1 <- input$state
    service_area_1 <- input$service_area
    service_area_type_1 <- input$service_area_type
    
    #Revenue columns
    revenue_column_1 <- input$default_Rev_cat
    revenue_column_2 <- input$revenue_column
    
    #Expenses columns
    expenses_column_1 <- input$default_exp_cat
    expenses_column_2 <- input$expenses_column
    
    #Case services columns
    caseSer_column_1 <- input$default_cat
    caseSer_column_2 <- input$caseSer_column
    
    #Demigraphics columns
    gender1 <- input$default_client_cat
    gender2 <- input$gender_cat
    veteran <- input$veteran_cat 
    #ethnicity_age_cat1 <- input$ethnicity_age_cat
    
    #Subset data
    data[which(data$grant_year %in% year & 
                 (data$state %in% state_1 | data$service_area %in% service_area_1) & 
                 data$service_area_type %in% service_area_type_1),
         
         c("org_name", "grant_year", "state", "service_area", "service_area_type",
           revenue_column_1, revenue_column_2, expenses_column_1, expenses_column_2, caseSer_column_1, caseSer_column_2,
           gender1, gender2, veteran, input$ethnicity_age_cat)
         ]
    
  })
  
  
  
  
  output$Table <- renderTable({
    tableInput()
  })
  
  
  # Download ------------------------------------------------------------------ 
  #############################################################################
  
  output$Download <- downloadHandler(
    filename = function() {
      paste("RShiny_download", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tableInput(), file, row.names = FALSE)
    }
  )
 
}



