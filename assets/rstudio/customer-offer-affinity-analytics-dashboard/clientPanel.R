# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# © Copyright IBM Corp. 2019, 2020. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

# Load shap JavaScript
shapjs <- httr::content(GET("https://github.com/slundberg/shap/raw/0849aa20551cf9825f9e294fcc29d7fbe7b9f932/shap/plots/resources/bundle.js"))

clientPanel <- function() {
  
  tabPanel(
    "Client View",
    value = "clientPanel",
    
    panel(
      br(),br(),
      fluidRow(
        column(3, class = "pull-left",
               div(id = "customerImage", class = "img-circle"),
               h2(class = "text-center" ,textOutput("customerName"))),
        column(3, 
               h4("Personal Information", class="text-center"),
               hr(),
               uiOutput("customerInfo")
        ),
        column(3, 
               h4("Financial Profile", class="text-center"),
               hr(),
               uiOutput("customerFinancesInfo")
        )
      ),
      br()
    ),
    
    panel(
      h3("Offer Affinity Prediction "),
      br(),
      
     
      
      tags$div(
        id = "authPanel",
        column(4,
               panel(
                 h4("Connect to Cloud Pak for Data API"),
                 textInput("hostname", "CPD Hostname"),
                 textInput("username", "CPD Username"),
                 passwordInput("password", "CPD Password"),
                 actionButton("authBtn", "Authenticate API", class = "btn-primary btn-lg btn-block", style = "max-width:300px", disabled = TRUE),
                 tags$head(tags$style("#authError{color:red;}")),
                 verbatimTextOutput("authError")
               ),
               style = "max-width:360px;"
        )
      ),
      hidden(
        tags$div(
          id = "deploymentPanel",
          column(4,
                 panel(
                   tags$h4("Model Scoring Pipeline Deployment"),
                   pickerInput(
                     inputId = 'deploymentSelector',
                     label = 'Deployment Name:',
                     choices = list(),
                     options = pickerOptions(width = "auto", style = "btn-primary")
                   ),
                   tags$p(
                     tags$strong("Space Name: "),
                     textOutput(outputId = "space_name", inline = TRUE),
                     
                   ),
                   tags$p(
                     tags$strong("GUID: "),
                     textOutput(outputId = "deployment_guid", inline = TRUE)
                   ),
                   tags$p(
                     tags$strong("Scoring Endpoint: "),
                     textOutput(outputId = "scoring_url", inline = TRUE),
                     style = "word-wrap: break-word"
                   )),
                 panel(
                   actionButton(
                     "reauthenticateBtn",
                     "Re-Authenticate",
                     class = "btn-primary btn-lg btn-block"
                   ) 
                 )
          ),
          tags$div(id = "scoreBtnSection",
                   column(4,
                          br(),
                          actionButton(
                            "scoreBtn",
                            "Predict Offer Affinity",
                            class = "btn-primary btn-lg btn-block",
                            disabled = TRUE
                          ),
                          br(),
                          h4("Input JSON:"),
                          verbatimTextOutput("pipelineInput"),
                          br(),
                          tags$head(tags$style("#scoringError{color:red;}")),
                          verbatimTextOutput("scoringError"))
          ),
          column(8,
                 hidden(
                   tags$div(id = "scoringResponse")
                 )
          )
        )
      )
      )
  )
  }






# Reactive server variables store (pervades across all sessions)
serverVariables = reactiveValues(deployments = list(), token = '')

if(nchar(Sys.getenv('CP4D_HOSTNAME')) > 0 && nchar(Sys.getenv('CP4D_USERNAME')) > 0 && nchar(Sys.getenv('CP4D_PASSWORD')) > 0) {
  tryCatch({
    deploymentsResp = collectDeployments(Sys.getenv('CP4D_HOSTNAME'), Sys.getenv('CP4D_USERNAME'), Sys.getenv('CP4D_PASSWORD'), "offer_affinity_scoring_pipeline_function_deployment")
    serverVariables$deployments <- deploymentsResp$deployments
    serverVariables$token = deploymentsResp$token
  }, warning = function(w) {
    print(w$message)
  }, error = function(e) {
    print(e$message)
  })
}

clientServer <- function(input, output, session, sessionVars) {
  
  observe({
    
    client <- clients[[toString(sessionVars$selectedClientId)]]
    
    # Update client name & image
    output$customerName <- renderText(client$name)
    removeUI(selector = "#customerImage > *")
    insertUI(
      selector = "#customerImage",
      where = "beforeEnd",
      ui = img(id=paste0("client-img-", sessionVars$selectedClientId),src = paste0("profiles/",client$image), style = "display: block;margin-left: auto;margin-right: auto;", width=150, height=150)
    )
    
    # Load customer data for customer sessionVars$selectedClientId
    selection <- customer[customer$CUSTOMER_ID == sessionVars$selectedClientId,][1,]
    
    # Table displays for Customer View
    # output$customerTable <- renderTable({
    #   demoDeets <- selection[,c("CUSTOMER_ID", "AGE_RANGE", "MARITAL_STATUS", "FAMILY_SIZE", "PROFESSION", "EDUCATION_LEVEL")]
    #   demoDeets[["CUSTOMER_ID"]] <- as.integer(demoDeets[["CUSTOMER_ID"]])
    #   demoDeets[["FAMILY_SIZE"]] <- as.integer(demoDeets[["FAMILY_SIZE"]])
    #   demoDeets[["ADDRESS"]] <- paste(selection[,"ADDRESS_HOME_CITY"], selection[,"ADDRESS_HOME_STATE"], sep = ', ')
    #   demoDeets[,c("CUSTOMER_ID", "AGE_RANGE", "ADDRESS", "MARITAL_STATUS", "FAMILY_SIZE", "PROFESSION", "EDUCATION_LEVEL")]
    # }, bordered = TRUE, align = 'l')
    # 
    # output$customerFinancesTable <- renderTable({
    #   finDeets <- selection[,c("ANNUAL_INCOME", "HOME_OWNER_INDICATOR", "MONTHLY_HOUSING_COST", "CREDIT_SCORE", "CREDIT_AUTHORITY_LEVEL")]
    #   finDeets[["ANNUAL_INCOME"]] <- dollar(finDeets[["ANNUAL_INCOME"]])
    #   finDeets[["MONTHLY_HOUSING_COST"]] <- dollar(finDeets[["MONTHLY_HOUSING_COST"]])
    #   finDeets
    # }, bordered = TRUE, align = 'l')
    # 
    
    output$customerInfo <- renderUI({
      infoDets <- selection[,c("CUSTOMER_ID", "AGE_RANGE", "MARITAL_STATUS", "FAMILY_SIZE", "PROFESSION", "EDUCATION_LEVEL", "PREFERRED_COMMUNICATION_FORM")]
      infoDets[["FAMILY_SIZE"]] <- as.integer(infoDets[["FAMILY_SIZE"]])
      infoDets[["ADDRESS"]] <- paste(selection[,"ADDRESS_HOME_CITY"], selection[,"ADDRESS_HOME_STATE"], sep = ', ')
      tags$ul( class = 'list-unstyled',
               tags$li(
                 tags$strong('Customer ID: '), tags$span(class = "pull-right", infoDets[["CUSTOMER_ID"]])
               ),
               tags$li(
                 tags$strong('Age: '), tags$span(class = "pull-right", infoDets[["AGE_RANGE"]], ' years old')
               ),
               tags$li(
                 tags$strong('Marital Status: '),tags$span(class = "pull-right", infoDets[["MARITAL_STATUS"]])
               ),
               tags$li(
                 tags$strong('Address: '), tags$span(class = "pull-right", infoDets[["ADDRESS"]])
               ),
               tags$li(
                 tags$strong('Profession: '), tags$span(class = "pull-right",infoDets[["PROFESSION"]])
               ),
               tags$li(
                 tags$strong('Level of Education: '), tags$span(class = "pull-right", infoDets[["EDUCATION_LEVEL"]])
               ),
               tags$li(
                 tags$strong('Preferred form of Communication: '), tags$span(class = "pull-right", infoDets[["PREFERRED_COMMUNICATION_FORM"]])
               )
      )
    })
    output$customerFinancesInfo <- renderUI({
      customerfinDets <- selection[,c("ANNUAL_INCOME", "HOME_OWNER_INDICATOR", "MONTHLY_HOUSING_COST", "CREDIT_SCORE", "CREDIT_AUTHORITY_LEVEL", "PURSUIT")]
      customerfinDets[["ANNUAL_INCOME"]] <- dollar(customerfinDets[["ANNUAL_INCOME"]])
      customerfinDets[["MONTHLY_HOUSING_COST"]] <- dollar(customerfinDets[["MONTHLY_HOUSING_COST"]])
      tags$ul( class = 'list-unstyled',
               tags$li(
                 tags$strong('Annual income: '), tags$span(class="pull-right", customerfinDets[["ANNUAL_INCOME"]])
               ),
               tags$li(
                 tags$strong('Home Owner: '), tags$span(class="pull-right", if(customerfinDets[["HOME_OWNER_INDICATOR"]] == TRUE) { 'Yes'} else { 'No'})
               ),
               tags$li(
                 tags$strong('Monthly Housing: '), tags$span(class="pull-right", customerfinDets[["MONTHLY_HOUSING_COST"]])
               ),
               tags$li(
                 tags$strong('Credit Score: '), tags$span(class="pull-right", round(customerfinDets[["CREDIT_SCORE"]], 0))
               ),
               tags$li(
                 tags$strong('Credit Authority Level: '), tags$span(class="pull-right", customerfinDets[["CREDIT_AUTHORITY_LEVEL"]])
               ),
               tags$li(
                 tags$strong('Customer Pursuit: '), tags$span(class="pull-right", customerfinDets[["PURSUIT"]])
               )
      )
    })
    
    # Reset scoring
    removeUI(selector = "#scoringResponse > *", multiple = TRUE)
    shinyjs::hide(id = "scoringResponse")
    shinyjs::show(id = "scoreBtnSection")
    output$scoringError <- renderText('')
    sessionVars$pipelineInput <- list(cust_id = sessionVars$selectedClientId, values = '2018-09-30')
    output$pipelineInput <- renderText(toJSON(sessionVars$pipelineInput, indent = 2))
  })
  
  # Set default hostname for ICP4D API
  observeEvent(session$clientData$url_hostname, {
    updateTextInput(session, "hostname", value = session$clientData$url_hostname)
  })
  
  # Enable buttons when inputs are provided
  observe({
    toggleState("authBtn", nchar(input$hostname) > 0 && nchar(input$username) > 0 && nchar(input$password) > 0)
    toggleState("scoreBtn", nchar(input$endpoint) > 0 && nchar(input$token) > 0 && length(input$allCustomers_rows_selected) > 0)
  })
  
  # Handle ICP4D API authentication button
  observeEvent(input$authBtn, {
    shinyjs::disable("authBtn")
    
    tryCatch({
      deploymentsResp = collectDeployments(input$hostname, input$username, input$password, "offer_affinity_scoring_pipeline_function_deployment")
      serverVariables$deployments <- deploymentsResp$deployments
      serverVariables$token = deploymentsResp$token
    }, warning = function(w) {
      output$authError <- renderText(w$message)
    }, error = function(e) {
      output$authError <- renderText(e$message)
    })
    
    shinyjs::enable("authBtn")

    if(length(serverVariables$deployments) > 0) {
      updateSelectInput(session, "deploymentSelector", choices = names(serverVariables$deployments))
      shinyjs::hide(id = "authPanel")
      shinyjs::show(id = "deploymentPanel")
    }
  })
  
  # Handle model deployment dropdown switching
  observeEvent(input$deploymentSelector, {
    selectedDeployment <- serverVariables$deployments[[input$deploymentSelector]]
    output$deployment_guid <- renderText(selectedDeployment$guid)
    output$space_name <- renderText(selectedDeployment$space_name)

    output$scoring_url <- renderText(selectedDeployment$scoring_url)
    toggleState("scoreBtn", nchar(selectedDeployment$scoring_url) > 0 && nchar(serverVariables$token) > 0)
  })
  
  # Handle model deployment scoring button
  observeEvent(input$scoreBtn, {
    shinyjs::disable("scoreBtn")
    
    selectedDeployment <- serverVariables$deployments[[input$deploymentSelector]]
    
    payload = list(
      values = '2018-09-30',
      cust_id = sessionVars$selectedClientId
    )
    
    response <- scoreModelDeployment(selectedDeployment$scoring_url, payload, serverVariables$token)
    
    if(length(response$error) > 0) {
      output$scoringError <- renderText(toString(response$error))
    }
    else if(length(response$predictions) > 0) {
      shinyjs::hide(id = "scoreBtnSection")
      shinyjs::show(id = "scoringResponse")
    #Making it output in descending order  
    ordered <- data.frame(id = character(5), prod_name = character(5), probi = numeric(5), stringsAsFactors = FALSE)
    i <- 1
    for(product_id in names(response$predictions[[1]]$values)){
      ordered$id[i] <- product_id
      
      if (product_id == "FINANCIALPLAN") {
        ordered$prod_name[i] <- "Financial Plan"
      }
      else if (product_id == "RETIREMENTPLAN") {
        ordered$prod_name[i] <- "Retirement Plan"
      }
      else if (product_id == "BROKERAGE") {
        ordered$prod_name[i] <- "Brokerage"
      }      
      else if (product_id == "EDUCATION") {
        ordered$prod_name[i] <- "Education"
      }              
      else if (product_id == "CASH") {
        ordered$prod_name[i] <- "Cash"
      }      
      ordered$probi[i] <- response$predictions[[1]]$values[[product_id]]$predictions[[1]]$values[[1]][[2]][[2]]
      i <- i+1
    }
    
     ordered <- arrange(ordered, desc(probi))
      for(product_id in ordered$id) {

        #product_result <- response$predictions[[1]]$values$predictions[[1]][[product_id]]
        # has_explain <- length(product_result$explain_plot_html) > 0
        # 
        # explain_panel1 <- p()
        # explain_panel2 <- div()
        # if(has_explain) {
        #   explain_panel1 <- p(
        #     tableOutput(paste0("explain-table-",product_id))
        #   )
        #   explain_panel2 <- div(
        #     p(
        #       strong("Explanation Plot: ")
        #     ),
        #     HTML(product_result$explain_plot_html)
        #   )
        # }
        
        insertUI(
          selector = "#scoringResponse",
          where = "beforeEnd",
          ui = panel(id = paste0("client-recom-panel-", which(ordered$id == product_id)),
            h3(paste("Recommendation #", paste0(which(ordered$id == product_id), ": ", ordered[ordered$id == product_id,]["prod_name"], " Product"))),
            p(
                     plotOutput(paste0("probPlot-",product_id), width = "600px", height = "400px")
             )
            
            #explain_panel2
          )
        )
        
        #if(has_explain) {
          # send responseInserted message
        #  session$sendCustomMessage('responseInserted',
        #                            list(
        #                              id=product_result$explain_plot_elem_id,
        #                              data=fromJSON(product_result$explain_plot_data))
        #  )
        #}
      }
      
      lapply(names(response$predictions[[1]]$values), function(product_id) {
  #      
        product_result <- response$predictions[[1]]$values[[product_id]]$predictions[[1]]
  #      has_explain <- length(product_result$explain_plot_html) > 0
  #      
  #      if(has_explain) {
  #        # render high impact features table
  #        vertical <- t(data.frame(product_result$explain))
  #        Impact <- vertical[order(vertical, decreasing = TRUE),]
  #        
  #        simpleCap <- function(x) {
  #          s <- strsplit(x, " ")[[1]]
  #          paste(toupper(substring(s, 1,1)), substring(s, 2),
  #               sep="", collapse=" ")
  #       }
  #        df <- data.frame(Impact)
  #        new_names <- row.names(df)
  #        i <- 1
  #        for(name in new_names){
  #          nicerName <- paste(unlist(strsplit(name, "_", fixed=TRUE)))
  #          nicerName <- paste(unlist(strsplit(nicerName, ".", fixed = TRUE)))
  #          nicerName <- tolower(nicerName)
  #          nicerName <- gsub("customer", "", nicerName, fixed=TRUE)
  #          nicerName <- gsub("summary", "", nicerName, fixed=TRUE)
  #          nicerName <- nicerName[nicerName != ""]
  #          nicerName <- paste(nicerName, collapse = " ")
  #          nicerName <- sapply(nicerName, simpleCap)
  #          new_names[i] <- nicerName
  #          i <- i+1
  #        }
  #        
  #        row.names(df) <- new_names
  #        dispTable <- tibble::rownames_to_column(df, "Highest Impact Features")
  #       output[[paste0("explain-table-",product_id)]] <- renderTable(dispTable, bordered = TRUE)
  #      }
        
        # generate probability pie
        probDF <- data.frame(t(data.frame(product_result$values[[1]][[2]])))
        colnames(probDF) <- "Probability"
        row.names(probDF) <- c("FALSE", "TRUE")
        probDF <- tibble::rownames_to_column(probDF, "Prediction")
        probDF <- probDF %>%
          mutate(percentage = paste0(round(100 * Probability, 1), "%")) %>%
          mutate(hover_text = paste0(Prediction, ": ", percentage))
        
        probPlot <- ggplot(probDF, aes(y = Probability, fill = Prediction)) +
          geom_bar(
            aes(x = 1),
            width = 0.4,
            stat = "identity",
            show.legend = TRUE
          ) + 
          annotate("text", x = 0, y = 0, size = 12,
                   label = probDF[["percentage"]][probDF[["Prediction"]] == "TRUE"]
          ) +
          coord_polar(theta = "y") +
          theme_void() +
          theme(legend.title=element_text(size=22),
                legend.text=element_text(size=16)) +
          guides(fill = guide_legend(reverse=TRUE))
        
        output[[paste0("probPlot-",product_id)]] <- renderPlot(probPlot,width="auto", height="auto", bg="transparent")
      })
      
    } else {
      output$scoringError <- renderText(response)
    }
    
    shinyjs::enable("scoreBtn")
  })
  observeEvent(input$reauthenticateBtn, {
    shinyjs::show(id = "scoreBtnSection")
    removeUI(selector = "#scoringResponse > *", multiple = TRUE)
    shinyjs::show(id = "authPanel")
    shinyjs::hide(id = "deploymentPanel")
  })
}