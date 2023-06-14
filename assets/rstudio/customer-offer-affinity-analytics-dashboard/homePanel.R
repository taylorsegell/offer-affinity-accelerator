# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# © Copyright IBM Corp. 2019, 2020. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

clientButton <- function(id, name, last_offer, last_offer_status, image) {
  tags$p(
    actionButton(paste0('client-btn-', id),
                 fluidRow(
                   column(3, 
                          tags$img(src = image, width = "100px", height = "100px")
                   ),
                   column(3, align="center", style="padding:40px;",
                          strong(name)#,
                          #tags$h4(product_status)
                   ),
                   column(3, align="center", style="padding:40px;",
                          strong(last_offer)
                          #tags$h3(name),
                          #tags$h4(last_offer)
                   ),
                   column(3, align="center", style="padding:40px;",
                          strong(last_offer_status)
                          #tags$h3(name),
                          #tags$h4(last_offer_status)
                   )
                 ),
                 style="width:100%"
    )
  )
}

homePanel <- function() {
  
  tabPanel(
    "Dashboard",
    tags$head(
      tags$style(HTML("
                      .datatables {
                      width: 100% !important;
                      }
                      "))
      ),
    shinyjs::useShinyjs(),
    
    fluidRow(
      column(5, panel(
        tags$h2("Top Action Clients"),
        tags$br(),
        column(6, align="center", tags$h4("Client")),
        column(3, align="center", tags$h4("Last Product Offered")),
        column(3, align="center", tags$h4("Last Product Purchased?")),
        lapply(clientIds, function(id){
          client <- clients[[toString(id)]]
          clientButton(id, client$name, client$last_offer, client$last_offer_status,
                       paste0("profiles/", client$image))
        })
      ),
      panel(
        h2("Product Opportunities"),
        br(),
        lapply(1:dim(prodOpp)[1], function(index) {
          opportunity <- prodOpp[index,]
          panel(
            column(6,
                   h3(class = "text-center", opportunity$PRODUCT)      
            ),
            column(4, offset = 2, class = "pull-right",
                   br(),
                   p(
                     if(!is.na(opportunity$CLIENT)) {
                       span( style = "color:#0099CC", strong(opportunity$CLIENT, " potential clients"))
                     })
            )
          )
        })
      )
      ),
      column(7, 
             panel(
               h2("Products Offered"),
               plotOutput("productOfferedPlot", width = "600px", height = "400px")
             ),
             panel(
               h2("Percentage of Products Purchased after Offered"),
               plotOutput("productPurchasedPlot", width = "600px", height = "400px")
             )
      )
    )
  )
  }


# Product offered plot
productOfferedData <- data.frame(product=c("Cash", "Brokerage", "Education", "Financial Plan", "Retirement Plan"),
                               customers=c(402, 404, 354, 590, 594))
productOfferedPlot <- ggplot(data=productOfferedData, aes(x=reorder(product, 1:nrow(productOfferedData)), y=customers)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=customers), vjust=-0.3, size=3.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Products", y = "Number of Offers")


# Products Purchased Percentage
productPurchasedData <- data.frame(product=c("Cash", "Brokerage", "Education", "Financial Plan", "Retirement Plan"),
                                 customers=c(38, 63, 27, 85, 72))
productPurchasedPlot <- ggplot(data=productPurchasedData, aes(x=reorder(product, 1:nrow(productPurchasedData)), y=customers)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=paste0(customers, "%")), vjust=-0.3, size=3.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "Products", y = "% Products Purchased")

# Static data for Product Opportunities
Prod <- c("Cash", "Brokerage", "Education", "Financial Plan", "Retirement Plan")
numPotentialClients <- c(3,6,1,8,11)
prodOpp <- data.frame(PRODUCT = Prod, CLIENT = numPotentialClients)


homeServer <- function(input, output, session, sessionVars) {
  
  # Observation events for client buttons
  lapply(paste0('client-btn-', clientIds),
         function(x){
           observeEvent(
             input[[x]],
             {
               id <- as.numeric(sub("client-btn-", "", x))
               sessionVars$selectedClientId <- id
               updateTabsetPanel(session, "proNav", selected = "clientPanel")
             }
           )
         })
  
  # Display plot
  output$productOfferedPlot <- renderPlot(productOfferedPlot, width="auto", height="auto")
  output$productPurchasedPlot <- renderPlot(productPurchasedPlot, width="auto", height="auto")
  
}