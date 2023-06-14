# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# © Copyrig ht IBM Corp. 2019, 2020. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

# Load Data from CSV files
library(readr)
library(scales)

readDataset <- function(fileName) {read.csv(file.path(fileName))}

customer <- readDataset("customer.csv")

clients <- list(
  list(name="Paige Carson", image="1F.jpg", last_offer="Cash Product", last_offer_status="Purchased"),
  list(name="Alex Anderson", image="2M.jpg", last_offer="Brokerage", last_offer_status="Purchased"),
  list(name="Ian Gray", image="3M.jpg", last_offer="No previous offers", last_offer_status=""),
  list(name="Jane Wilson", image="8F.jpg", last_offer="Education", last_offer_status="No Purchase"),
  list(name="Robert Taylor", image="4M.jpg", last_offer="Financial Planning", last_offer_status="Purchased")
)
clientIds <- c(1039, 1040, 1041, 1023, 1011)
names(clients) <- clientIds

for(id in clientIds) {
  clients[[toString(id)]]$income <- dollar(customer[customer$CUSTOMER_ID == id,][[1,'ANNUAL_INCOME']])
}