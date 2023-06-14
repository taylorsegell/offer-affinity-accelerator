# Title     : Offer Affinity Prediction Dashboard
# Objective : Demonstrate Offer Affinity Prediction Industry Accelerator for Data Science Use Cases

# Sample Materials, provided under license.
# Licensed Materials - Property of IBM
# © Copyright IBM Corp. 2019, 2020. All Rights Reserved.
# US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.

### Install any missing packages ###

# Determine packages to install among requirements
list.of.packages <- c("shinyjs", "dplyr","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) {  # check if there's anything to install
  
  # set default libpath
  if (Sys.getenv("DSX_PROJECT_DIR")!=""){                           # If we are in WSL,
    target <- paste0(Sys.getenv("DSX_PROJECT_DIR"),"/packages/R")   # default to project packages/R
  } else {                                                          # Otherwise,
    target <- .libPaths()[1]                                        # default to first libPath (default)
  }
  
  # check for other valid libpaths
  for(libpath in .libPaths()) {           # check the .libPaths
    if(file.access(libpath, 2) == 0) {    # if we have write access to a libpath, use it
      target <- libpath
      break
    }
  }
  
  # Install the packages
  print(paste("Installing ", paste(new.packages, collapse = ", "), "to", target))
  install.packages(new.packages, lib = target,repos = "http://cran.us.r-project.org")
}


# if the packages that get installed have a different version number to development, install the correct version
if((!"shinyWidgets" %in% rownames(installed.packages()))||((packageVersion("shinyWidgets")!= "0.4.9")))
{
  packageUrl <- "https://cran.r-project.org/src/contrib/Archive/shinyWidgets/shinyWidgets_0.4.9.tar.gz"
  install.packages(packageUrl, repos = NULL, type='source')
}


# Load required packages
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(httr)
library(rjson)
library(ggplot2)
library(dplyr)

# Load datasets & Cloud Pak for Data API functions
source("lib/load-data.R")
source("lib/icp4d-api.R")

# Load panels
source("homePanel.R")
source("clientPanel.R")

#if(packageVersion("dplyr")!= "0.8.5") { packageUrl <- "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.8.5.tar.gz" install.packages(packageUrl, repos = NULL, type='source') packageVersion("dplyr") }
# Serve customer profile images
#addResourcePath('profiles', normalizePath('../../misc/rshiny/profiles'))



ui <- navbarPage(
  "Offer Affinity Prediction",
  id = "proNav",
  inverse = TRUE,
  responsive = TRUE,
  collapsible = TRUE,
  theme = "css/style.css",
  homePanel(),
  clientPanel()
)

server <- function(input, output, session) {
  
  sessionVars <- reactiveValues(selectedClientId = 1039)
  
  homeServer(input, output, session, sessionVars)
  clientServer(input, output, session, sessionVars)
  

}

# Create Shiny app
shinyApp(ui, server)