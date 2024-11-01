# Load libraries
print("loading shiny")
library(shiny)
print("loading shinyWidgets")
library(shinyWidgets)
print("loading shinyjs")
library(shinyjs)
print("loading uuid")
library(uuid)   # For generating unique tokens
print("loading Seurat")
library(Seurat)
print("loading HDF5Array")
library(HDF5Array)
print("loading dplyr")
library(dplyr)
print("loading tidydr")
library(tidydr)
print("loading tibble")
library(tibble)
print("loading ggplot2")
library(ggplot2)
print("loading patchwork")
library(patchwork)
print("loading purrr")
library(purrr)
print("loading ggpubr")
library(ggpubr)
print("loading ComplexHeatmap")
library(ComplexHeatmap)
print("loading circlize")
library(circlize)
print("loading tidyverse")
library(tidyverse)
print("loading ggh4x")
library(ggh4x)
print("loading RColorBrewer")
library(RColorBrewer)
print("loading DBI")
library(DBI)
print("loading RMySQL")
library(RMySQL)
print("loading digest")
library(digest) # For hashing passwords
print("loading dotenv")
library(dotenv)
print("loading mailR")
library(mailR)
print("loading glue")
library(glue)
print("loading pool")
library(pool)

# Load functions
source("code/sources/computeUMAP.R")
source("code/sources/computeFeatureUMAP.R")
source("code/sources/computeBarPlot_CellType_Status.R")
source("code/sources/computeVln.R")
source("code/sources/computeHeatmap.R")
source("code/sources/computeStackBarPlot_Subject.R")
source("code/sources/computeFeaturePert.R")
source("code/sources/computeDotPlot.R")
source("code/sources/seurat_analysis_utilities.R")

# Determine the environment and load the appropriate .env file
env <- Sys.getenv("APP_ENV", unset = "local")
cat("env:", env, "\n")

# Load environment variables
db_host <- Sys.getenv(paste0("DB_HOST_", toupper(env)))
db_port <- Sys.getenv("DB_PORT")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_name <- Sys.getenv("DB_NAME")

shiny_port <- as.integer(Sys.getenv("SHINY_PORT"))
shiny_host <- Sys.getenv("SHINY_HOST")

email_server <- Sys.getenv("EMAIL_SERVER")
email_port <- as.integer(Sys.getenv("EMAIL_PORT"))
email_address <- Sys.getenv("EMAIL_ADDRESS")
email_user <- Sys.getenv("EMAIL_USER")
email_password <- Sys.getenv("EMAIL_PASSWORD")

# Select the appropriate APP_DOMAIN based on the environment
app_domain <- Sys.getenv(paste0("APP_DOMAIN_", toupper(env)))

# Print loaded environment variables for debugging
cat("Loaded environment variables:\n")
cat("db_host:", db_host, "\n")
cat("db_port:", db_port, "\n")
cat("db_user:", db_user, "\n")
cat("db_password:", db_password, "\n")
cat("db_name:", db_name, "\n")
cat("shiny_port:", shiny_port, "\n")
cat("shiny_host:", shiny_host, "\n")
cat("email_server:", email_server, "\n")
cat("email_port:", email_port, "\n")
cat("email_user:", email_user, "\n")
cat("app_domain:", app_domain, "\n")

# Debugging Shiny Server version comparison
# Check the Shiny Server version
shiny_server_version <- Sys.getenv('SHINY_SERVER_VERSION')
cat("shiny_server_version:", shiny_server_version, "\n")

# Handle missing or invalid version strings
if (is.na(shiny_server_version)) {
  cat("SHINY_SERVER_VERSION is not set\n")
} else {
  cat("SHINY_SERVER_VERSION is set to", shiny_server_version, "\n")
}
