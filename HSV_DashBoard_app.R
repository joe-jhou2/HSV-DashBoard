# Here is workaround to use the same code for both local and docker deployment
# to handle the image path. The image_prefix variable is set based on the
# environment variable DOCKER_ENV. If DOCKER_ENV is set to true, the image_prefix
# is set to "www/" to point to the images in the www directory. Otherwise, the
# image_prefix is set to an empty string. The image_prefix is then passed to the
# signup_ui function to render the images correctly.
if (Sys.getenv("DOCKER_ENV") == "true") {
  shiny::addResourcePath('www', '/home/shiny/www')
  image_prefix <- "www/"
} else {
  image_prefix <- ""
}

# Source global setup and modules
source("code/global.R")  # This includes sourcing emailSignupModule.R

# Source the first logging in page UI and server functions
source("code/signUI.R")
source("code/signServer.R")

# Source the main page UI and server scripts
source("code/mainUI.R")
source("code/mainServer.R")


# UI
ui <- fluidPage(
  uiOutput("page_content")
)

# Server
server <- function(input, output, session) {
  # Establish database connection
  con <- dbConnect(
    RMySQL::MySQL(),
    dbname = db_name,
    host = db_host,
    user = db_user,
    password = db_password,
    port = as.integer(db_port)
  )
  
  # User authentication state
  user_auth <- reactiveVal(FALSE)
  user_email <- reactiveVal(NULL)
  
  # Track sign-up status
  signed_up <- reactiveVal(FALSE)
  
  # Render either login page or main page based on authentication state
  output$page_content <- renderUI({
    if (!user_auth()) {
      signup_ui("login", image_prefix)
    } else {
      main_ui("main")
    }
  })
  
  # Call SignupServer module
  callModule(signup_server, "login", con, user_auth, user_email, signed_up)
  
  # Call main module server when user is authenticated
  observeEvent(user_auth(), {
    if (user_auth()) {
      main_server(input, output, session)
    }
  })
  
  # Sign out logic (listen for sign out button)
  observeEvent(input$signOutBtn, {
    print("Sign out button clicked")
    
    # Reset user authentication and email
    user_auth(FALSE)
    user_email(NULL)
    # signed_up(FALSE)
    
    # Reset the URL query string (optional)
    shiny::updateQueryString("?", mode = "replace")
    
    # Reload the session to clear out any session-specific state
    session$reload()
  })
  
  # Close database connection when app closes
  onStop(function() {
    dbDisconnect(con)
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server, options = list(host = shiny_host, port = shiny_port))
