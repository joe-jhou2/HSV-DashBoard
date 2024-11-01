signup_server <- function(input, output, session, con, loginStatus, user_email, signed_up) {
  # ns <- session$ns
  
  #----------------------------------------------------------------------------#
  # Reactive value to track sign-up status
  # signed_up <- reactiveVal(FALSE)
  
  # Log In Handler
  observeEvent(input$logIn, {
    email <- input$email
    password <- input$password
    print(paste("User attempting to log in:", email))
    
    login_result <- check_login(email, password, con)
    print(paste("Login result:", login_result))

    if (login_result == "success") {
      loginStatus(TRUE)  # Update loginStatus to TRUE
      user_email(email)  # Update user_email reactive value
      # signed_up(TRUE)  # Set signed up to TRUE
      
      output$message_login <- renderUI({
        div(style = "color: green;", "Login successful!")
      })
    } else if (login_result == "not_activated") {
      loginStatus(FALSE)  # Set loginStatus to FALSE if login fails
      output$message_login <- renderUI({
        div(style = "color: red;", "Account not activated.")
      })
    } else if (login_result == "not_signed_up") {
      loginStatus(FALSE)  # Set loginStatus to FALSE if login fails
      output$message_login <- renderUI({
        div(style = "color: red;", "You need to sign up first.")
      })
    } else if (login_result == "invalid_credentials") {
      loginStatus(FALSE)  # Set loginStatus to FALSE if login fails
      output$message_login <- renderUI({
        div(style = "color: red;", "Invalid password.")
      })
    }
  })
  
  # Helper function - check if user already login
  check_login <- function(email, password, con) {
    query <- glue_sql("
  SELECT id, password_hash, is_active 
  FROM users 
  WHERE email = {email}",
                      email = email, 
                      .con = con)
    
    result <- dbGetQuery(con, query)
    
    if (nrow(result) == 0) {
      return("not_signed_up")
    } else if (result$is_active == 0) {
      return("not_activated")
    }else if (result$is_active == 1 && digest(password, algo = "sha256") == result$password_hash[1]) {
      user_id <- result$id[1]
      store_login(user_id)  # Store login time (helper function)
      return("success")
    } else if (result$is_active == 1 && digest(password, algo = "sha256") != result$password_hash[1]) {
      return("invalid_credentials")
    }
  }
  
  # Help function - store login time
  store_login <- function(user_id) {
    query <- glue_sql("
    INSERT INTO logins (user_id, login_time) 
    VALUES ({user_id}, NOW())",
                      user_id = user_id, 
                      .con = con)
    
    dbExecute(con, query)
  }
  
  #----------------------------------------------------------------------------#
  # Sign up Handler
  observeEvent(input$signUp, {
    email <- input$email
    password <- input$password
    
    # Check if the email is already in the database and handle activation
    if (email != "" && password != "") {
      user_info <- user_exist_check(email, con)
      
      if (nrow(user_info) > 0) {
        if (user_info$is_active == 0) {
          # If the user exists but is inactive, resend activation email
          send_activation_email(email, user_info$activation_token, con)
          output$message_login <- renderText("A validation email has been resent.\nPlease check your email.")
        } else {
          output$message_login <- renderText("Your account is already active.\nPlease log in.")
        }
      } else {
        # If the user does not exist, create a new user
        activation_token <- UUIDgenerate()  # Generate a unique activation token
        password_hash <- digest(password, algo = "sha256")  # Hash the password
        
        # Store the new user's details
        store_user(email, password_hash, activation_token, con)
        
        # Send the activation email
        send_activation_email(email, activation_token, con)
        
        output$message_login <- renderText("A validation email has been sent. Please check your email.")
      }
      
      # Handle user status (disable analysis features)
      output$mainContent <- renderUI({
        div("Please validate your email to access analysis features.")
      })
    } else {
      output$message_login <- renderText("Please enter a valid email address and password.")
    }
  })
  
  # Helper function - Check if users exist 
  user_exist_check <-  function(email, con){
    # Check if the email exists in the database and is inactive
    query <- glue_sql("
    SELECT activation_token, is_active 
    FROM users 
    WHERE email = {email}",
                      email = email,
                      .con = con)
    
    result <- dbGetQuery(con, query)
    return(result)
  }
  
  # Helper function - Send validation email 
  send_activation_email <- function(email, token, con) {
    # Define email parameters
    from <- email_address
    
    # Get the current port and domain dynamically
    port <- session$clientData$url_port
    app_domain <- app_domain
    
    # Construct the full URL dynamically
    activation_link <- paste0(app_domain, ":", port, "/?token=", URLencode(token))
    # print(activation_link)
    
    # Prepare the activation email body
    subject <- "HSV Dashboard App - Please validate your account"
    body <- paste(
      "<p>Click the link below to activate your account:</p>",
      "<a href='", activation_link, "'>Activate Account</a>"
    )
    
    # Send the email
    tryCatch({
    mailR::send.mail(
      from = from,
      to = email,
      subject = subject,
      body = body,
      smtp = list(
        host.name = email_server,
        port = email_port,
        user.name = email_user,
        passwd = email_password,
        ssl = TRUE
      ),
      # smtp = list(host.name = email_server, port = email_port),
      authenticate = TRUE,
      html = TRUE,
      send = TRUE
    )
      print("Activation email sent successfully.")
    }, error = function(e) {
      print(paste("Error sending email:", e$message))
    })
  }
  
  # Help function - save user into database
  store_user <- function(email, password_hash, token, con) {
    query <- glue_sql("
    INSERT INTO users (email, password_hash, activation_token, is_active) 
    VALUES ({email}, {password_hash}, {token}, {is_active})",
                      email = email,
                      password_hash = password_hash,
                      token = token,
                      is_active = 0,
                      .con = con)
    
    result <- tryCatch({
      dbExecute(con, query)
    }, error = function(e) {
      print(paste("Error storing user:", e$message))
      NULL
    })
    
    if (!is.null(result)) {
      print("User stored successfully.")
    }
  }
  
  #----------------------------------------------------------------------------#
  # Activation Handler
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # print(paste("URL parameters:", session$clientData$url_search))
    # print(paste("query:", query))
    
    if ("token" %in% names(query)) {
      token <- query$token
      print(paste("Activation token received:", token))
      
      if (validate_token(token, con)) {
        # print("Token validated successfully")
        if (activate_user(token, con)) {
          # print("User activated successfully")
          output$mainContent <- renderUI({
            div("Your email has been validated. 
                You can now access analysis features.")
          })
        } else {
          # print("Failed to activate user")
          output$mainContent <- renderUI({
            div("Error occurred during account activation. 
                 Please try again or contact support.")
          })
        }
      } else {
        # print("Token validation failed")
        output$mainContent <- renderUI({
          div("Invalid or expired token.")
        })
      }
    }
  })
  
  # Helper function - Validate token for activating account
  validate_token <- function(token, con) {
    query <- glue_sql("
    SELECT COUNT(*) AS count 
    FROM users 
    WHERE activation_token = {token} AND is_active = 0",
                      token = token,
                      .con = con)
    
    # print(paste("Validation query:", query))
    
    result <- dbGetQuery(con, query)
    print(paste("Token validation result:", result$count))
    return(result$count > 0)
  }
  
  # Helper function - activate account 
  activate_user <- function(token, con) {
    query <- glue_sql("
    UPDATE users 
    SET is_active = 1, activation_token = NULL 
    WHERE activation_token = {token}",
                      token = token,
                      .con = con)
    
    # print(paste("Activation query:", query))
    
    result <- dbExecute(con, query)
    print(paste("Rows affected by activation:", result))
    return(result > 0)
  }
  
  #----------------------------------------------------------------------------#
  # Handle the Reset Password button click
  observeEvent(input$reset_password_btn, {
    email <- input$reset_email
    new_password <- input$reset_password
    
    if (email != "" && new_password != "") {
      # Call the function to update the password
      success <- update_password(email, new_password, con)
      
      if (success) {
        showModal(modalDialog(
          title = "Success",
          "Your password has been successfully reset.",
          easyClose = TRUE,
          footer = NULL
        ))
        # Clear the input fields after success
        updateTextInput(session, "reset_email", value = "")
        updateTextInput(session, "reset_password", value = "")
      } else {
        showModal(modalDialog(
          title = "Error",
          "There was an error resetting your password. Please try again.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    } else {
      showModal(modalDialog(
        title = "Input Error",
        "Please provide both email and new password.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Helper function to hash the new password using SHA-256
  hash_password <- function(password) {
    digest::digest(password, algo = "sha256")
  }
  
  # Function to update the password in the database
  update_password <- function(email, new_password, con) {
    hashed_password <- hash_password(new_password)
    
    # Create the SQL query to update the user's password
    query <- glue_sql("
      UPDATE users 
      SET password_hash = {hashed_password} 
      WHERE email = {email}",
                      hashed_password = hashed_password,
                      email = email,
                      .con = con
    )
    
    # Execute the query
    success <- tryCatch({
      dbExecute(con, query)
      TRUE
    }, error = function(e) {
      print(paste("Error updating password:", e$message))
      FALSE
    })
    
    return(success)
  }
  
  # return(signed_up)
  
}
