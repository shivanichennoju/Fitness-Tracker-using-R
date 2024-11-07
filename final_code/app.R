# library(shiny)
library(DBI)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
library(RPostgres)
library(htmltools)
library(shinyjs)
library(DT)
#connection details
dbname <- "neondb"
user <- "abhigna0512"
password <- "oq4RAU8rzsVe"
host <- "ep-shy-frost-194510.us-east-2.aws.neon.tech"
port <- 5432
options<- 'project=ep-shy-frost-194510'

# Define connection string to connect to SQL database
con <-
  dbConnect(
    RPostgres::Postgres(),
    dbname = dbname,
    options = options,
    user = user,
    password = password,
    host = host,
    port = port,
    sslmode = "require"
  )
rv <- reactiveValues(current_user_id = "")


# Define UI for login page
login_ui <- function() {
  fluidPage(headerPanel("Login"),
            mainPanel(fluidRow(column(
              width = 6,
              offset = 6,
              wellPanel(
                h3("Login to Wellness Tracker"),
                textInput("user_id", "User Id"),
                passwordInput("password", "Password"),
                checkboxInput("remember", "Remember me", FALSE),
                actionButton("submit_login", "Login"),
                actionButton("switch_to_register", "Create a new account")
              )
            ))))
}

# Define UI for register page
register_ui <- function() {
  rv$current_user_id <- user_id_global()
  fluidPage(headerPanel("Register"),
            mainPanel(fluidRow(column(
              width = 6,
              offset =6 ,
              wellPanel(
                h3("Create a new account"),
                textInput("user_id", "User ID"),
                passwordInput("new_password", "Password"),
                passwordInput("confirm_password", "Confirm password"),
                actionButton("submit_register", "Create account"),
                actionButton("switch_to_login", "Already have an account? Login")
              )
            ))))
}

admin_ui <- function () {
  fluidPage(# Title of the app
    titlePanel(paste0("All User Statistics- Admin View")),
    
    # Sidebar with a dropdown input for user id
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "user_id",
          "Select User Id:",
          choices = dbGetQuery(con, "SELECT DISTINCT \"Id\" FROM daily_calories")$Id
        )
      ),
      
      # Main panel with three plots
      mainPanel(
        # First plot
        plotOutput("plot1", height = 250),
        
        # Second plot
        plotOutput("plot2", height = 250),
        
        # Third plot
        plotOutput(outputId = "pie_chart")
      )
    ))
}

user_ui <- function () {
  fluidPage(# Title of the app
    titlePanel(paste0(
      "User Information for ", rv$current_user_id
    )),
    htmlOutput("my_message"),
    # Main panel with three plots
    mainPanel(
      # First plot
      plotOutput("plot1", height = 250),
      
      # Second plot
      plotOutput("plot2", height = 250),
      
      # Third plot
      plotOutput(outputId = "pie_chart")
    ))
}


crud_ui <- function () {
  fluidPage(
    useShinyjs(),
    tags$h3("Calories burnt during activity"),
    DTOutput("table_calories"),
    br(),
    actionButton("add_row_calories", "Add Row"),
    actionButton("update_row_calories", "Update Row"),
    actionButton("delete_row_calories", "Delete Row"),
    br(),
    br(),
    br(),
    tags$h3("Minutes spent on activity"),
    DTOutput("table_minute"),
    br(),
    actionButton("add_row_minute", "Add Row"),
    actionButton("update_row_minute", "Update Row"),
    actionButton("delete_row_minute", "Delete Row"),
    br(),
    br(),
    br(),
    tags$h3("Steps taken during activity"),
    DTOutput("table_steps"),
    br(),
    actionButton("add_row_steps", "Add Row"),
    actionButton("update_row_steps", "Update Row"),
    actionButton("delete_row_steps", "Delete Row"),
    br(),
    br(),
    br(),
    
  )
  
}

homes_ui <- function() {
  fluidPage(
    # Page title
    titlePanel(""),
    
    # Navigation bar
    navbarPage(
      "",
      theme = "light",
      id = 'tabs',
      # Tab for Home page
      tabPanel(
        "Home",
        fluidRow(column(
          width = 12,
          tags$h1("Welcome to Wellness Tracker", class = "display-4 text-center mb-5"),
          tags$p("Track your daily steps and stay motivated...", class = "text-center lead")
        )),
        fluidRow(column(
          width = 12,
          tags$h2("Stay Motivated! Keep Hydrated! Keep Running!", class = "text-center mb-5"),
          tags$div(
            tags$img(src = 'sample.jpg', alt = "Fitness Image", width = "1420")
          )
        )),
        fluidRow(column(
          width = 12,
          class = "bg-light py-5",
          tags$div(
            class = "container",
            tags$h2("About Us", class = "text-center mb-5"),
            tags$div(
              class = "row",
              tags$div(
                class = "col-lg-6 mx-auto text-center mb-5",
                tags$p(
                  HTML(
                    "We are a team of fitness enthusiasts who want to help you achieve your goals. Our Wellness Tracker was designed to make it easy for you to track your daily steps and stay motivated. <i class='fas fa-heart'></i>"
                  ),
                  class = "lead"
                ),
                tags$p(
                  HTML(
                    "With our website, you can track your progress and achieve your fitness goals. We believe that staying active is important for your health and well-being, and we want to make it fun and easy for you to stay on track. <i class='fas fa-check'></i>"
                  ),
                  class = "lead"
                )
              ),
              tags$div(
                class = "col-lg-6 mx-auto text-center",
                tags$img(
                  src = "sanple.jpg",
                  class = "img-fluid rounded shadow",
                  alt = "About Us Image",
                  height = "280"
                )
              )
            )
          )
        ))
      ),
      
      tabPanel("Your Insights", viz_ui(), value = "graphs"),
      tabPanel("Update Details", crud_ui()),
      tabPanel("Log Out", value = "Logout")
      
    ),
    
    
    tags$style(
      HTML(
        "
  /* Style for page title */
  .navbar-brand, .navbar-text {
    color: #2b2d42 !important;
  }

  /* Style for 'Home' tab */
  .nav-link.active, .nav-link:hover {
    color: #fff !important;
    background-color: #2b2d42 !important;
  }

  /* Style for headings */
  h1, h2, h3, h4, h5, h6 {
    font-weight: 600;
  }

  /* Style for overlay on fitness image */
  .overlay {
    position: relative;
  }

  .overlay::before {
    content: '';
    position: absolute;
    top: 0;
    bottom: 0;
    left: 0;
    right: 0;
    background-color: rgba(0, 0, 0, 0.5);
  }
/* Style for footer */
.footer {
  position: absolute;
  bottom: 0;
  width: 100%;
  height: 50px;
  line-height: 50px;
}

/* Style for login and register forms */
.form-signin {
  max-width: 330px;
  padding: 15px;
  margin: 0 auto;
}

.form-signin .form-control {
  position: relative;
  box-sizing: border-box;
  height: auto;
  padding: 10px;
  font-size: 16px;
}

.form-signin .form-control:focus {
  z-index: 2;
}

/* Style for error messages */
.error {
  color: #dc3545;
  font-weight: bold;
  margin-bottom: 10px;
}

/* Style for success messages */
.success {
  color: #28a745;
  font-weight: bold;
  margin-bottom: 10px;
}

/* Style for buttons */
.btn-primary {
  background-color: #2b2d42;
  border: none;
}

.btn-primary:hover {
  background-color: #1d1f2f;
}

.btn-secondary {
  background-color: #6c757d;
  border: none;
}

.btn-secondary:hover {
  background-color: #545b62;
}

")
    )
  )
}

viz_ui <- function() {
  if (rv$current_user_id == 12345) {
    renderUI(admin_ui())
  } else {
    renderUI(user_ui())
  }
}

# login_new_ui <- function() {
#   fluidPage(headerPanel("Login"),
#             mainPanel(fluidRow(column(
#               width = 12,
#               offset = 3,
#               wellPanel(
#                 h3("Login to Wellness Tracker"),
#                 textInput("user_id_1", "User Id"),
#                 passwordInput("password", "Password"),
#                 checkboxInput("remember", "Remember me", FALSE),
#                 actionButton("submit_login_1", "Login"),
#                 actionButton("switch_to_register", "Create a new account")
#               )
#             ))))
# }

query_calories <- function(user_id) {
  dbGetQuery(con,
             paste0("SELECT * FROM daily_calories WHERE \"Id\" = '", user_id, "'"))
}

query_minutes <- function(user_id) {
  dbGetQuery(con,
             paste0("SELECT * FROM minutes WHERE \"Id\" = '", user_id, "'"))
}

query_steps <- function(user_id) {
  dbGetQuery(con, paste0("SELECT * FROM steps "))
}

query_user_steps <- function(user_id) {
  dbGetQuery(con, paste0("SELECT * FROM steps WHERE \"Id\" = '", user_id, "'"))
}

user_id_global <- reactiveVal(0)


# Define server
server <- function(input, output, session) {
  session$userData$current_user_id_new = 0
  # Show login page by default
  output$ui <- renderUI(login_ui())
  
  # Handle login form submission
  observeEvent(input$submit_login, {
    user_id_global(input$user_id)
    # Perform login verification here
    query_login_check <-
      paste0("select password,admin from creds where ",
             input$user_id,
             " = creds.\"username\";")
    login_creds <- dbGetQuery(con, query_login_check)
    if (input$password == login_creds$password) {
      # Redirect to dashboard if credentials are correct
      # replace "/dashboard" with your desired dashboard page
      rv$current_user_id <- input$user_id
      plots_function(input$user_id)
      session$userData$current_user_id_new <- input$user_id
      fetch_crud_data()
      output$ui <- renderUI(homes_ui())
    } else {
      # Show error message if credentials are incorrect
      showModal(
        modalDialog(
          title = "Invalid login",
          "The email and/or password you entered are incorrect. Please try again.",
          easyClose = TRUE
        )
      )
    }
  })
  
  
  #print(login_creds$admin)
  
  # Handle register form submission
  observeEvent(input$submit_register, {
    # Perform register verification here
    query_user_check <-
      paste0(
        "SELECT EXISTS(SELECT * FROM creds WHERE",
        " creds.\"username\"=",
        input$user_id,
        ");"
      )
    user_check <- dbGetQuery(con, query_user_check)
    if (input$new_password != input$confirm_password) {
      # Show error message if passwords do not match
      showModal(
        modalDialog(
          title = "Registration error",
          "The passwords you entered do not match. Please try again.",
          easyClose = TRUE
        )
      )
    }
    else if (user_check == "TRUE") {
      showModal(
        modalDialog(
          title = "Registration error",
          "User already exists",
          easyClose = TRUE
        )
      )
    }
    else {
      # Register user and redirect to login page
      # replace "/login" with the URL of your login page
      query_register <-
        paste0("insert into creds values (",
               input$user_id,
               ",",
               input$new_password,
               ",",
               0,
               ");")
      dbGetQuery(con, query_register)
      updateTextInput(session, "current_user_id_new", value = input$user_id)
      output$ui <- renderUI(login_ui())
    }
  })
  
  # Switch to register page
  observeEvent(input$switch_to_register, {
    output$ui <- renderUI(register_ui())
  })
  
  # Switch to login page
  observeEvent(input$switch_to_login, {
    output$ui <- renderUI(login_ui())
  })
  
  
  ##plots
  
  plots_function <- function (user_id) {
    if (input$user_id == 12345) {
      query_calories <- reactive({
        paste0("SELECT * FROM daily_calories where daily_calories.\"Id\"=",
               input$user_id)
      })
      query_steps <- reactive({
        paste0("SELECT * FROM steps ")
      })
      query_minutes <- reactive({
        paste0("SELECT * FROM minutes where minutes.\"Id\"=",
               input$user_id)
      })
      
      #Getting the data
      daily_Steps <- reactive({
        dbGetQuery(con, query_steps())
      })
      daily_calories <- reactive({
        dbGetQuery(con, query_calories())
      })
      daily_minutes <- reactive({
        dbGetQuery(con, query_minutes())
      })
      
      steps <- reactive({
        # print(class(daily_Steps()$Id))
        daily_Steps()[as.character(daily_Steps()$Id) == input$user_id, ]
      })
      # First plot
      observe({
        # print(steps())
        output$plot1 <- renderPlot({
          ggplot(steps(), aes(x = ActivityDate, y = as.integer(TotalSteps))) +
            geom_bar(stat = "identity") +
            xlab("Activity Date") +
            ylab("Steps") +
            ggtitle(paste0("Steps per Day for ID:", input$user_id))
        })
      })
      
      # Second plot
      
      observe({
        output$plot2 <- renderPlot({
          ggplot() +
            geom_line(data = steps(),
                      aes(
                        x = ActivityDate,
                        y = as.integer(TotalSteps),
                        color = "Selected User"
                      )) +
            geom_line(
              data = daily_Steps() %>% group_by(ActivityDate) %>% summarize(mean_steps = mean(as.integer(
                TotalSteps
              ))),
              aes(
                x = ActivityDate,
                y = mean_steps,
                color = "All Users"
              )
            ) +
            labs(x = "Date",
                 y = "Steps",
                 color = " ") +
            scale_color_manual(values = c("Selected User" = "blue", "All Users" = "red"))
        })
      })
      
      # Third plot
      observe({
        minutes_user <-
          daily_minutes()[daily_minutes()$Id == input$user_id, ]
        
        # Calculate total time spent on each activity
        total <-
          colSums(minutes_user[, c(
            "VeryActiveMinutes",
            "FairlyActiveMinutes",
            "LightlyActiveMinutes",
            "SedentaryMinutes"
          )])
        
        # Calculate percentage time spent on each activity
        percent <- total / sum(total) * 100
        
        # Create pie chart
        activity_labels <-
          c("Very Active",
            "Fairly Active",
            "Lightly Active",
            "Sedentary")
        activity_data <-
          data.frame(Activity = activity_labels, Percent = percent)
        chart <-
          ggplot(activity_data, aes(
            x = "",
            y = Percent,
            fill = Activity
          )) +
          geom_bar(stat = "identity",
                   width = 1,
                   color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(fill = "Activity")
        
        # Display pie chart
        output$pie_chart <- renderPlot({
          chart
        })
      })
    } else {
      user_data <- reactive({
        list(
          calories = query_calories(rv$current_user_id),
          minutes = query_minutes(rv$current_user_id),
          q_steps = query_steps(rv$current_user_id)
        )
      })
      steps <- reactive({
        # print(class(daily_Steps()$Id))
        query_steps(rv$current_user_id)[as.character(query_steps()$Id) == rv$current_user_id, ]
      })
      if(nrow(steps())<=0 & nrow(user_data()$minutes)<=0){
        output$my_message <- renderText("Please update your daily activity in Update details!!, 
                                        to get insights")
      }
      else{
      output$plot1 <- renderPlot({
        ggplot(steps(), aes(x = ActivityDate, y = as.integer(TotalSteps))) +
          geom_bar(stat = "identity") +
          xlab("Activity Date") +
          ylab("Steps") +
          ggtitle(paste0("Steps per Day for ID:", rv$current_user_id))
        
      })
      
      output$plot2 <- renderPlot({
        ggplot() +
          geom_line(
            data = query_user_steps(rv$current_user_id),
            aes(
              x = ActivityDate,
              y = as.integer(TotalSteps),
              color = "Selected User"
            )
          ) +
          geom_line(
            data = query_steps(rv$current_user_id) %>% group_by(ActivityDate) %>% summarize(mean_steps = mean(as.integer(TotalSteps))),
            aes(
              x = ActivityDate,
              y = mean_steps,
              color = "All Users"
            )
          ) +
          labs(x = "Date",
               y = "Steps",
               color = " ") +
          scale_color_manual(values = c("Selected User" = "blue", "All Users" = "red")) +
          ggtitle(paste0(
            "Comparing user ",
            rv$current_user_id,
            " with other users"
          ))
      })
      
      # Pie chart
      output$pie_chart <- renderPlot({
        # Query minutes for selected user id
        minutes_user <-
          user_data()$minutes[user_data()$minutes$Id == rv$current_user_id, ]
        
        # Calculate total time spent on each activity
        total <-
          colSums(minutes_user[, c(
            "VeryActiveMinutes",
            "FairlyActiveMinutes",
            "LightlyActiveMinutes",
            "SedentaryMinutes"
          )])
        
        # Calculate percentage time spent on each activity
        percent <- total / sum(total) * 100
        
        # Create pie chart
        activity_labels <-
          c("Very Active",
            "Fairly Active",
            "Lightly Active",
            "Sedentary")
        activity_data <-
          data.frame(Activity = activity_labels, Percent = percent)
        chart <-
          ggplot(activity_data, aes(
            x = "",
            y = Percent,
            fill = Activity
          )) +
          geom_bar(stat = "identity",
                   width = 1,
                   color = "white") +
          coord_polar("y", start = 0) +
          theme_void() +
          labs(fill = "Activity") +
          ggtitle(
            paste0(
              "Time spent on burning calories and on steps by user:",
              rv$current_user_id
            )
          )
        
        output$pie_chart <- renderPlot({
          chart
        })
      })
      }
    }
  }
  
  
  
  # query to fetch minutes, calories and steps data from postgres
  
  # fetching minutes data
  fetch_crud_data <- function() {
    query_get_minutes <- paste0(
      "select * from minutes where ",
      session$userData$current_user_id_new,
      " = minutes.\"Id\";"
    )
    # query_get_minutes<- paste0("select * from minutes;")
    minutes_data_full <- dbGetQuery(con, query_get_minutes)
    # Taking latest 10 records
    minutes_data = tail(minutes_data_full, n = 10)
    #Taking date from drop down while updating and deleting the rows
    minutes_date <- minutes_data_full$ActivityDate
    # Sending the data in table format
    data_minutes <- reactiveValues(
      table_data = data.frame(
        Id = minutes_data$Id,
        ActivityDate = minutes_data$ActivityDate,
        VeryActiveMinutes = minutes_data$VeryActiveMinutes,
        FairlyActiveMinutes = minutes_data$FairlyActiveMinutes,
        LightlyActiveMinutes = minutes_data$LightlyActiveMinutes,
        SedentaryMinutes = minutes_data$SedentaryMinutes
      )
    )
    
    
    
    #fetching calories data
    query_get_calories <-
      paste0(
        "select * from daily_calories where ",
        session$userData$current_user_id_new,
        " = daily_calories.\"Id\";"
      )
    calories_data_full <- dbGetQuery(con, query_get_calories)
    # Taking latest 10 records
    calories_data = tail(calories_data_full, n = 10)
    #Taking date from drop down while updating and deleting the rows
    calories_date <- calories_data_full$ActivityDate
    
    # Sending the data in table format
    data_calories <- reactiveValues(
      table_data = data.frame(
        Id = calories_data$Id,
        ActivityDate = calories_data$ActivityDate,
        Calories = calories_data$Calories
      )
    )
    
    
    #fetching steps data
    query_get_steps <- paste0(
      "select * from steps where ",
      session$userData$current_user_id_new,
      " = steps.\"Id\";"
    )
    steps_data_full <- dbGetQuery(con, query_get_steps)
    # Taking latest 10 records
    steps_data = tail(steps_data_full, n = 10)
    #Taking date from drop down while updating and deleting the rows
    steps_date <- steps_data_full$ActivityDate
    
    # Sending the data in table format
    data_steps <- reactiveValues(
      table_data = data.frame(
        Id = steps_data$Id,
        ActivityDate = steps_data$ActivityDate,
        TotalSteps = steps_data$TotalSteps
      )
    )
    
    # Render the calories table
    output$table_calories <- renderDT({
      datatable(
        data_calories$table_data,
        options = list(dom = "t",
                       pageLength = 20),
        rownames = FALSE
      )
    })
    
    # Render the minutes table
    output$table_minute <- renderDT({
      datatable(
        data_minutes$table_data,
        options = list(dom = "t",
                       pageLength = 20),
        rownames = FALSE
      )
    })
    
    # Render the steps table
    output$table_steps <- renderDT({
      datatable(
        data_steps$table_data,
        options = list(dom = "t",
                       pageLength = 20),
        rownames = FALSE
      )
    })
    ############# Operations #################
    
    ######### Calories Data #############
    
    ##### Add Row #####
    
    # Add a new row to the calories table
    observeEvent(input$add_row_calories, {
      showModal(
        # Define modal for adding a new row
        modalDialog(
          title = "Add new activity to minutes table",
          dateInput("activity_date_calories", "Activity Date"),
          numericInput("calories", "Calories", 0),
          footer = tagList(
            actionButton("add_new_row_calories", "Submit"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    # Handle the "Add" button click event in the modal
    observeEvent(input$add_new_row_calories, {
      new_row <- data.frame(
        Id = session$userData$current_user_id_new,
        ActivityDate = as.Date(input$activity_date_calories),
        Calories = input$calories
      )
      new_row$ActivityDate <-
        as.Date(new_row$ActivityDate, format = "%Y-%m-%d")
      query_insert_calories <-
        paste0(
          "insert into daily_calories values (",
          new_row$Id,
          ",",
          "'",
          as.character(new_row$ActivityDate),
          "'",
          ",",
          new_row$Calories,
          ");"
        )
      dbSendQuery(con, query_insert_calories)
      
      data_calories$table_data <-
        rbind(data_calories$table_data, new_row)
      removeModal()
    })
    ##### Update Row #####
    
    # Update existing row to calories table
    observeEvent(input$activity_date_calories, {
      current_data <-
        calories_data_full[calories_data_full$ActivityDate == input$activity_date_calories,]
      updateTextInput(session, "s_calories", value = current_data$Calories)
    })
    
    observeEvent(input$update_row_calories, {
      #input$activity_date <- minutes_date[0]
      showModal(
        # Define modal for adding a new row
        modalDialog(
          title = "Update activity to calories table",
          selectInput("activity_date_calories", "Activty Date:", choices = calories_date),
          numericInput("s_calories", "Calories", 0),
          footer = tagList(
            actionButton("update_new_row_calories", "Update"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$update_new_row_calories, {
      new_row <- data.frame(
        Id = session$userData$current_user_id_new,
        ActivityDate = input$activity_date_calories,
        Calories = input$s_calories
      )
      query_update_calories <- paste0(
        "UPDATE daily_calories SET ",
        "\"Calories\" = ",
        new_row$Calories,
        " WHERE daily_calories.\"ActivityDate\" = '",
        new_row$ActivityDate,
        "' and daily_calories.\"Id\" = ",
        new_row$Id,
        ";"
      )
      
      
      dbSendQuery(con, query_update_calories)
      data_calories$table_data <-
        subset(
          data_calories$table_data,!(Id == new_row$Id &
                                       ActivityDate == new_row$ActivityDate)
        )
      data_calories$table_data <-
        rbind(data_calories$table_data, new_row)
      removeModal()
    })
    
    ##### Delete Row #####
    # delete an existing row from calories table
    observeEvent(input$delete_row_calories, {
      #input$activity_date <- minutes_date[0]
      showModal(
        # Define modal for adding a new row
        modalDialog(
          title = "Delete activity from calories table",
          selectInput(
            "activity_date_calories_delete",
            "Activty Date:",
            choices = calories_date
          ),
          footer = tagList(
            actionButton("delete_new_row_calories", "Delete"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$activity_date_calories_delete, {
      current_data <-
        calories_data_full[calories_data_full$ActivityDate == input$activity_date_calories_delete,]
    })
    
    observeEvent(input$delete_new_row_calories, {
      query_delete_calories <- paste0(
        "DELETE FROM daily_calories WHERE daily_calories.\"ActivityDate\" = '",
        input$activity_date_calories_delete,
        "' and daily_calories.\"Id\" = ",
        session$userData$current_user_id_new,
        ";"
      )
      dbSendQuery(con, query_delete_calories)
      data_calories$table_data <-
        subset(
          data_calories$table_data,!(
            Id == session$userData$current_user_id_new &
              ActivityDate == input$activity_date_calories_delete
          )
        )
      
      removeModal()
    })
    
    
    
    ######### Minutes Data #############
    
    ##### Add Row #####
    # Add a new row to the minutes table
    observeEvent(input$add_row_minute, {
      showModal(
        # Define modal for adding a new row
        modalDialog(
          title = "Add new activity to minutes table",
          dateInput("activity_date", "Activity Date"),
          numericInput("very_active_minutes", "Very Active Minutes", 0),
          numericInput("fairly_active_minutes", "Fairly Active Minutes", 0),
          numericInput("lightly_active_minutes", "Lightly Active Minutes", 0),
          numericInput("sedentary_minutes", "Sedentary Minutes", 0),
          footer = tagList(
            actionButton("add_new_row_minute", "Submit"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    # Handle the "Add" button click event in the modal
    observeEvent(input$add_new_row_minute, {
      new_row <- data.frame(
        Id = session$userData$current_user_id_new,
        ActivityDate = as.Date(input$activity_date),
        VeryActiveMinutes = input$very_active_minutes,
        FairlyActiveMinutes = input$fairly_active_minutes,
        LightlyActiveMinutes = input$lightly_active_minutes,
        SedentaryMinutes = input$sedentary_minutes
      )
      new_row$ActivityDate <-
        as.Date(new_row$ActivityDate, format = "%Y-%m-%d")
      query_insert_minute <- paste0(
        "insert into minutes values (",
        new_row$Id,
        ",",
        "'",
        as.character(new_row$ActivityDate),
        "'",
        ",",
        new_row$VeryActiveMinutes,
        ",",
        new_row$FairlyActiveMinutes,
        ",",
        new_row$LightlyActiveMinutes,
        ",",
        new_row$SedentaryMinutes,
        ");"
      )
      dbSendQuery(con, query_insert_minute)
      data_minutes$table_data <-
        rbind(data_minutes$table_data, new_row)
      removeModal()
    })
    
    ##### Update Row #####
    # Update existing row to minutes table
    observeEvent(input$activity_date, {
      current_data <-
        minutes_data_full[minutes_data_full$ActivityDate == input$activity_date,]
      updateTextInput(session,
                      "s_very_active_minutes",
                      value = current_data$VeryActiveMinutes)
      updateTextInput(session,
                      "s_fairly_active_minutes",
                      value = current_data$FairlyActiveMinutes)
      updateTextInput(session,
                      "s_lightly_active_minutes",
                      value = current_data$LightlyActiveMinutes)
      updateTextInput(session,
                      "s_sedentary_minutes",
                      value = current_data$SedentaryMinutes)
    })
    
    observeEvent(input$update_row_minute, {
      #input$activity_date <- minutes_date[0]
      showModal(
        # Define modal for adding a new row
        modalDialog(
          title = "Update activity to minutes table",
          selectInput("activity_date", "Activty Date:", choices = minutes_date),
          numericInput("s_very_active_minutes", "Very Active Minutes", 0),
          numericInput("s_fairly_active_minutes", "Fairly Active Minutes", 0),
          numericInput("s_lightly_active_minutes", "Lightly Active Minutes", 0),
          numericInput("s_sedentary_minutes", "Sedentary Minutes", 0),
          footer = tagList(
            actionButton("update_new_row_minute", "Update"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$update_new_row_minute, {
      #print(input)
      new_row <- data.frame(
        Id = session$userData$current_user_id_new,
        ActivityDate = input$activity_date,
        VeryActiveMinutes = input$s_very_active_minutes,
        FairlyActiveMinutes = input$s_fairly_active_minutes,
        LightlyActiveMinutes = input$s_lightly_active_minutes,
        SedentaryMinutes = input$s_sedentary_minutes
      )
      # print(new_row)
      query_update_minute <- paste0(
        "UPDATE minutes SET ",
        "\"VeryActiveMinutes\" = ",
        new_row$VeryActiveMinutes,
        ",",
        "\"FairlyActiveMinutes\" = ",
        new_row$FairlyActiveMinutes,
        ",",
        "\"LightlyActiveMinutes\" = ",
        new_row$LightlyActiveMinutes,
        ",",
        "\"SedentaryMinutes\" = ",
        new_row$SedentaryMinutes,
        " WHERE minutes.\"ActivityDate\" = '",
        new_row$ActivityDate,
        "' and minutes.\"Id\" = ",
        new_row$Id,
        ";"
      )
      
      
      dbSendQuery(con, query_update_minute)
      data_minutes$table_data <-
        subset(
          data_minutes$table_data,!(Id == new_row$Id &
                                      ActivityDate == new_row$ActivityDate)
        )
      data_minutes$table_data <-
        rbind(data_minutes$table_data, new_row)
      removeModal()
    })
    
    ##### Delete Row #####
    # delete an existing row from minutes table
    observeEvent(input$delete_row_minute, {
      #input$activity_date <- minutes_date[0]
      showModal(
        # Define modal for adding a new row
        modalDialog(
          title = "Delete activity from minutes table",
          selectInput("activity_date_delete", "Activty Date:", choices = minutes_date),
          footer = tagList(
            actionButton("delete_new_row_minute", "Delete"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$activity_date_delete, {
      current_data <-
        minutes_data_full[minutes_data_full$ActivityDate == input$activity_date_delete,]
    })
    
    observeEvent(input$delete_new_row_minute, {
      query_delete_minute <- paste0(
        "DELETE FROM minutes WHERE minutes.\"ActivityDate\" = '",
        input$activity_date_delete,
        "' and minutes.\"Id\" = ",
        session$userData$current_user_id_new,
        ";"
      )
      dbSendQuery(con, query_delete_minute)
      data_minutes$table_data <-
        subset(
          data_minutes$table_data,!(
            Id == session$userData$current_user_id_new &
              ActivityDate == input$activity_date_delete
          )
        )
      removeModal()
    })
    
    ######### Steps Data #########
    
    ##### Add Row #####
    
    # Add a new row to the calories table
    observeEvent(input$add_row_steps, {
      showModal(
        # Define modal for adding a new row
        modalDialog(
          title = "Add new activity to steps table",
          dateInput("activity_date_steps", "Activity Date"),
          numericInput("steps", "Steps", 0),
          footer = tagList(
            actionButton("add_new_row_steps", "Submit"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    # Handle the "Add" button click event in the modal
    observeEvent(input$add_new_row_steps, {
      new_row <- data.frame(
        Id = session$userData$current_user_id_new,
        ActivityDate = as.Date(input$activity_date_steps),
        TotalSteps = input$steps
      )
      new_row$ActivityDate <-
        as.Date(new_row$ActivityDate, format = "%Y-%m-%d")
      query_insert_calories <- paste0(
        "insert into steps values (",
        new_row$Id,
        ",",
        "'",
        as.character(new_row$ActivityDate),
        "'",
        ",",
        new_row$TotalSteps,
        ");"
      )
      dbSendQuery(con, query_insert_calories)
      data_steps$table_data <- rbind(data_steps$table_data, new_row)
      removeModal()
    })
    
    ##### Update Row #####
    
    # Update existing row to steps table
    observeEvent(input$activity_date_steps, {
      current_data <-
        steps_data_full[steps_data_full$ActivityDate == input$activity_date_steps,]
      updateTextInput(session, "s_steps", value = current_data$TotalSteps)
    })
    
    observeEvent(input$update_row_steps, {
      showModal(
        modalDialog(
          title = "Update activity to steps table",
          selectInput("activity_date_steps", "Activty Date:", choices = steps_date),
          numericInput("s_steps", "Calories", 0),
          footer = tagList(
            actionButton("update_new_row_steps", "Update"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$update_new_row_steps, {
      #print(input)
      new_row <- data.frame(
        Id = session$userData$current_user_id_new,
        ActivityDate = input$activity_date_steps,
        TotalSteps = input$s_steps
      )
      query_update_steps <- paste0(
        "UPDATE steps SET ",
        "\"TotalSteps\" = ",
        new_row$TotalSteps,
        " WHERE steps.\"ActivityDate\" = '",
        new_row$ActivityDate,
        "' and steps.\"Id\" = ",
        new_row$Id,
        ";"
      )
      
      
      dbSendQuery(con, query_update_steps)
      data_steps$table_data <-
        subset(
          data_steps$table_data,!(Id == new_row$Id &
                                    ActivityDate == new_row$ActivityDate)
        )
      data_steps$table_data <- rbind(data_steps$table_data, new_row)
      removeModal()
    })
    
    ##### Delete Row #####
    # delete an existing row from steps table
    observeEvent(input$delete_row_steps, {
      #input$activity_date <- minutes_date[0]
      showModal(
        # Define modal for adding a new row
        modalDialog(
          title = "Delete activity from steps table",
          selectInput("activity_date_steps_delete", "Activty Date:", choices = steps_date),
          footer = tagList(
            actionButton("delete_new_row_steps", "Delete"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$activity_date_steps_delete, {
      current_data <-
        steps_data_full[steps_data_full$ActivityDate == input$activity_date_steps_delete,]
    })
    
    observeEvent(input$delete_new_row_steps, {
      query_delete_steps <- paste0(
        "DELETE FROM steps WHERE steps.\"ActivityDate\" = '",
        input$activity_date_steps_delete,
        "' and steps.\"Id\" = ",
        session$userData$current_user_id_new,
        ";"
      )
      dbSendQuery(con, query_delete_steps)
      data_steps$table_data <-
        subset(
          data_steps$table_data,!(
            Id == session$userData$current_user_id_new &
              ActivityDate == input$activity_date_steps_delete
          )
        )
      removeModal()
    })
  }

  observeEvent(input$tabs, {
    # print(input$tabs)
    if (input$tabs == "Logout") {
      session$reload()
    }
    if (input$tabs == "graphs" & rv$current_user_id != 12345) {
      plots_function(rv$current_user_id)
    }
  })
}

# Run the app
shinyApp(ui = fluidPage(uiOutput("ui")), server = server)
