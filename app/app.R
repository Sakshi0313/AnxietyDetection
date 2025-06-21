library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(randomForest)

# Load trained Random Forest model (adjust path if needed)
rf_model <- readRDS("models/random_forest_model.rds")

# Load dataset to get feature columns (adjust path if needed)
dataset <- read.csv("cleaned_gaming_dataset_final.csv")

# Define SPIN questions
spin_questions <- c(
  "I fear talking to strangers.",
  "I avoid activities where I am the center of attention.",
  "I feel anxious when making eye contact with people.",
  "I fear being embarrassed or looking foolish.",
  "I feel uncomfortable eating or drinking in front of others.",
  "I avoid speaking in public because of fear.",
  "I experience anxiety in social gatherings.",
  "I have difficulty interacting with people I don't know well.",
  "I worry about being judged by others.",
  "I feel uncomfortable when being introduced to new people.",
  "I get extremely anxious when I have to perform in front of others.",
  "I experience panic when meeting authority figures.",
  "I tend to avoid social situations due to fear.",
  "I have trouble starting conversations.",
  "I fear being criticized in public.",
  "I find it difficult to mix with people.",
  "I avoid being in crowded places."
)

# Recommendation function
recommend_content <- function(anxiety_type) {
  recommendation <- list()
  
  if (anxiety_type == "GAD") {
    recommendation$game <- "Monument Valley (calm puzzle game)"
    recommendation$quote <- "You are stronger than you think, and you are not alone."
  } else if (anxiety_type == "SAD") {
    recommendation$game <- "Stardew Valley (social-friendly farming game)"
    recommendation$quote <- "Take one step at a time, and you will get there."
  } else if (anxiety_type == "Mixed_Anxiety") {
    recommendation$game <- "Journey (relaxing adventure game)"
    recommendation$quote <- "Breathe. Trust yourself. You are capable of overcoming this."
  } else if (anxiety_type == "No_Anxiety") {
    recommendation$game <- "Enjoy any game you like!"
    recommendation$quote <- "No anxiety detected. Keep up the great work and enjoy gaming responsibly!"
  } else {
    recommendation$game <- NA
    recommendation$quote <- "Keep enjoying your journey!"
  }
  
  return(recommendation)
}

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  # Background image CSS
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('gaming1.jpg');
        background-size: cover;
        background-attachment: fixed;
        background-position: center;
        background-repeat: no-repeat;
      }
      .well, .panel, .form-control, .selectize-input {
        background-color: rgba(255, 255, 255, 0.85) !important;
      }
      .shiny-input-container {
        background-color: rgba(255, 255, 255, 0.85) !important;
        padding: 10px;
        border-radius: 10px;
      }
      .btn-primary {
        background-color: #337ab7;
        border-color: #2e6da4;
      }
    "))
  ),
  
  titlePanel("🎮 Online Gaming Anxiety Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Answer the questions below:"),
      hr(),
      
      h4("🧑 Demographic and Background Info"),
      selectInput("gender", "Gender:", choices = c("Male", "Female", "Other")),
      
      selectInput("whyplay", "Why do you play games?", 
                  choices = c("Stress Relief", "Entertainment", "Social Interaction", "Competition", "Other")),
      conditionalPanel(
        condition = "input.whyplay == 'Other'",
        textInput("whyplay_other", "Please specify your reason:")
      ),
      
      numericInput("hours", "Average gaming hours per day:", value = 2, min = 0),
      
      selectInput("degree", "Current degree/course:", 
                  choices = c("BTech", "BSc", "BA", "MTech", "MSc", "Other")),
      conditionalPanel(
        condition = "input.degree == 'Other'",
        textInput("degree_other", "Please specify your qualification:")
      ),
      
      selectInput("work", "Are you working currently?", choices = c("Yes", "No")),
      
      hr(),
      h4("🟡 Generalized Anxiety Disorder (GAD)"),
      lapply(1:7, function(i) {
        selectInput(
          paste0("Q", i),
          label = c(
            "How often have you felt nervous, anxious, or on edge?",
            "How often have you had trouble relaxing?",
            "How often have you felt so restless that it's hard to sit still?",
            "How often have you felt afraid as if something awful might happen?",
            "How often have you felt easily annoyed or irritable?",
            "How often have you felt worried about different things?",
            "How often have you had difficulty controlling your worrying?"
          )[i],
          choices = c("0: Not at all" = 0, "1: Several days" = 1, "2: More than half the days" = 2, "3: Nearly every day" = 3)
        )
      }),
      
      h4("🔵 Satisfaction With Life Scale (SWL)"),
      lapply(8:12, function(i) {
        selectInput(
          paste0("Q", i),
          label = c(
            "In most ways, my life is close to my ideal.",
            "The conditions of my life are excellent.",
            "I am satisfied with my life.",
            "So far, I have gotten the important things I want in life.",
            "If I could live my life over, I would change almost nothing."
          )[i - 7],
          choices = c(
            "1: Strongly Disagree" = 1,
            "2: Disagree" = 2,
            "3: Slightly Disagree" = 3,
            "4: Neutral" = 4,
            "5: Slightly Agree" = 5,
            "6: Agree" = 6,
            "7: Strongly Agree" = 7
          )
        )
      }),
      
      h4("🔴 Social Phobia Inventory (SPIN)"),
      tagList(
        lapply(13:29, function(i) {
          selectInput(
            paste0("Q", i),
            label = spin_questions[i - 12],
            choices = c(
              "0: Not at all" = 0,
              "1: A little bit" = 1,
              "2: Somewhat" = 2,
              "3: Very much" = 3,
              "4: Extremely" = 4
            )
          )
        })
      ),
      
      br(),
      actionButton("submit", "Predict", class = "btn-primary btn-lg")
    ),
    
    mainPanel(
      h3("Prediction Result:"),
      verbatimTextOutput("result"),
      hr(),
      h3("🎮 Game Recommendation & Quote:"),
      verbatimTextOutput("recommendation"),
      hr(),
      DTOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {
  observeEvent(input$submit, {
    # Validate 'whyplay'
    whyplay_input <- if (input$whyplay == "Other") {
      if (is.null(input$whyplay_other) || trimws(input$whyplay_other) == "") {
        showNotification("⚠️ Please provide a valid reason for 'Why you play games'.", type = "error")
        return()
      } else {
        input$whyplay_other
      }
    } else {
      input$whyplay
    }
    
    # Validate 'degree'
    degree_input <- if (input$degree == "Other") {
      if (is.null(input$degree_other) || trimws(input$degree_other) == "") {
        showNotification("⚠️ Please provide a valid degree/qualification.", type = "error")
        return()
      } else {
        input$degree_other
      }
    } else {
      input$degree
    }
    
    # Collect questionnaire answers
    question_values <- sapply(1:29, function(i) {
      val <- input[[paste0("Q", i)]]
      if (is.null(val) || val == "") NA else as.numeric(val)
    })
    
    # Check for any missing answers
    if (any(is.na(question_values))) {
      showNotification("⚠️ Please answer all the questionnaire items.", type = "error")
      return()
    }
    
    # Calculate total scores
    GAD_T <- sum(question_values[1:7])
    SWL_T <- sum(question_values[8:12])
    SPIN_T <- sum(question_values[13:29])
    
    # Prepare input data frame for prediction
    input_data <- as.data.frame(t(c(question_values, GAD_T = GAD_T, SWL_T = SWL_T, SPIN_T = SPIN_T)))
    colnames(input_data) <- c(paste0("Q", 1:29), "GAD_T", "SWL_T", "SPIN_T")
    
    # Predict anxiety type
    prediction <- tryCatch({
      predict(rf_model, newdata = input_data)
    }, error = function(e) {
      paste("Prediction error:", e$message)
    })
    
    # Show prediction result
    output$result <- renderText({
      paste("🎯 Predicted Anxiety Type:", prediction)
    })
    
    # Show recommendation
    recommendation <- recommend_content(as.character(prediction))
    output$recommendation <- renderText({
      paste0("Recommended Game: ", recommendation$game, "\n",
             "Quote: ", recommendation$quote)
    })
    
    # Show summary table
    display_data <- data.frame(
      GAD_Total = GAD_T,
      SWL_Total = SWL_T,
      SPIN_Total = SPIN_T,
      Predicted_Anxiety_Type = prediction,
      stringsAsFactors = FALSE
    )
    
    output$table <- renderDT({
      datatable(
        display_data,
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      )
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
