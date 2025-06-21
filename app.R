library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(randomForest)

# Load trained Random Forest model
rf_model <- readRDS("D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/models/random_forest_model.rds")

# Load dataset to get feature columns
dataset <- read.csv("D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/data/cleaned_gaming_dataset_final.csv")

# Define feature columns (EXCLUDE age)
feature_columns <- setdiff(names(dataset), c("age"))
feature_columns <- unique(c(feature_columns, "GAD_T", "SWL_T", "SPIN_T"))

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
    recommendation$game <- "Monument Valley (Calming puzzle game)"
    recommendation$quote <- "You are stronger than you think. Take it one breath at a time."
  } else if (anxiety_type == "SAD") {
    recommendation$game <- "Stardew Valley (Social-friendly farming game)"
    recommendation$quote <- "Even small steps matter. You’re doing great!"
  } else if (anxiety_type == "Mixed_Anxiety") {
    recommendation$game <- "Journey (Relaxing adventure game)"
    recommendation$quote <- "Breathe deeply. Your path forward is unique and beautiful."
  } else {
    recommendation$game <- "Any game of your choice!"
    recommendation$quote <- "You’re doing well. Keep enjoying your journey!"
  }
  return(recommendation)
}

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  titlePanel("🎮 Online Gaming Anxiety Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Answer the questions below:"),
      hr(),
      h4("🧑 Demographic and Background Info"),
      selectInput("gender", "Gender:", choices = c("Male", "Female", "Other")),
      selectInput("whyplay", "Why do you play games?", 
                  choices = c("Stress Relief", "Entertainment", "Social Interaction", "Competition", "Other")),
      conditionalPanel(condition = "input.whyplay == 'Other'",
                       textInput("whyplay_other", "Please specify your reason:")
      ),
      numericInput("hours", "Average gaming hours per day:", value = 2, min = 0),
      selectInput("degree", "Current degree/course:", 
                  choices = c("BTech", "BSc", "BA", "MTech", "MSc", "Other")),
      conditionalPanel(condition = "input.degree == 'Other'",
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
            choices = c("0: Not at all" = 0, "1: A little bit" = 1, "2: Somewhat" = 2, "3: Very much" = 3, "4: Extremely" = 4)
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
      h3("🎯 Game Recommendation & Quote:"),
      verbatimTextOutput("recommendation"),
      hr(),
      DTOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {
  observeEvent(input$submit, {
    whyplay_input <- if (input$whyplay == "Other") {
      if (is.null(input$whyplay_other) || trimws(input$whyplay_other) == "") {
        showNotification("⚠ Please provide a valid reason for 'Why you play games'.", type = "error")
        return()
      } else {
        input$whyplay_other
      }
    } else {
      input$whyplay
    }
    
    degree_input <- if (input$degree == "Other") {
      if (is.null(input$degree_other) || trimws(input$degree_other) == "") {
        showNotification("⚠ Please provide a valid degree/qualification.", type = "error")
        return()
      } else {
        input$degree_other
      }
    } else {
      input$degree
    }
    
    question_data <- data.frame(
      t(sapply(1:29, function(i) {
        val <- input[[paste0("Q", i)]]
        if (is.null(val) || val == "") return(NA)
        as.numeric(val)
      }))
    )
    colnames(question_data) <- names(dataset)[1:29]
    
    question_data$GAD_T <- rowSums(question_data[, 1:7], na.rm = TRUE)
    question_data$SWL_T <- rowSums(question_data[, 8:12], na.rm = TRUE)
    question_data$SPIN_T <- rowSums(question_data[, 13:29], na.rm = TRUE)
    
    question_data$whyplay <- whyplay_input
    question_data$gender <- input$gender
    question_data$hours <- input$hours
    question_data$degree <- degree_input
    question_data$work <- input$work
    
    input_data <- question_data[, feature_columns, drop = FALSE]
    
    prediction <- tryCatch({
      predict(rf_model, input_data)
    }, error = function(e) {
      return(paste("Prediction error:", e$message))
    })
    
    output$result <- renderText({
      paste("🎯 Predicted Anxiety Type:", prediction)
    })
    
    # Add recommendation output
    output$recommendation <- renderText({
      rec <- recommend_content(as.character(prediction))
      paste("Recommended Game:", rec$game, "\nQuote:", rec$quote)
    })
    
    output$table <- renderDT({
      display_data <- data.frame(
        Gender = input$gender,
        Why_Play = whyplay_input,
        Hours_Per_Day = input$hours,
        Degree = degree_input,
        Working = input$work,
        GAD_Total = question_data$GAD_T,
        SWL_Total = question_data$SWL_T,
        SPIN_Total = question_data$SPIN_T,
        Predicted_Anxiety_Type = prediction
      )
      datatable(display_data, options = list(dom = 't', scrollX = TRUE, ordering = FALSE)) %>%
        formatStyle(names(display_data), textAlign = 'center')
    })
  })
}

# Run App
shinyApp(ui = ui, server = server)
