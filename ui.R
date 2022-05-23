library(shiny)

# Define UI 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Nutrition Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age", value=100, min=0, max=100, step=1),
      radioButtons("gender", "Gender", c("Male","Female")),
      numericInput("weight", "What is your weight? (in kg)", value=1000, min=1, max=1000, step=0.01),
      numericInput("height", "What is your height? (in m)", value=1000, min=1, max=1000, step=0.01),
      selectInput("activityLevel", "Activity Level", c("Sedentary", "Lightly Active", "Moderately Active", "Very Active", "Extremely Active"), selected=NULL, multiple=FALSE),
      submitButton("Submit"),
    ),
    
    
    mainPanel(
      tags$head(tags$style('h4 {color:blue;}')),
      h4("Your Body Mass Index (BMI) is"),
      verbatimTextOutput("bmi"),
      
      h4("Your Basal Metabolic Rate (BMR) is"),
      verbatimTextOutput("bmr"),
      
      h4("Your Total Daily Energy Expenditure (TDEE) is"),
      verbatimTextOutput("tdee"),
      
      h4("Your ideal weight (in kg) is"),
      verbatimTextOutput("goalweight"),
      verbatimTextOutput("bmiStatus"),
      
      h4("Your daily calorie should be"),
      verbatimTextOutput("dailyCalorie"),
      h4("Your current progress"),
      #verbatimTextOutput("progress"),
      plotOutput("progress")
      
    )
  )
))
