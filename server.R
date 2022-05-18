library(shiny)
library(ggplot2)

bmi <- function(weight, height){
  weight/(height*height)
}

bmr <- function(age, gender, weight, height){
   if(gender=="Male"){
     bmr1 <- 66 + (13.7*weight) + (5*(height*100)) - (6.8*age)
   }else if(gender=="Female"){
     bmr2 <- 655 + (9.6*weight) + (1.8*(height*100)) - (4.7*age)
   }
  }
  
tdee <- function(activityLevel,bmr){
  if(activityLevel=="Sedentary"){
    tdee1 <- bmr*1.2
  }else if(activityLevel=="Lightly Active"){
    tdee2 <- bmr*1.375
  }else if(activityLevel=="Moderately Active"){
    tdee3 <- bmr*1.55
  }else if(activityLevel=="Very Active"){
    tdee4 <- bmr*1.725
  }else if(activityLevel=="Extremely Active"){
    tdee5 <- bmr*1.9
  }
}



minIdealWeight <- function(height){
  minIdealWeight <- round(18.5*(height*height), digits=2)
}
  
maxIdealWeight <- function(height){
  maxIdealWeight <- round(24.9*(height*height), digits=2)
}

idealWeight <- function(minIdealWeight, maxIdealWeight){
  print(minIdealWeight,"to", maxIdealWeight)
}

bmiStatus <- function(bmi){
  
  if(bmi<18.5){
    print("You are underweight. You need to gain weight.")
  }else if(bmi>18.5 && bmi<24.9){
    print("You are normal. Keep on maintaining!")
  }else if(bmi>24.9){
    print("You are overweight. You have to lose weight.")
  }
}

dailyCalorie <- function(tdee, bmi){
  if(bmi<18.5){   #to gain weight
    dailyCalorie1 <- tdee + 1000
  }else if(bmi>18.5 && bmi<24.9){
    dailyCalorie2 <- tdee
  }else if(bmi>24.9){
    dailyCalorie3 <- tdee-1000
  }
}

# progress
library(ggplot2)
progress <- function(bmi, height, weight){
data <- data.frame(category=c("BMI", "Height", "Weight"), count=c(bmi,height,weight))

# compute percentage
data$fraction <- data$count/ sum(data$count)

# compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# compute the bottom of each rectangle
data$ymin <- c(0,head(data$ymax, n=-1))

# compute label position
data$labelPosition <- (data$ymax + data$ymin)/2

# compute a good label
data$label <- paste0(data$category,"\n value:", data$count)



}

shinyServer(function(input, output) {

    output$bmi <- renderText({bmi(input$weight, input$height)})
    output$bmr <- renderText({bmr(input$age, input$gender, input$weight, input$height)})
    output$tdee <- renderText({tdee(input$activityLevel, bmr(input$age, input$gender, input$weight, input$height))})
    output$goalweight <- renderText({noquote(paste0(minIdealWeight(input$height), " to ", (maxIdealWeight(input$height))))})
    output$bmiStatus <- renderText({bmiStatus(bmi(input$weight, input$height))})
    output$dailyCalorie <- renderText(dailyCalorie(tdee(input$activityLevel, bmr(input$age, input$gender, input$weight, input$height)),bmi(input$weight, input$height)))
    output$progress <- renderPlot({ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_text(x=1, aes(y=labelPosition, label=label, color=category), size=4) + # x here controls label position (inner / outer)
        scale_fill_brewer(palette=3) +
        scale_color_brewer(palette=3) +
        coord_polar(theta="y") +
        xlim(c(-4, 4)) +
        theme_void() +
        theme(legend.position = "none")})
})


