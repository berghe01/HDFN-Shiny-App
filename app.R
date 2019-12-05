
library(shiny)
library(shinythemes)
library(ggplot2)



ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel(
    h2("Projected decline in fetal hematocrit post-intrauterine transfusion", align = "left")
  ),
  
  titlePanel(
    h3("By method of transfusion", align = "left")
  ),
  
  titlePanel(
    h5("DISCLAIMER: Illustrative example only, NOT to be used for clinical decision making!")
  ),
  
  hr(),
  
  
  sidebarPanel(
    
    verticalLayout(
      
      h4("Adjust the inputs below to see why the combination of \nIVT + IPT for fetal transfusion results in a post-transfusion fetal hct that is within the target range longer than IVT alone", align = "left"),
      
      br(),
      
      numericInput("hct", "Input post-transfusion final fetal hematocrit(%)",
                   value = 45, min = 0, max = 80, 
                   width = "300px"),
      
      sliderInput("day", "Select gestational age at procedure and future date (days)",

                  min = 112, max = 245, 
                  value = c(140, 170))
    )
  ),
  
  
  mainPanel(
    plotOutput(
      
      "hct_plot", width = "100%"
      
    ),
    
    hr(),
    

    tags$div(
      HTML(paste("Projections", tags$sup(1), " based on 0.1 (± 0.4)% and 1.06 (±0.68)% decrease in fetal hct per day with IVT+IPT vs. IVT alone, respectively", sep = "")) 
    ),
    
    h5("1. Moise Jr, Kenneth J., et al. Comparison of four types of intrauterine transfusion: effect on fetal hematocrit. Fetal Diagnosis and Therapy 4.2-3 (1989): 126-137.")
  )
)




server <- function(input, output) {
  
  
  
  # create reactive plot
  
  
  
  
  
  output$hct_plot <- renderPlot({
    
    ggplot(data = data.frame(ga = seq(input$day[1], input$day[2], 1)),
           
           aes(x = ga))+
      
      stat_function(fun = function(x) input$hct -1.06*x + 1.06*x[1], size = 1.5, geom = "line", aes(color = "IVT alone")) +
      stat_function(fun = function(x) input$hct -0.1*x + 0.1*x[1], size = 1.5, geom = "line",  aes(color = "IVT + IPT")) +
      geom_ribbon(aes(ymax = 50, ymin = 40), fill = "pink", color = "lightpink", alpha = 0.1)+
      
      labs(color = "IUT method")+
      
      ylim(12,55)+
      
      xlim(input$day[1], input$day[2]) +
      
      labs(title = "Estimated hct decline per week post-transfusion, by method",

           subtitle = "*Rate of decline may vary by degree to which fetal erythropoesis is suppressed",

           x= "\nGestational age (days)",
           y = "Fetal Hct (%)",
           caption = 
             "\nHCT, hematocrit; IUT, intrauterine transfusion; \nIVT, intravenous transfusion; IPT, intraperitoneal transfusion")+
      
      geom_hline(yintercept = 30, size = 0.5, linetype = "dashed", color = "black") +
      
      geom_vline(xintercept = input$day[1] + 7, size = 0.25, linetype = "dashed", color = "purple",
                 
                 show.legend = TRUE) +
      
      geom_label(aes(x = input$day[1] + 7, y = 22, label = "Week 1"), color = "black", fill = "pink") +
      
      geom_vline(xintercept = input$day[1] + 14, size = 0.25, linetype = "dashed", color = "purple",
                 
                 show.legend = TRUE) +
      
      geom_label(aes(x = input$day[1] + 14, y = 18, label = "Week 2"), color = "black", fill = "pink") +
      
      geom_vline(xintercept = input$day[1] + 21, size = 0.25, linetype = "dashed", color = "purple",
                 
                 show.legend = TRUE) +
      
      geom_label(aes(x = input$day[1] + 21, y = 14, label = "Week 3"), color = "black", fill = "pink") +
      
      
      geom_vline(xintercept = input$day[1], size = 0.25, linetype = "dashed", color = "purple",
                 
                 show.legend = TRUE) +
      
      geom_label(aes(x = input$day[1], y = input$hct+4, label = "Post-\nhct"), color = "black", fill = "pink") +
      geom_label(aes(x = input$day[2]-5, y = 30, label = "Severe anemia, \nHct 30% or below"), color = "black", fill = "white")+
      geom_text(aes(x = input$day[2]-5, y = 51.5, label = "Target hct range: 40-50%"), color = "red", check_overlap = TRUE)+
      theme_minimal() +
      theme(plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 17, face = "italic"),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 17),
            plot.caption = element_text(size = 16, face = "italic", color = "grey39"),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 16)) 

 
    
  })
  
 
  
  
  
  
}




shinyApp(ui, server)
