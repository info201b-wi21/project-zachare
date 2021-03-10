source("expenditures_and_malnourishment.R")

ui <- fluidPage(
  
  titlePanel("Health Expenditures and Malnourishment"),

    sidebarLayout(
  
      sidebarPanel(
    
        sliderInput(
          inputId = "plot_year",
          label = h3("Year Range to Plot"),
          min=2011, max=2018, value = c(2012, 2016), sep="", ticks=FALSE
        ),
  
        br(),
  
  
        checkboxGroupInput(
          inputId = "categories",
          label = h3("Categories to Plot"),
          selected = c("Wasting", "Overweight", "Underweight", "Stunting"), 
          inline = FALSE, 
          choices = c("Wasting", "Overweight", "Underweight", "Stunting")
       )
      ),
       
       mainPanel(
         tabsetPanel(type = "tabs",
                     tabPanel("Health Expenditures and Malnourishment", plotOutput("correlation_plot"))
  )
)
)

)
