source("expenditures_and_malnourishment.R")

side_panel_info <- sidebarPanel(
  sliderInput(
    inputId = "plot_year",
    label = h3("Year Range to Plot"),
    min=2011, max=2018, value = c(2012, 2016), sep="", ticks=FALSE
  ),
  checkboxGroupInput(
  inputId = "categories",
  label = h3("Categories to Plot"),
  selected = c("Wasting", "Overweight", "Underweight", "Stunting"), 
  inline = FALSE, 
  choices = c("Wasting", "Overweight", "Underweight", "Stunting")
  )
)


malnourishment_plot <- tabPanel("Health Expenditures and Malnourishment",
                                sidebarLayout(
                                  side_panel_info,
                                  mainPanel(
                                    plotOutput("correlation_plot")
                                  )
                                )
)

ui <- navbarPage(strong("Health Expenditures and Malnourishment"), malnourishment_plot)
