side_panel_info <- sidebarPanel(
  sliderInput(
    inputId = "plot_year",
    label = h3("Year Range"),
    min=2000, max=2018, value = c(2005, 2015), sep="", ticks=FALSE
  ),
  checkboxGroupInput(
    inputId = "categories",
    label = h3("Categories to Plot"),
    selected = "Wasting", "Overweight", "Underweight", "Stunting", 
    inline = TRUE, 
    choiceNames = "Wasting", "Overweight", "Underweight", "Stunting"
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