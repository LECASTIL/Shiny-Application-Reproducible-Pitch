library(shiny)
ui<-fluidPage(
    titlePanel("Regression Model (Dataset: Swiss)"),
    sidebarLayout(
        sidebarPanel(
            selectInput("outcome", label = h3("Outcome"),
                        choices = list("Fertility" = "Fertility",
                                       "Agriculture" = "Agriculture",
                                       "Examination" = "Examination",
                                       "Education" = "Education",
                                       "Catholic" = "Catholic",
                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1),
            
            selectInput("indepvar", label = h3("Explanatory variable"),
                        choices = list("Fertility" = "Fertility",
                                       "Agriculture" = "Agriculture",
                                       "Examination" = "Examination",
                                       "Education" = "Education",
                                       "Catholic" = "Catholic",
                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1)
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                        tabPanel("Distribution", # Plots of distributions
                                 fluidRow(
                                     column(6, plotOutput("distribution1")),
                                     column(6, plotOutput("distribution2")))
                        ),
                        tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                        tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    ))
