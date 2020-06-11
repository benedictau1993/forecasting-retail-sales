########################################################
############ BARK Regional Sales Prediction ############
############    UChicago TS | June, 2020    ############
########################################################

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)


fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("united"),
    # theme = "lux.css",
    # theme = "sketchy.css",
    # theme = "litera.css",
    # items <- c("FOODS_3_586", "FOODS_3_252", "FOODS_3_555", "FOODS_3_101", "FOODS_3_007")

    titlePanel("BARK® Regional Sales Prediction"),
    
    sidebarLayout(
        sidebarPanel(
            # Only enable controls after system has been initialised
            disabled(selectInput("state", "Select state:", c("CA", "TX", "WI"))),
            disabled(selectInput("category", "Select product category:", c("HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2", "FOODS_1", "FOODS_2", "FOODS_3"))),
            disabled(htmlOutput("item_selector")),
            disabled(sliderInput(inputId="holdout_days", label="Holdout (days):", min=30, max=360, value=90)),
            disabled(sliderInput(inputId="window", label="CV rolling window width (days):", min=365, max=1400, value=1300)),
            disabled(sliderInput(inputId="h", label="CV forecast steps (days):", min=1, max=60, value=30)),
            tags$b("Select models:"),
            disabled(checkboxInput("t_naive", "Naive", TRUE)),
            disabled(checkboxInput("t_arima", "ARIMA", FALSE)),
            disabled(checkboxInput("t_sarima", "Seasonal ARIMA", FALSE)),
            disabled(checkboxInput("t_arfima", "Fractional ARIMA", FALSE)),
            disabled(checkboxInput("t_hw", "Holt-Winters", FALSE)),
            disabled(checkboxInput("t_stl", "STL Decomposition", FALSE)),
            disabled(checkboxInput("t_crost", "Croston's method", FALSE))
        ),

        mainPanel(
            textOutput("hist_avg_r"),
            textOutput("actu_avg_r"),
            textOutput("pred_avg_r"),
            withSpinner(plotOutput("plot_r"), color="#0dc5c1"), 
            dataTableOutput('results_df') 
        )
    )
)
