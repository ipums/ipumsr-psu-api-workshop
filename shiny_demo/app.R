#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


## Perform a weighted crosstab of user selected variables
## plot raw values???

library(shiny)
library(ipumsr)
library(dplyr)

## Set up wait for extract

## Read in Data

data_check <- list.files() %>% stringr::str_detect(".xml")


if(!any(data_check)){
  tt <- define_extract_from_json("api_ex.json", "usa") %>% submit_extract() %>% wait_for_extract() %>% download_extract()
}

ddi_path <- list.files(pattern = ".xml")

ddi <- read_ipums_ddi(ddi_path)
data <- read_ipums_micro(ddi)


select_choices <- ddi$var_info$var_name
names(select_choices) <-  ddi$var_info$var_label

# to_plot <-
#     xtabs(data$HHWT ~ data$NUMPREC+data$YEAR)

## Perform any summaries that don't require user input

#### Recode NAs

data$INCTOT[data$INCTOT==9999999 |
                data$INCTOT < 0] <- NA

####


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Visualizing Effects of Hurricane Maria"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "Var",
                "Select Variable",
                choices = select_choices,
                selected = "NUMPREC"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(tabPanel(
            "Main",
            plotOutput("main"),
            tableOutput("main_tab")
        )))

    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    ## use REACTIVE to read inputs/create objects

    to_plot <- reactive({
        if(input$Var=="NUMPREC"){

            xtabs(data$HHWT ~ data[[input$Var]]+data$YEAR)
        } else {
            xtabs(data$PERWT ~ data[[input$Var]]+ data$YEAR)
        }

    })




    output$main_tab <- renderTable({


        as.data.frame.matrix(to_plot())

    },
    striped = TRUE, borderd = TRUE, rownames =TRUE
    )


    output$main <- renderPlot({

        cols <- rainbow(nrow(to_plot()), .6, .9)



            barplot(to_plot(),
                    ylab = "",
                    main = paste("Trends in", input$Var),
                    beside = TRUE,
                    col = cols,
                    xlab = ""
            )


    })
}

# Run the application
shinyApp(ui = ui, server = server)
