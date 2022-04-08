#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



## Steps


## UI

# User input, variable

## main panel, plot variable
## main panel, table weighted freqs

## Perform a weighted crosstab of user selected variables
## plot raw values???


## Server

# Load data
# define list of variables for analysis and thier plot type




## Using derek's code

# select value
## checkbox of values to include
## toggle on/off for missing

## table and plot given variable
## radio button for bar plot vs line plot

## additional tabs for dan's stuff
#### hhs with extended family - expected to rise after 2017
#### age distributions over time - 5 panel likely easier to plot/understand than grouped versions


##### notes

## check box to filter out missing vlaues

## 3 types of graphs: one for continuous, one for dichotomous (trend in pcts, pct of children in hhs), one for multi-categories,  

## make a vector that classifies each variable into 1 of the 3 types

##

## add val + val labels to the side bar to facilitate the filter

## subsequent tabs for crosstabs
#### grouped by a (limited) given variable, what are trends
#### drop down options within the tab?


#### stretch

## filter box for sidebar
## display val+lbl on sidebar to aid in var selection
## toggle to filter NAs/Missing

###### revising extract ####
# 
# tt <- define_extract_micro("usa", "final_api_demo_version",c("us2015b", "us2016b", "us2017b", "us2018b", "us2019b"),
#                            c( "YEAR", "SAMPLE","SERIAL", "CBSERIAL", "HHWT", "CPI99", "GQ", "COSTELEC", "HHINCOME", "NUMPREC","VACANCY", "CINETHH", "PERNUM", "PERWT", "MOMLOC", "POPLOC", "RELATE",  "RELATED", "SEX", "AGE", "MARRINYR", "DIVINYR", "WIDINYR", "FERTYR", "RACE", "RACED", "HISPAN", "HISPAND", "SPEAKENG", "EDUC", "EDUCD", "EMPSTAT","EMPSTATD", "INCTOT", "MIGRATE1", "MIGRATE1D"))
# 
# save_extract_as_json(tt, "api_extract_final.json")

##########
library(shiny)
library(ipumsr)
library(dplyr)
library(stringr)

## Set up wait for extract

## Read in Data

data_check <- list.files() %>% stringr::str_detect(".xml")
extract_path <- list.files(pattern = ".json")


# 
# if(length(extract_path!=1)){
#   warning("Error: only one extract definitions (.json) allowed in shiny folder")
# }


if(!any(data_check)){
  tt <- define_extract_from_json(extract_path, "usa") %>% submit_extract() %>% wait_for_extract() %>% download_extract()
}

ddi_path <- list.files(pattern = ".xml")

ddi <- read_ipums_ddi(ddi_path)
data <- read_ipums_micro(ddi)



#### perform educ recode from pres
# 
# college_regex <- "^[123] year(s)? of college$"
# data$EDUCD3 <- data$EDUCD %>%
#   lbl_collapse(~.val %/% 10) %>% 
#   lbl_relabel(
#     lbl(2, "Less than High School") ~.val > 0 & .val < 6,
#     lbl(3, "High school") ~.lbl == "Grade 12",
#     lbl(4, "Some college") ~str_detect(.lbl, college_regex),
#     lbl(5, "College or more") ~.val %in% c(10, 11)
#   ) %>%
#   as_factor()


select_choices <- data.frame("Var"= ddi$var_info$var_name)

names(select_choices$Var) <-  ddi$var_info$var_label

select_choices$plot_type <- c("CAT", "HIDE", "HIDE", "HIDE", "COUNT","HIDE", "HIDE", "CAT", "CONT", "CONT", "HIDE","CAT", "HIDE", "HIDE", "HIDE", "HIDE", "CAT", "HIDE", "CAT", "CONT", "DICHOT", "DICHOT", "DICHOT", "DICHOT", "CAT", "HIDE", "CAT", "HIDE", "CAT", "CAT", "HIDE", "CAT", "HIDE", "CONT", "CAT", "HIDE")


#### Create empty objects for later

out_tab <- NULL
vals <- NULL


####


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    h3("Exploring Trends Over Time: Puerto Rico Community Survey"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "Var",
                "Select Variable",
                choices = select_choices$Var[select_choices$plot_type!="HIDE"],
                selected = "AGE"
            ),
            checkboxInput(
              "drop_na",
              "Drop NA/Missing Values"
            ),
            checkboxInput(
              "grouped",
              "Unstack Barplot"
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
          tabPanel(
          "Plot",
          h3(textOutput("main_plot_lbl")),
          plotOutput("main_plot"),
          h3(textOutput("main_tab_lbl")),
          tableOutput("main_tab")
        ),
        tabPanel(
          "Metadata",
          
          h3("Variable Definition"),
          textOutput("var_desc"),
          h3("Value Labels (All Possible)"),
          tableOutput("vals_lbls")
        )
        
        
        ))
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  # input <- list("Var" = "EDUCD3","drop_na" = TRUE)
  
  
  #### Render Text Main panel
  output$main_plot_lbl <- renderText({paste("Trends in", input$Var, "over time")})
  
  output$main_tab_lbl <- renderText({paste("Weighted frequencies for", input$Var)})
  
  #### Render Metadata

  output$var_desc <- renderText({
    
    if(input$Var == "EDUCD3"){
      text <- ipums_var_desc(ddi, "EDUCD")
    } else {
      text <- ipums_var_desc(ddi, input$Var)
    }
    text
      })
  
  output$vals_lbls <- renderTable({
    if(input$Var == "EDUCD3"){
      ipums_val_labels(ddi, "EDUCD")
    }else {
      ipums_val_labels(ddi, input$Var)
    }
      }, digits = 0, align = "c")

  
  #### capture data
  
  selectedData <- reactive({
    vars <- c(input$Var, "YEAR", "HHWT", "PERWT", "CPI99", "PERNUM")
    
    if(input$Var == "EDUCD3"){
      drop_vals <- data.frame("val" = 1:length(levels(data$EDUCD3)),
                              "lbl" = levels(data$EDUCD3))
    } else {
      drop_vals <- ipums_val_labels(ddi, input$Var)
      
    }
    drop_vals_chk <- stringr::str_detect(drop_vals$lbl, "N/A") |
      stringr::str_detect(drop_vals$lbl, "[Mm]issing")
    drop_vals <- drop_vals[drop_vals_chk,]
      
    if(input$drop_na & nrow(drop_vals) > 0){
      
      data <- data[!data[[input$Var]] %in% drop_vals$val,] 
    } 
    
 
      
      data <- data %>% select(all_of(vars))
      return(data)
     
    
  })
  
    to_plot <- reactive({
      
      if (input$Var %in% c("GQ", "COSTELEC", "HHINCOME", "VACANCY", "CINETHH", "NUMPREC")) {
        out_tab <-
          xtabs(selectedData()$HHWT[selectedData()$PERNUM == 1] ~ 
                  selectedData()[[input$Var]][selectedData()$PERNUM == 1] +
                  selectedData()$YEAR[selectedData()$PERNUM == 1])
      } else {
        out_tab <-
          xtabs(selectedData()$PERWT ~ 
                  selectedData()[[input$Var]] + 
                  selectedData()$YEAR)
      }
      
      
      
      #### reformat continues variables
      
      if(input$Var %in% c("COSTELEC", "HHINCOME", "AGE", "INCTOT")){
        
        year_summary <- data.frame("Min." = numeric(),"1st Qu." = numeric(), "Median" = numeric(), "Mean" = numeric(), "3d Qu." = numeric(), "Max." = numeric())
        
        for(i in unique(selectedData()$YEAR)){
          year_summary <- rbind(year_summary, summary(selectedData()[[input$Var]][selectedData()$YEAR==i]))
        }
        
        rownames(year_summary) <- unique(selectedData()$YEAR)
        out_tab <- year_summary
        colnames(out_tab) <- c("Min.", "1st Qu.", "Median", "Mean", "3d Qu.", "Max.")
        
      } else {
        
  
        
        t_labs <- ipums_val_labels(ddi, input$Var)
        if(length(t_labs) > 0 ){
          
          tab_names <- data.frame("val" = as.numeric(rownames(out_tab)))
          tab_names <- left_join(tab_names, t_labs, by = "val")
          rownames(out_tab) <- tab_names$lbl
          
        }
      }
   
      return(out_tab)

    })




    output$main_tab <- renderTable({


        as.data.frame.matrix(to_plot())

    },
    striped = TRUE, borderd = TRUE, rownames =TRUE, digits = 0, width = "85%", align = "c"
    )


    output$main_plot <- renderPlot({
      
      cols <- rainbow(nrow(to_plot()), .6, .9)
      par("mar" = c(2,1,0,0))
     layout(matrix(c(1,1,2), ncol =3))
      
      if(input$Var %in% c("COSTELEC", "HHINCOME", "AGE", "INCTOT")){
        
        ## add a toggle to drop NA
        ## add toggle to drop special values
       
        
        boxplot(selectedData()[[input$Var]] ~ selectedData()$YEAR, col = rainbow(length(unique(selectedData()$YEAR)), .6,.9), xlab = "", ylab = "", main = "")
        
        ## line plot
      } else {
        ## stacked bar chart
        #### maybe make stacked vs grouped an option
        barplot(to_plot(),
                ylab = "",

                beside = input$grouped,
                col = cols,
                xlab = ""
        )
        par("mar" = c(0,0,0,0))
        plot(1, type = "n", bty = "n", xaxt = "n", yaxt="n", xlab = "", ylab = "")
        legend("center", legend = rownames(to_plot()), pch = 22, pt.bg = cols, cex = 1.5)
        
        
      }



        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
