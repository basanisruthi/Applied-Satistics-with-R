library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(shinydashboard)
library(tidyverse)
library(broom)
library(kableExtra)
library(vctrs)
library(rlang)
library(shinythemes)
library(corrplot)
library(dplyr)
library(readr)
# Set the path to the file
file_path <- "C:/Users/User/Desktop/MITB projects/Applied_stats/ASR_Group_Project/project_final_files/all_data_cleaned_final.csv"

# Read the file using read.csv() function
data <- read.csv(file_path, header=TRUE, stringsAsFactors = FALSE)


## Shiny UI component for the Dashboard
# Define UI
ui <-  dashboardPage(
  dashboardHeader(title = "HDB ReSale Analysis"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem("Visualization", tabName = "viz", icon=icon("bar-chart")),
      menuItem("Linear Regression", tabName = "lm", icon=icon("line-chart"))
      
    )
  ),
  dashboardBody(tabItems(
    ## First tab item
    tabItem(tabName = "data", 
            tabBox(id="t1", width = 10, 
                   tabPanel("About", icon=icon("address-card"),
                            fluidRow(
                              column(width = 10, tags$img(src="danist-soh.jpg", width =600 , height = 300),
                                     tags$br() , 
                                     tags$a("Photo by danist soh on Unsplash"), align = "center"),
                              column(width = 10, tags$br() ,
                                     tags$p("The Singapore housing market is experiencing a boom due to monetary policies both within and outside the country; 
                                            housing affordability has thus become a key concern for the Singaporeans. 
                                            This app will attempt to  provide some key insights on affordability to address these concerns, 
                                            particularly for the HDB resale market who despite a global economic downturn due to the federal reserve’s tightening starting March 2022, 
                                            has managed to see growth till the time this app was made. ")
                              )
                            )
                            
                            
                   ),
                   tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")), 
                   tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                   tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
            )
            
    ),
    #second Tab Item
    tabItem(tabName = "viz",
            tabBox(id="t2",width = 12,
                   tabPanel("Popular Flat Type", value="distro",fluidRow(
                     column(width = 4, selectInput("year_input", label = "Select Year", choices = unique(data$Year_Extract))),
                     column(width = 4, selectInput("region_input", label = "Select Region", choices = unique(data$Planning_Region_from_resale_data)))
                   ),
                   plotOutput("pop_flat_barplot", height = "500px")),
                   tabPanel("Corelation Matrix", value="trends",plotOutput("corrPlot",height="500px")),
                   tabPanel("Boxplot", value="values",
                            fluidRow(
                              column(width = 12, 
                                     selectInput("year_input_boxplot", label = "Select Year", choices = unique(data$Year_Extract)),
                                     plotOutput("boxplot", height = "500px")
                   )
                   )
                   ))),
    
# fourth tab item
tabItem(tabName = "lm",
        fluidRow(
          column(4, wellPanel(
            h4("Select Variables"),
            selectInput("dep_var", "Dependent Variable:", choices = colnames(data)),
            selectInput("ind_vars", "Independent Variables:", choices = colnames(data), multiple = TRUE),
            br(),
            actionButton("lm_run", "Run Linear Regression"),
            downloadButton("lm_download", "Download Results")
          )),
          mainPanel(
            verbatimTextOutput("lm_summary"),
            plotOutput("fitted_vs_residual_plot")
          )
        )
)
)
)
)

# Server component of the 
server <- function(input, output) {
  #Structure
  output$structure <- renderPrint({
    data %>% 
      str()
  })
  #Summary
  output$summary <- renderPrint({
    data %>% 
      summary()
  })
  #DataTable
  output$dataT <- renderDataTable(data)
  #popular flat
  output$pop_flat_barplot <- renderPlot({
    year_selected <- input$year_input
    region_selected <- input$region_input
    
    HDB_resale_main_data_filtered <- data %>% 
      filter(Year_Extract == year_selected, Planning_Region_from_resale_data == region_selected) %>%
      count(flat_type) %>%
      arrange(desc(n)) %>%
      head(10)
    
    ggplot(HDB_resale_main_data_filtered, aes(x = reorder(flat_type, n), y = n)) + 
      geom_col(fill = "darkseagreen4") +
      labs(title = paste0("Most Popular Flat Type Sold in ", region_selected, " in ", year_selected), x = "Flat Type", y = "Number of Sales")
  })
  
  #Corelation plot
  output$corrPlot <- renderPlot({
    #corelation_matrix chart 11.
    # select the columns of interest
    vars_of_interest <- c("Year_Extract", "PSM", "Household_Net_Worth", "Assets", 
                          "Mortgage_Loans","remaining_lease",
                          "CPF_intrest_rate","Average_of_SORA","Liabilities",
                          "floor_area_sqm","median_rent","liabilities_assest_ratio",
                          "mortgage_asset_ratio")
    
    data_clean <- data[complete.cases(data[vars_of_interest]),]
    # calculate the correlation matrix
    correlation_matrix <- cor(data_clean[vars_of_interest])
    
    corrplot(correlation_matrix, method="circle", type="upper", order="hclust", tl.col="black", tl.srt=45)
  })
  
  # Define the linear regression model
  lm_model <- reactive({
    if (length(input$ind_vars) == 0) {
      return(NULL)
    }
    formula <- paste("PSM ~", paste(input$ind_vars, collapse = " + "))
    lm(formula, data = data)
  })
  
  observeEvent(input$lm_run, {
    lm_res <- lm_model()
    if (is.null(lm_res)) {
      return()
    }
    
    # Create the fitted vs residual plot
    output$fitted_vs_residual_plot <- renderPlot({
      ggplot(data.frame(fitted = lm_res$fitted.values, residual = lm_res$residuals), aes(x = fitted, y = residual)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        xlab("Fitted Values") +
        ylab("Residuals") +
        ggtitle("Fitted vs Residual Plot")
    })
    # Print the plot of the model
    output$lm_plot <- renderPlot({
      plot(lm_res, col = "blue", las = 1, cex.axis = 0.8, main = "Linear Regression Results")
    })
    # Print the summary of the model
    output$lm_summary <- renderPrint({
      summary(lm_res)
    })
  })
  #boxplot
  output$boxplot <- renderPlot({
    year_selected <- input$year_input_boxplot
    HDB_resale_main_data_filtered <- data %>% filter(Year_Extract == year_selected)
    ggplot(HDB_resale_main_data_filtered, aes(x = factor(Planning_Region_from_resale_data), y = PSM, color = factor(flat_type))) +  
      geom_boxplot() + 
      labs(title = paste0("HDB Resale Price (sqm) by Flat Type & Region Boxplot (", year_selected, ")"), x = "Planning Region", y = "Price per squared meter (SGD)")
  })
  
  # Download the linear regression results
  output$lm_download <- downloadHandler(
    filename = function() {
      paste("linear_regression_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      lm_res <- lm_model()
      if (is.null(lm_res)) {
        return(NULL)
      }
      write.table(summary(lm_res), file, sep = "\t", row.names = TRUE)
    }
  ) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
