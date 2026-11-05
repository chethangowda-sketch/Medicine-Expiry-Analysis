
# ==========================================
# MEDICINE EXPIRY ANALYSIS - SHINY DASHBOARD
# ==========================================

# Install packages (run only once if required)
packages <- c("shiny", "bslib", "ggplot2", "dplyr", "DT")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)

# ------------------------------------------
# USER INTERFACE
# ------------------------------------------

ui <- navbarPage(
  title = "Medicine Inventory Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  header = tags$div(
    style = "padding: 12px 20px;",
    h4("Medicine Expiry & Stock Management System"),
    p("Explore medicine inventory, expiry status, sales and revenue.")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Upload Dataset"),
      
      fileInput(
        "file",
        "Choose Medicine CSV File",
        accept = ".csv"
      ),
      
      helpText("Upload your medicine inventory CSV file."),
      
      hr(),
      
      h4("Filters"),
      
      uiOutput("manufacturer_ui"),
      
      uiOutput("status_ui"),
      
      actionButton(
        "reset",
        "Reset Filters",
        class = "btn-warning"
      ),
      
      hr(),
      
      downloadButton(
        "download",
        "Download Filtered Data",
        class = "btn-success"
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        # ----------------------------------
        # TAB 1: OVERVIEW
        # ----------------------------------
        
        tabPanel(
          "Overview",
          
          br(),
          
          h3("Dashboard Overview"),
          
          fluidRow(
            column(3, wellPanel(
              h5("Total Medicines"),
              h2(textOutput("total_medicines"))
            )),
            
            column(3, wellPanel(
              h5("Expired Medicines"),
              h2(textOutput("expired_count"))
            )),
            
            column(3, wellPanel(
              h5("Expiring Soon"),
              h2(textOutput("expiring_count"))
            )),
            
            column(3, wellPanel(
              h5("Total Revenue"),
              h2(textOutput("total_revenue"))
            ))
          ),
          
          fluidRow(
            column(6,
                   plotOutput("expiry_plot", height = "350px")
            ),
            
            column(6,
                   plotOutput("manufacturer_plot", height = "350px")
            )
          )
        ),
        
        # ----------------------------------
        # TAB 2: EXPIRY ANALYSIS
        # ----------------------------------
        
        tabPanel(
          "Expiry Analysis",
          
          br(),
          
          h3("Medicine Expiry Analysis"),
          
          plotOutput("expiry_detail_plot", height = "400px"),
          
          h4("Medicine Expiry Details"),
          
          DTOutput("expiry_table")
        ),
        
        # ----------------------------------
        # TAB 3: STOCK ANALYSIS
        # ----------------------------------
        
        tabPanel(
          "Stock Management",
          
          br(),
          
          h3("Medicine Stock Analysis"),
          
          fluidRow(
            column(4, wellPanel(
              h5("Total Remaining Stock"),
              h3(textOutput("remaining_stock"))
            )),
            
            column(4, wellPanel(
              h5("Low Stock Medicines"),
              h3(textOutput("low_stock_count"))
            )),
            
            column(4, wellPanel(
              h5("High Stock Medicines"),
              h3(textOutput("high_stock_count"))
            ))
          ),
          
          plotOutput("stock_plot", height = "400px"),
          
          DTOutput("stock_table")
        ),
        
        # ----------------------------------
        # TAB 4: SALES ANALYSIS
        # ----------------------------------
        
        tabPanel(
          "Sales & Revenue",
          
          br(),
          
          h3("Sales and Revenue Analysis"),
          
          fluidRow(
            column(6, wellPanel(
              h5("Total Quantity Sold"),
              h3(textOutput("quantity_sold"))
            )),
            
            column(6, wellPanel(
              h5("Average Revenue"),
              h3(textOutput("average_revenue"))
            ))
          ),
          
          plotOutput("revenue_plot", height = "400px"),
          
          DTOutput("revenue_table")
        ),
        
        # ----------------------------------
        # TAB 5: MANUFACTURER ANALYSIS
        # ----------------------------------
        
        tabPanel(
          "Manufacturer Analysis",
          
          br(),
          
          h3("Manufacturer-wise Analysis"),
          
          plotOutput("manufacturer_revenue_plot",
                     height = "400px"),
          
          DTOutput("manufacturer_table")
        ),
        
        # ----------------------------------
        # TAB 6: DATASET
        # ----------------------------------
        
        tabPanel(
          "Medicine Records",
          
          br(),
          
          h3("Complete Medicine Dataset"),
          
          DTOutput("medicine_table")
        )
      )
    )
  )
)

# ------------------------------------------
# SERVER
# ------------------------------------------

server <- function(input, output, session) {
  
  # Read uploaded CSV file
  
  raw_data <- reactive({
    
    req(input$file)
    
    df <- read.csv(
      input$file$datapath,
      stringsAsFactors = FALSE
    )
    
    required <- c(
      "Medicine_ID",
      "Medicine_Name",
      "Manufacturer",
      "Stock_Quantity",
      "Quantity_Sold",
      "Remaining_Stock",
      "Manufacture_Date",
      "Expiry_Date",
      "Sales_Date",
      "Price_Per_Unit_INR",
      "Sales_Revenue_INR"
    )
    
    missing_cols <- setdiff(required, names(df))
    
    validate(
      need(
        length(missing_cols) == 0,
        paste("Missing columns:", paste(missing_cols,
                                        collapse = ", "))
      )
    )
    
    # Convert dates
    
    df$Manufacture_Date <- as.Date(
      df$Manufacture_Date
    )
    
    df$Expiry_Date <- as.Date(
      df$Expiry_Date
    )
    
    df$Sales_Date <- as.Date(
      df$Sales_Date
    )
    
    # Calculate expiry days
    
    df$Days_To_Expiry <- as.numeric(
      df$Expiry_Date - Sys.Date()
    )
    
    # Categorise expiry status
    
    df$Expiry_Status <- ifelse(
      is.na(df$Days_To_Expiry), "Unknown",
      ifelse(df$Days_To_Expiry < 0, "Expired",
             ifelse(df$Days_To_Expiry <= 30,
                    "Expiring Within 30 Days",
                    ifelse(df$Days_To_Expiry <= 90,
                           "Expiring Within 90 Days",
                           "Safe")))
    )
    
    df
  })
  
  # Manufacturer filter
  
  output$manufacturer_ui <- renderUI({
    
    req(raw_data())
    
    selectInput(
      "manufacturer",
      "Select Manufacturer",
      choices = c(
        "All",
        sort(unique(raw_data()$Manufacturer))
      ),
      selected = "All"
    )
  })
  
  # Expiry status filter
  
  output$status_ui <- renderUI({
    
    req(raw_data())
    
    selectInput(
      "status",
      "Select Expiry Status",
      choices = c(
        "All",
        "Expired",
        "Expiring Within 30 Days",
        "Expiring Within 90 Days",
        "Safe",
        "Unknown"
      ),
      selected = "All"
    )
  })
  
  # Reset filters
  
  observeEvent(input$reset, {
    
    updateSelectInput(
      session,
      "manufacturer",
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "status",
      selected = "All"
    )
  })
  
  # Apply filters
  
  filtered_data <- reactive({
    
    df <- raw_data()
    
    if (!is.null(input$manufacturer) &&
        input$manufacturer != "All") {
      
      df <- df %>%
        filter(Manufacturer == input$manufacturer)
    }
    
    if (!is.null(input$status) &&
        input$status != "All") {
      
      df <- df %>%
        filter(Expiry_Status == input$status)
    }
    
    df
  })
  
  # ----------------------------------
  # OVERVIEW OUTPUTS
  # ----------------------------------
  
  output$total_medicines <- renderText({
    nrow(filtered_data())
  })
  
  output$expired_count <- renderText({
    sum(filtered_data()$Expiry_Status == "Expired")
  })
  
  output$expiring_count <- renderText({
    sum(filtered_data()$Expiry_Status %in%
          c("Expiring Within 30 Days",
            "Expiring Within 90 Days"))
  })
  
  output$total_revenue <- renderText({
    paste0(
      "₹ ",
      format(
        sum(filtered_data()$Sales_Revenue_INR,
            na.rm = TRUE),
        big.mark = ",",
        digits = 2
      )
    )
  })
  
  # Expiry chart
  
  output$expiry_plot <- renderPlot({
    
    df <- filtered_data()
    
    ggplot(df, aes(x = Expiry_Status,
                   fill = Expiry_Status)) +
      geom_bar() +
      labs(
        title = "Medicine Expiry Distribution",
        x = "Expiry Status",
        y = "Number of Medicines"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Manufacturer chart
  
  output$manufacturer_plot <- renderPlot({
    
    df <- filtered_data()
    
    manufacturer_data <- df %>%
      count(Manufacturer)
    
    ggplot(
      manufacturer_data,
      aes(x = reorder(Manufacturer, n),
          y = n)
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Manufacturer-wise Medicine Count",
        x = "Manufacturer",
        y = "Medicine Count"
      ) +
      theme_minimal()
  })
  
  # ----------------------------------
  # EXPIRY ANALYSIS
  # ----------------------------------
  
  output$expiry_detail_plot <- renderPlot({
    
    df <- filtered_data()
    
    ggplot(
      df,
      aes(x = Expiry_Status,
          y = Remaining_Stock,
          fill = Expiry_Status)
    ) +
      geom_boxplot() +
      labs(
        title = "Remaining Stock by Expiry Status",
        x = "Expiry Status",
        y = "Remaining Stock"
      ) +
      theme_minimal()
  })
  
  output$expiry_table <- renderDT({
    
    df <- filtered_data() %>%
      select(
        Medicine_ID,
        Medicine_Name,
        Manufacturer,
        Expiry_Date,
        Days_To_Expiry,
        Remaining_Stock,
        Expiry_Status
      )
    
    datatable(
      df,
      filter = "top",
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ----------------------------------
  # STOCK ANALYSIS
  # ----------------------------------
  
  output$remaining_stock <- renderText({
    sum(filtered_data()$Remaining_Stock,
        na.rm = TRUE)
  })
  
  output$low_stock_count <- renderText({
    sum(filtered_data()$Remaining_Stock < 100,
        na.rm = TRUE)
  })
  
  output$high_stock_count <- renderText({
    sum(filtered_data()$Remaining_Stock > 1000,
        na.rm = TRUE)
  })
  
  output$stock_plot <- renderPlot({
    
    df <- filtered_data() %>%
      arrange(desc(Remaining_Stock)) %>%
      head(10)
    
    ggplot(
      df,
      aes(x = reorder(Medicine_Name,
                      Remaining_Stock),
          y = Remaining_Stock)
    ) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(
        title = "Top 10 Medicines by Remaining Stock",
        x = "Medicine",
        y = "Remaining Stock"
      ) +
      theme_minimal()
  })
  
  output$stock_table <- renderDT({
    
    df <- filtered_data() %>%
      arrange(Remaining_Stock) %>%
      select(
        Medicine_Name,
        Manufacturer,
        Remaining_Stock,
        Expiry_Date,
        Expiry_Status
      )
    
    datatable(
      df,
      filter = "top",
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ----------------------------------
  # SALES AND REVENUE
  # ----------------------------------
  
  output$quantity_sold <- renderText({
    sum(filtered_data()$Quantity_Sold,
        na.rm = TRUE)
  })
  
  output$average_revenue <- renderText({
    paste0(
      "₹ ",
      round(
        mean(filtered_data()$Sales_Revenue_INR,
             na.rm = TRUE),
        2
      )
    )
  })
  
  output$revenue_plot <- renderPlot({
    
    df <- filtered_data() %>%
      group_by(Medicine_Name) %>%
      summarise(
        Revenue = sum(Sales_Revenue_INR,
                      na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Revenue)) %>%
      head(10)
    
    ggplot(
      df,
      aes(x = reorder(Medicine_Name, Revenue),
          y = Revenue)
    ) +
      geom_col(fill = "seagreen") +
      coord_flip() +
      labs(
        title = "Top 10 Revenue Generating Medicines",
        x = "Medicine",
        y = "Revenue (INR)"
      ) +
      theme_minimal()
  })
  
  output$revenue_table <- renderDT({
    
    df <- filtered_data() %>%
      arrange(desc(Sales_Revenue_INR)) %>%
      select(
        Medicine_Name,
        Manufacturer,
        Quantity_Sold,
        Price_Per_Unit_INR,
        Sales_Revenue_INR
      )
    
    datatable(
      df,
      filter = "top",
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ----------------------------------
  # MANUFACTURER ANALYSIS
  # ----------------------------------
  
  manufacturer_summary <- reactive({
    
    filtered_data() %>%
      group_by(Manufacturer) %>%
      summarise(
        Medicine_Count = n(),
        Total_Stock = sum(Remaining_Stock,
                          na.rm = TRUE),
        Expired_Count = sum(
          Expiry_Status == "Expired",
          na.rm = TRUE
        ),
        Total_Revenue = sum(
          Sales_Revenue_INR,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  })
  
  output$manufacturer_revenue_plot <- renderPlot({
    
    df <- manufacturer_summary()
    
    ggplot(
      df,
      aes(x = reorder(Manufacturer,
                      Total_Revenue),
          y = Total_Revenue)
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Manufacturer-wise Revenue",
        x = "Manufacturer",
        y = "Revenue (INR)"
      ) +
      theme_minimal()
  })
  
  output$manufacturer_table <- renderDT({
    
    datatable(
      manufacturer_summary(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ----------------------------------
  # COMPLETE MEDICINE RECORDS
  # ----------------------------------
  
  output$medicine_table <- renderDT({
    
    datatable(
      filtered_data(),
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ----------------------------------
  # DOWNLOAD FILTERED DATA
  # ----------------------------------
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(
        "Medicine_Analysis_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
}

# ------------------------------------------
# RUN APPLICATION
# ------------------------------------------

shinyApp(ui = ui, server = server)
