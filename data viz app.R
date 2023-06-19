library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)

load("C:/Users/danie/Downloads/aggData.rda")

# Color palettes for colorblindness
color_palette <- c("#004C97", "#E18700", "#009E73", "#D62628", "#0173B2", "#FDBF11", "#8C564B", "#1A1A1A", "#BEAED4", "#B17BAA") # Replace with your desired colors

# UI
ui <- fluidPage(
  titlePanel("KPI Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("hospital", "Hospital:", choices = unique(agg_dataNum$h_name)[unique(agg_dataNum$h_name) != "all"]),
      selectInput("QI", "QI:", "door_to_needle"),
      selectInput("patient_description", "Patient description:", choices = unique(agg_dataNum$subGroup)[!is.na(unique(agg_dataNum$subGroup))]),
      selectInput("by_60_45", "By 60 or 45:", choices = c("dnt_leq_60", "dnt_leq_45")),
      numericInput("startYear", "Start Year:", value = min(agg_dataNum$year)),
      numericInput("endYear", "End Year:", value = max(agg_dataNum$year)),
      selectInput("startQuarter", "Start Quarter:", choices = unique(agg_dataNum$quarter)[unique(agg_dataNum$quarter) != "all"]),
      selectInput("endQuarter", "End Quarter:", choices = unique(agg_dataNum$quarter)[unique(agg_dataNum$quarter) != "all"]),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Charts", plotlyOutput("kpiChart1")),
        tabPanel("Summary", dataTableOutput("summaryTable"))
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("chartYear", "Chart Year:", choices = unique(agg_dataNum$year))
    ),
    mainPanel(
      plotlyOutput("newChart")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter the agg_dataNum based on user inputs
  filteredData <- reactive({
    agg_dataNum %>%
      filter(
        isKPI == "TRUE",
        isYearAgg == "FALSE",
        h_name != "all",  # Exclude the "all" option
        h_name == input$hospital,
        QI == input$QI,
        subGroup == input$patient_description,
        nameOfAggr == input$by_60_45,
        year >= input$startYear,
        year <= input$endYear,
        quarter >= input$startQuarter,
        quarter <= input$endQuarter
      )
  })
  
  # Calculate average percentage for each hospital and year
  avgPercentage <- reactive({
    filteredData() %>%
      group_by(h_name, year) %>%
      summarise(avgPercent = mean(Value))
  })
  
  # Render the summary table
  output$summaryTable <- renderDataTable({
    filteredData()
  }, options = list(scrollX = TRUE, pageLength = 10))
  
  # Render charts
  theme_set(theme_light(base_size = 25))
  
  output$kpiChart1 <- renderPlotly({
    if(input$QI == "door_to_needle") {
      if(input$patient_description == "gender") {
        df <- filteredData() %>% 
          filter(nameOfAggr == nameOfAggr) %>%
          mutate(year = as.numeric(year), 
                 quarter_num = as.numeric(gsub("Q", "", quarter)),
                 year_quarter = paste(year, quarter, sep = " "))
        
        plot <- ggplot(df, aes(x = year_quarter, y = Value, fill = subGroupVal)) +
          geom_col(position = "dodge", width = 0.7) +
          geom_hline(yintercept = c(25, 50, 75, 100), linetype = "dashed", color = "black") +
          labs(
            title = paste0("Door to Needle Time Across Quarters for ", input$patient_description),
            subtitle = paste0("Percentage of patients achieving \u2264 ", strsplit(input$by_60_45, "_")[[1]][3], " minutes for", input$patient_description),
            x = "Year and Quarter",
            y = paste0("Percentage of DNT \u2264 ", strsplit(input$by_60_45, "_")[[1]][3], " min"),
            caption = "Source: Your Data Source",
            fill = "Gender"
          ) +
          scale_fill_manual(values = color_palette) +
          theme_minimal(base_size = 16) +
          theme(
            plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 10)),
            plot.subtitle = element_text(size = 16, hjust = 0, margin = margin(b = 15)),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.position = "right",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          coord_cartesian(clip = "off")
        
        plotly_chart <- ggplotly(plot, tooltip = "text")
        plotly_chart
        
      } else if(input$patient_description == "stroke_type") {
        df <- filteredData() %>% 
          filter(nameOfAggr == nameOfAggr) %>%
          mutate(year = as.numeric(year), 
                 quarter_num = as.numeric(gsub("Q", "", quarter)),
                 year_quarter = paste(year, quarter, sep = " "))
        
        plot <- ggplot(df, aes(x = year_quarter, y = Value, fill = subGroupVal)) +
          geom_col(position = "dodge", width = 0.7) +
          geom_hline(yintercept = c(25, 50, 75, 100), linetype = "dashed", color = "black") +
          labs(
            title = paste0("Door to Needle Time Across Quarters for ", input$patient_description),
            subtitle = paste0("Percentage of patients achieving \u2264 ", strsplit(input$by_60_45, "_")[[1]][3], " minutes for", input$patient_description),
            x = "Year and Quarter",
            y = paste0("Percentage of DNT \u2264 ", strsplit(input$by_60_45, "_")[[1]][3], " min"),
            caption = "Source: Your Data Source",
            fill = input$patient_description
          ) +
          scale_fill_manual(values = color_palette) +
          theme_minimal(base_size = 16) +
          theme(
            plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 10)),
            plot.subtitle = element_text(size = 16, hjust = 0, margin = margin(b = 15)),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.position = "right",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          coord_cartesian(clip = "off")
        
        plotly_chart <- ggplotly(plot, tooltip = "text")
        plotly_chart
      } else if(input$patient_description == "imaging_done") {
        df <- filteredData() %>% 
          filter(nameOfAggr == nameOfAggr) %>%
          mutate(year = as.numeric(year), 
                 quarter_num = as.numeric(gsub("Q", "", quarter)),
                 year_quarter = paste(year, quarter, sep = " "))
        
        plot <- ggplot(df, aes(x = year_quarter, y = Value, fill = subGroupVal)) +
          geom_col(position = "dodge", width = 0.7) +
          geom_hline(yintercept = c(25, 50, 75, 100), linetype = "dashed", color = "black") +
          labs(
            title = paste0("Door to Needle Time Across Quarters for ", input$patient_description),
            subtitle = paste0("Percentage of patients achieving \u2264 ", strsplit(input$by_60_45, "_")[[1]][3], " minutes for", input$patient_description),
            x = "Year and Quarter",
            y = paste0("Percentage of DNT \u2264 ", strsplit(input$by_60_45, "_")[[1]][3], " min"),
            caption = "Source: Your Data Source",
            fill = input$patient_description
          ) +
          scale_fill_manual(values = color_palette) +
          theme_minimal(base_size = 16) +
          theme(
            plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 10)),
            plot.subtitle = element_text(size = 16, hjust = 0, margin = margin(b = 15)),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.position = "right",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          coord_cartesian(clip = "off")
        
        plotly_chart <- ggplotly(plot, tooltip = "text")
        plotly_chart
      } else if(input$patient_description == "prenotification") {
        df <- filteredData() %>% 
          filter(nameOfAggr == nameOfAggr) %>%
          mutate(year = as.numeric(year), 
                 quarter_num = as.numeric(gsub("Q", "", quarter)),
                 year_quarter = paste(year, quarter, sep = " "))
        
        plot <- ggplot(df, aes(x = year_quarter, y = Value, fill = subGroupVal)) +
          geom_col(position = "dodge", width = 0.7) +
          geom_hline(yintercept = c(25, 50, 75, 100), linetype = "dashed", color = "black") +
          labs(
            title = paste0("Door to Needle Time Across Quarters for ", input$patient_description),
            subtitle = paste0("Percentage of patients achieving \u2264 ", strsplit(input$by_60_45, "_")[[1]][3], " minutes for", input$patient_description),
            x = "Year and Quarter",
            y = paste0("Percentage of DNT \u2264 ", strsplit(input$by_60_45, "_")[[1]][3], " min"),
            caption = "Source: Your Data Source",
            fill = input$patient_description
          ) +
          scale_fill_manual(values = color_palette) +
          theme_minimal(base_size = 16) +
          theme(
            plot.title = element_text(face = "bold", size = 20, hjust = 0, margin = margin(b = 10)),
            plot.subtitle = element_text(size = 16, hjust = 0, margin = margin(b = 15)),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.position = "right",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          coord_cartesian(clip = "off")
        
        plotly_chart <- ggplotly(plot, tooltip = "text")
        plotly_chart
      }
    }
  })
  
  output$kpiChart2 <- renderPlotly({
    if(input$QI == "door_to_needle") {
      if(input$patient_description == "gender") {
        df <- filteredData() %>% 
          filter(nameOfAggr == 'dnt_leq_60') %>%
          mutate(year = as.numeric(year), 
                 quarter_num = as.numeric(gsub("Q", "", quarter)),
                 year_quarter = paste(year, quarter, sep = " "))
        
        plot <- ggplot(df, aes(x = year_quarter, y = Value, fill = subGroupVal)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Percentage of Patients With Door To Needle Time <= 60 Minutes",
               subtitle = "Displayed by Gender",
               x = "Year and Quarter",
               y = "Percentage (%)",
               caption = "Source: Your Data Source",
               fill = "Gender") +
          scale_fill_manual(values = color_palette) +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(plot)
      }
    }
  })
  
  output$kpiChart3 <- renderPlotly({
    if(input$QI == "door_to_needle") {
      if(input$patient_description == "gender") {
        df <- filteredData() %>% 
          filter(nameOfAggr == 'door_to_needle') %>%
          mutate(year = as.numeric(year), 
                 quarter_num = as.numeric(gsub("Q", "", quarter)),
                 year_quarter = paste(year, quarter, sep = " "))
        
        plot <- ggplot(df, aes(x = year_quarter, y = Value, fill = subGroupVal)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Door To Needle Time",
               subtitle = "Displayed by Gender",
               x = "Year and Quarter",
               y = "Value",
               caption = "Source: Your Data Source",
               fill = "Gender") +
          scale_fill_manual(values = color_palette) +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(plot)
      }
    }
  })
  
  output$newChart <- renderPlotly({
    df <- agg_dataNum %>%
      filter(
        QI == "door_to_needle",
        h_name != "all",
        year == input$chartYear,
        nameOfAggr == input$by_60_45,
        !is.na(Value)
      ) %>%
      group_by(h_name) %>%
      summarise(avgPercent = mean(Value))
    
    plot <- ggplot(df, aes(x = reorder(h_name, avgPercent), y = avgPercent, fill = avgPercent)) +
      geom_col() +
      labs(
        title = "Average Percentage of Patients with Door To Needle Time",
        subtitle = paste0("Less Than ", strsplit(input$by_60_45, "_")[[1]][3], " Minutes"),
        x = "Hospital",
        y = "Average Percentage (%)",
        caption = "Source: Your Data Source"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0),
        plot.subtitle = element_text(size = 14, hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_blank()
      ) +
      scale_fill_gradient(low = "#E69F00", high = "#56B4E9") +
      guides(fill = FALSE)
    
    plotly_chart <- ggplotly(plot, tooltip = "text") %>% 
      event_register("plotly_click")
    
    plotly_chart
  })
  
  
  # Update hospital selection based on clicked bar
  observeEvent(event_data('plotly_click'), {
    clicked_bar <- event_data('plotly_click')
    
    if (!is.null(clicked_bar)) {
      chart_data <- agg_dataNum %>%
        filter(
          QI == "door_to_needle",
          h_name != "all",
          year == input$chartYear,
          nameOfAggr == input$by_60_45,
          !is.na(Value)
        ) %>%
        group_by(h_name) %>%
        summarise(avgPercent = mean(Value))
      
      selected_hospital <- chart_data$h_name[clicked_bar$pointNumber + 1]
      updateSelectInput(session, 'hospital', selected = selected_hospital)
    }
  })
  
  
  
  
}

# Run the Shiny app
shinyApp(ui, server)












#Requirements for data visualizations
#Create a dashboard for clinicians.
#Include all KPIs and their values for four variables describing patients or their strokes (subGroup: "prenotification", "imaging_done" "stroke_type", "gender")
#provide a temporal overview of the evolution of selected KPIs in relation to benchmarks. Consider how to visualize:
#  current/past quarter without or incomplete data,
#KPIs (first time) above benchmark, current last quarter averages/median, country average/median,
#understand relationships between variables
#subset analysis: e.g. DNT (door_to_needle time) overall fine, but it is bad for patients with a hemorrhagic (bleeding in the brain) stroke
