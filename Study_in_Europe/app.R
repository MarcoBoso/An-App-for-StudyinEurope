# Libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(readr)
library(leaflet)
library(RColorBrewer)

# Datasets
cost_of_living <- read.csv("./cost_of_living.csv")
programs <- read.csv("./programs.csv")

programs <- programs |> 
  mutate(
    Free_Tuition_EU = ifelse(Free_Tuition_EU == "Yes", "Yes", "No"),
    ECTS = as.numeric(ECTS),
    Link = paste0("<a href='", Link, "' target='_blank'>more information about the program here</a>")
  )

# Reactive value to share selected countries between tabs
selected_countries <- reactiveVal()

bold_if_selected <- function(country, selected_countries) {
  if (country %in% selected_countries) paste0("<b>", country, "</b>") else country
}

# === UI ===
ui <- navbarPage("Study in Europe",
                 tabPanel("Programs",
                          fluidPage(
                            titlePanel(
                              div(
                                h1("Study in Europe", style = "margin-bottom: 0px;"),
                                h4("Explore and compare university programs all around Europe", style = "margin-top: 0px; color: gray;")
                              )
                            ),
                            tags$script(HTML("$(function() { setTimeout(function() { Shiny.setInputValue('show_instructions', true); }, 500); });")),
                            textInput("search", "Search by program title:", width = "100%"),
                            fluidRow(
                              column(3,
                                     selectizeInput("country", "Country:", choices = sort(unique(programs$Country)), selected = NULL, multiple = TRUE),
                                     selectizeInput("level", "Course Level:", choices = sort(unique(programs$Level)), selected = NULL, multiple = TRUE),
                                     selectizeInput("language", "Language:", choices = sort(unique(programs$Language)), selected = NULL, multiple = TRUE),
                                     selectizeInput("tuition", "Free Tuition for EU Students:", choices = c("Yes", "No"), selected = NULL, multiple = TRUE),
                                     sliderInput("ects_range", "ECTS Credits:", min = 0, max = 360, value = c(0, 360))
                              ),
                              column(9, DTOutput("results_table"))
                            ),
                            br(),
                            leafletOutput("map", height = "500px"),
                            br(),
                            fluidRow(
                              column(6, plotlyOutput("bar_ects", height = "400px")),
                              column(6, plotlyOutput("bar_country", height = "400px"))
                            )
                          )
                 ),
                 tabPanel("Cost of Living",
                          fluidPage(
                            tags$script(HTML("$(function() { setTimeout(function() { Shiny.setInputValue('show_instructions', true); }, 500); });")),
                            titlePanel(
                              div(
                                h1("Study in Europe", style = "margin-bottom: 0px;"),
                                h4("See how much the cost of life differs across different countries in Europe", style = "margin-top: 0px; color: gray;")
                              )
                            ),
                            uiOutput("selected_countries_reference"),
                            tabsetPanel(
                              tabPanel("Food",
                                       selectInput("currency_food", "Select currency:", c("Value_EUR", "Value_USD"), "Value_EUR"),
                                       h4("Meal, Inexpensive Restaurant"), plotlyOutput("plot_meal1"),
                                       h4("Meal for 2 People, Mid-range Restaurant"), plotlyOutput("plot_meal2")
                              ),
                              tabPanel("Transportation",
                                       selectInput("currency_transport", "Select currency:", c("Value_EUR", "Value_USD"), "Value_EUR"),
                                       plotlyOutput("plot_transport")
                              ),
                              tabPanel("Utilities",
                                       selectInput("currency_utilities", "Select currency:", c("Value_EUR", "Value_USD"), "Value_EUR"),
                                       plotlyOutput("plot_utilities")
                              ),
                              tabPanel("Fitness",
                                       selectInput("currency_fitness", "Select currency:", c("Value_EUR", "Value_USD"), "Value_EUR"),
                                       plotlyOutput("plot_fitness")
                              ),
                              tabPanel("Rent",
                                       selectInput("currency_rent", "Select currency:", c("Value_EUR", "Value_USD"), "Value_EUR"),
                                       h4("Apartment (1 bedroom) in City Centre"), plotlyOutput("plot_rent1"),
                                       h4("Apartment (1 bedroom) Outside of Centre"), plotlyOutput("plot_rent2")
                              ),
                              tabPanel("Salary",
                                       selectInput("currency_salary", "Select currency:", c("Value_EUR", "Value_USD"), "Value_EUR"),
                                       plotlyOutput("plot_salary")
                              )
                            )
                          )
                 )
)

# === Server ===
server <- function(input, output, session) {
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      title = "Welcome to the app of Study in Europe",
      HTML("<p>How this app work:</p>
            <p>First you will find the <strong>Programs</strong> section, here you can search and filter for different European university programs by country, level, language, tuition, and ECTS credits. You can select up to 10 programs to compare!.</p>
            <p>Then there is the <strong>Cost of Living</strong> section, here you can compare average living expenses (you can choose to see them in EUR or USD) across countries. The countries selected from the Programs tab are highlighted in bold in the differents charts.</p>
            <p>Enjoy the experience!</p>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  filtered_data <- reactive({
    data <- programs
    if (input$search != "") data <- data |>  filter(grepl(input$search, Program_Title, ignore.case = TRUE))
    if (length(input$country) > 0) data <- data |>  filter(Country %in% input$country)
    if (length(input$level) > 0) data <- data |>  filter(Level %in% input$level)
    if (length(input$language) > 0) data <- data |> filter(Language %in% input$language)
    if (length(input$tuition) > 0) data <- data |>  filter(Free_Tuition_EU %in% input$tuition)
    data <- data |> filter(is.na(ECTS) | (ECTS >= input$ects_range[1] & ECTS <= input$ects_range[2]))
    data
  })
  
  output$results_table <- renderDT({
    datatable(
      filtered_data() |>  select(Program_Title, Institution, Level, Language, Country, ECTS, Free_Tuition_EU, Link),
      escape = FALSE,
      selection = list(mode = "multiple", target = "row"),
      options = list(pageLength = 7, dom = 'tip', autoWidth = TRUE),
      rownames = FALSE,
      colnames = c("Program Title", "University", "Level", "Language", "Country", "ECTS", "Free Tuition EU", "Link")
    )
  })
  
  selected_programs <- reactive({
    data <- filtered_data()
    selected_rows <- input$results_table_rows_selected
    if (length(selected_rows) > 10) selected_rows <- selected_rows[1:10]
    if (length(selected_rows) == 0) return(NULL)
    selected <- unique(data[selected_rows, ]$Country)
    selected_countries(selected)
    data[selected_rows, ]
  })
  
  output$selected_countries_reference <- renderUI({
    sel <- selected_countries()
    if (!is.null(sel) && length(sel) > 0) {
      HTML(paste0("<p><strong>Selected countries from the 'Programs' section:</strong> ", paste(sel, collapse = ", "), "</p>"))
    }
  })
  
  output$map <- renderLeaflet({
    data <- selected_programs()
    if (is.null(data) || nrow(data) == 0) return(leaflet() %>% addTiles())
    leaflet(data) |> 
      addTiles() |> 
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0("<strong>", Program_Title, "</strong><br>", Institution, "<br>", Country)
      )
  })
  
  output$bar_ects <- renderPlotly({
    data <- selected_programs()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    plot <- data |> 
      mutate(Program_Label = stringr::str_trunc(Program_Title, 40), ECTS_plot = ifelse(is.na(ECTS), 0, ECTS)) %>%
      ggplot(aes(x = reorder(Program_Label, ECTS_plot), y = ECTS_plot,
                 text = paste0("Program: ", Program_Title, "<br>ECTS: ", ifelse(is.na(ECTS), "NA", ECTS)))) +
      geom_bar(stat = "identity", fill = "#2c7fb8") +
      coord_flip() +
      labs(title = "ECTS per Selected Program", x = "", y = "") +
      theme_minimal(base_size = 14)
    ggplotly(plot, tooltip = "text")
  })
  
  output$bar_country <- renderPlotly({
    data <- selected_programs()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    countries <- unique(data$Country)
    colors <- colorRampPalette(brewer.pal(9, "Blues"))(length(countries))
    data_summary <- data %>% mutate(Country = factor(Country, levels = countries)) %>% count(Country)
    plot <- ggplot(data_summary, aes(x = n, y = reorder(Country, n), fill = Country, text = paste("Programs:", n))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = colors) +
      labs(title = "Number of Selected Programs by Country", x = "", y = "") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    ggplotly(plot, tooltip = "text")
  })
  
  render_cost_plot <- function(label, currency_input, reverse = FALSE) {
    renderPlotly({
      col <- input[[currency_input]]
      selected <- selected_countries()
      data <- cost_of_living |> 
        filter(Label == label) |> 
        group_by(Country) |> 
        summarise(Value = mean(get(col), na.rm = TRUE)) |> 
        arrange(if (reverse) desc(Value) else Value) |> 
        mutate(Rank = row_number())
      
      if (label == "Average Monthly Net Salary (After Tax)") {
        pal <- colorRampPalette(c("red", "yellow", "green"))(nrow(data))
        data$Color <- pal[rank(data$Value)]  # higher values get greener colors
      } else {
        pal <- colorRampPalette(c("green", "yellow", "red"))(nrow(data))
        data$Color <- pal[rank(data$Value)]  # lower values get greener colors
      }
      
      data$CountryLabel <- sapply(data$Country, bold_if_selected, selected_countries = selected)

      
      plot <- ggplot(data, aes(x = Value, y = reorder(CountryLabel, Value), text = paste0("Country: ", Country, "\nValue: ", round(Value, 2), " ", gsub("Value_", "", col)))) +
        geom_bar(stat = "identity", fill = data$Color) +
        scale_fill_gradientn(colors = pal) +
        theme_minimal(base_size = 14) +
        labs(x = "", y = "") +
        theme(legend.position = "none")
      
      ggplotly(plot, tooltip = "text") %>% layout(margin = list(l = 100))
    })
  }
  
  output$plot_meal1 <- render_cost_plot("Meal, Inexpensive Restaurant", "currency_food")
  output$plot_meal2 <- render_cost_plot("Meal for 2 People, Mid-range Restaurant, Three-course", "currency_food")
  output$plot_transport <- render_cost_plot("Monthly Pass (Regular Price)", "currency_transport")
  output$plot_utilities <- render_cost_plot("Basic (Electricity, Heating, Cooling, Water, Garbage) for 85m2 Apartment", "currency_utilities")
  output$plot_fitness <- render_cost_plot("Fitness Club, Monthly Fee for 1 Adult", "currency_fitness")
  output$plot_rent1 <- render_cost_plot("Apartment (1 bedroom) in City Centre", "currency_rent")
  output$plot_rent2 <- render_cost_plot("Apartment (1 bedroom) Outside of Centre", "currency_rent")
  output$plot_salary <- render_cost_plot("Average Monthly Net Salary (After Tax)", "currency_salary", reverse = TRUE)
}

shinyApp(ui = ui, server = server)



