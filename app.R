# Load R packages
library(shiny)
library(shinythemes)
library(dplyr)
library(tidytuesdayR)
library(DT)
library(readr)
library(ggplot2)

# Load dataset
prizes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv')
length(unique(prizes$degree_institution))
write.csv(prizes, "prizes.csv", row.names = FALSE)
prizes <- read_csv("prizes.csv")   # change path/name if needed

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  navbarPage(
    "Prize Explorer",
    
    # -------- TAB 1: DATA TABLE --------
    tabPanel(
      "Data table",
      sidebarLayout(
        sidebarPanel(
          tags$h3("Filters"),
          selectInput(
            "year_filter", "Prize year:",
            choices = c("All", sort(unique(prizes$prize_year))),
            selected = "All"
          ),
          selectInput(
            "gender_filter", "Gender:",
            choices = c("All", sort(unique(prizes$gender))),
            selected = "All"
          ),
          selectInput(
            "genre_filter", "Genre:",
            choices = c("All", sort(unique(prizes$prize_genre))),
            selected = "All"
          )
        ),
        mainPanel(
          h2("Data Table"),
          DTOutput("prize_table")
        )
      )
    ),
    
    # -------- TAB 2: GENDER --------
    tabPanel(
      "Gender",
      fluidRow(
        column(
          12,
          h3("Is the prize dominated by one gender?"),
          p("The prize does not appear to be dominated by one gender. 
         Men and women have similar representation in the dataset, 
         while non-binary and transgender authors are extremely underrepresented."),
          br(),
          plotOutput("plot_gender"),
          br(),
          DTOutput("table_gender")
        )
      )
    ),
    
    # -------- TAB 3: GENRE --------
    tabPanel(
      "Genre",
      fluidRow(
        column(
          12,
          h3("Which genres dominate the prize awards?"),
          p("Fiction overwhelmingly dominates prize awards, 
            far surpassing all other genres. Non-fiction, crime, 
            and poetry are moderately represented, while biography, 
            children's literature, drama, and SFF appear only rarely."),
          br(),
          plotOutput("plot_genre"),
          br(),
          DTOutput("table_genre")
        )
      )
    ),
    
    # -------- TAB 4: HIGHEST DEGREE --------
    tabPanel(
      "Highest degree",
      fluidRow(
        column(
          12,
          h3("What academic backgrounds do most prize recipients have?"),
          p("Most prize recipients have a bachelor’s degree, 
            followed by substantial numbers with master’s and doctoral degrees. 
            Several degree types appear very rarely, and a significant portion 
            of educational backgrounds are unreported. 
            Overall, prize winners tend to have higher levels of education."),
          br(),
          plotOutput("plot_degree"),
          br(),
          DTOutput("table_degree")
        )
      )
    ),
    
    # -------- TAB 5: GENDER vs GENRE (heatmap) --------
    tabPanel(
      "Gender vs Genre",
      fluidRow(
        column(
          12,
          h3("Which gender–genre combinations are the most common among prize recipients?"),
          p("Fiction and non-fiction dominate for both men and women, 
            while non-binary and transman authors are minimally represented 
            and concentrated in only a few genres."),
          br(),
          plotOutput("heat_gender_genre"),
          br(),
          DTOutput("table_gender_genre")
        )
      )
    ),
    
    # -------- TAB 6: HIGHEST DEGREE vs GENRE (heatmap) --------
    tabPanel(
      "Highest degree vs Genre",
      fluidRow(
        column(
          12,
          h3("Do some genres attract authors with higher degrees?"),
          p("Fiction and non-fiction attract the largest number of authors 
            with advanced degrees, while Master’s and Doctorate holders appear 
            across many genres. Rare degree categories contribute very little 
            across genres, and a substantial amount of educational data is unreported."),
          br(),
          plotOutput("heat_degree_genre"),
          br(),
          DTOutput("table_degree_genre")
        )
      )
    ),
    
    # -------- TAB 7: GENDER vs HIGHEST DEGREE (heatmap) --------
    tabPanel(
      "Gender vs Highest degree",
      fluidRow(
        column(
          12,
          h3("Do men and women differ in their highest degree attainment within the prize population?"),
          p("Men and women show similar educational patterns, with most holding bachelor’s, 
            master’s, or doctoral degrees; other gender identities and degree categories 
            are rare in the dataset."),
          br(),
          plotOutput("heat_gender_degree"),
          br(),
          DTOutput("table_gender_degree")
        )
      )
    )
  )
)

# ----------------- SERVER -----------------
server <- function(input, output, session) {
  
  # ---- Reactive data filtered by sidebar controls (used in ALL tabs) ----
  filtered_data <- reactive({
    df <- prizes
    
    if (input$year_filter != "All") {
      df <- df %>% filter(prize_year == input$year_filter)
    }
    if (input$gender_filter != "All") {
      df <- df %>% filter(gender == input$gender_filter)
    }
    if (input$genre_filter != "All") {
      df <- df %>% filter(prize_genre == input$genre_filter)
    }
    df
  })
  
  # ---- TAB 1: Data table ----
  output$prize_table <- renderDT({
    datatable(
      filtered_data(),
      filter  = "top",
      options = list(
        pageLength = 25,
        autoWidth  = TRUE,
        scrollX    = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ---- TAB 2: Gender ----
  output$plot_gender <- renderPlot({
    ggplot(filtered_data(), aes(x = gender)) +
      geom_bar() +
      labs(x = "Gender", y = "Count")
  })
  
  output$table_gender <- renderDT({
    filtered_data() %>%
      count(gender, name = "n") %>%
      arrange(desc(n)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---- TAB 3: Genre ----
  output$plot_genre <- renderPlot({
    ggplot(filtered_data(), aes(x = prize_genre)) +
      geom_bar() +
      labs(x = "Genre", y = "Count")
  })
  
  output$table_genre <- renderDT({
    filtered_data() %>%
      count(prize_genre, name = "n") %>%
      arrange(desc(n)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---- TAB 4: Highest degree ----
  output$plot_degree <- renderPlot({
    ggplot(filtered_data(), aes(x = highest_degree)) +
      geom_bar() +
      labs(x = "Highest degree", y = "Count")
  })
  
  output$table_degree <- renderDT({
    filtered_data() %>%
      count(highest_degree, name = "n") %>%
      arrange(desc(n)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---- TAB 5: Gender vs Genre (heatmap) ----
  output$heat_gender_genre <- renderPlot({
    df_tab <- filtered_data() %>%
      count(gender, prize_genre, name = "n")
    
    ggplot(df_tab, aes(x = prize_genre, y = gender, fill = n)) +
      geom_tile() +
      labs(x = "Genre", y = "Gender", fill = "Count")
  })
  
  output$table_gender_genre <- renderDT({
    filtered_data() %>%
      count(gender, prize_genre, name = "n") %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---- TAB 6: Highest degree vs Genre (heatmap) ----
  output$heat_degree_genre <- renderPlot({
    df_tab <- filtered_data() %>%
      count(highest_degree, prize_genre, name = "n")
    
    ggplot(df_tab, aes(x = prize_genre, y = highest_degree, fill = n)) +
      geom_tile() +
      labs(x = "Genre", y = "Highest degree", fill = "Count")
  })
  
  output$table_degree_genre <- renderDT({
    filtered_data() %>%
      count(highest_degree, prize_genre, name = "n") %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---- TAB 7: Gender vs Highest degree (heatmap) ----
  output$heat_gender_degree <- renderPlot({
    df_tab <- filtered_data() %>%
      count(gender, highest_degree, name = "n")
    
    ggplot(df_tab, aes(x = highest_degree, y = gender, fill = n)) +
      geom_tile() +
      labs(x = "Highest degree", y = "Gender", fill = "Count")
  })
  
  output$table_gender_degree <- renderDT({
    filtered_data() %>%
      count(gender, highest_degree, name = "n") %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
}

# ----------------- Run app -----------------
shinyApp(ui = ui, server = server)