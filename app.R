
# Personality Assessment Shiny App with Integrated IRT Pipeline
# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(mirt)

# Debugging flag - set to TRUE to auto-fill questions for testing
DEBUG_MODE <- TRUE  # Set to FALSE for production

# Your actual factor structure
factor_items <- list(
  Bold = c("DARING", "TOUGH", "CUNNING", "BOLD", "RUGGED","AMBITIOUS"),
  Dominant = c("FORCEFUL", "DOMINANT", "DEMANDING","AGGRESSIVE", "STRONG_WILLED"),
  Conscientious = c("THOROUGH", "EFFICIENT", "SYSTEMATIC", "ORGANIZED"),
  Traditional = c("TRADITIONAL","CONSERVATIVE","STRAIT_LACED","OLD_FASHIONED","CONFORMING"),
  Curious = c("CREATIVE", "INQUISITIVE", "INNOVATIVE","IMAGINATIVE","ARTISTIC","CURIOUS"),
  Expressive = c("TALKATIVE", "QUIET", "CHATTY"),
  Emotional = c("ANXIOUS", "FEARFUL", "FRETFUL", "CALM", "HIGH_STRUNG","RELAXED"),
  Warm = c("COMPASSIONATE", "SYMPATHETIC", "TENDER_MINDED", "GENEROUS", "CONSIDERATE"),
  Openness = c("INTELLECTUAL", "INQUISITIVE", "INNOVATIVE","IMAGINATIVE","ARTISTIC","COMPLEX", "CREATIVE"),
  Dynamism = c("DOMINANT", "HEADSTRONG", "STRONG_WILLED", "DARING", "TOUGH", "CUNNING", "BOLD"),
  Stabilizing = c("COMPASSIONATE", "SYMPATHETIC", "WARM", "SENSITIVE", "SOFT_HEARTED", "CONSIDERATE")
)


# Create unique items list from your factors
all_items <- unique(unlist(factor_items, use.names = FALSE))

# Randomize the order of items (different each time)
all_items <- sample(all_items)

score_new_data <- function(irt_results, new_data, method = "EAP", return_se = TRUE) {
  models <- irt_results$models
  factor_items <- irt_results$factor_items
  
  stopifnot(is.data.frame(new_data))
  all_items <- unlist(factor_items, use.names = FALSE)
  missing_items <- setdiff(all_items, colnames(new_data))
  if (length(missing_items) > 0) {
    stop("New data missing items: ", paste(missing_items, collapse = ", "))
  }
  
  n_rows <- nrow(new_data)
  results <- data.frame(row_id = 1:n_rows)
  
  for (factor_name in names(models)) {
    mod <- models[[factor_name]]
    if (is.null(mod)) {
      cat("Skipping", factor_name, "- model failed to fit\n")
      next
    }
    
    items <- factor_items[[factor_name]]
    
    factor_data <- new_data[, items, drop = FALSE]
    
    factor_data[] <- lapply(factor_data, function(x) {
      if (is.factor(x)) x <- as.numeric(as.character(x))
      if (is.character(x)) x <- as.numeric(x)
      as.integer(round(x))
    })
    
    tryCatch({
      scores <- fscores(mod, response.pattern = factor_data, method = method,
                        full.scores.SE = return_se, verbose = FALSE)
      
      if (is.matrix(scores)) {
        results[[factor_name]] <- scores[, 1]
        
        if (return_se && ncol(scores) > 1) {
          results[[paste0(factor_name, "_SE")]] <- scores[, ncol(scores)]
        }
      } else {
        results[[factor_name]] <- as.numeric(scores)
      }
      
    }, error = function(e) {
      warning("Failed to score factor ", factor_name, ": ", e$message)
      results[[factor_name]] <<- rep(NA, n_rows)
      if (return_se) {
        results[[paste0(factor_name, "_SE")]] <<- rep(NA, n_rows)
      }
    })
  }
  
  results$row_id <- NULL
  return(results)
}

theta_to_percentile <- function(theta) {
  # Convert theta (normally distributed, mean=0, sd=1) to percentiles
  # Theta scores typically range from about -3 to +3
  percentile <- pnorm(theta) * 100
  return(round(percentile, 1))
}

# Function to create score summary with percentiles only
create_score_summary <- function(scores, score_type = "Subfactor") {
  if (is.null(scores) || length(scores) == 0) {
    return(data.frame(
      Factor = paste("Complete the assessment first"),
      Percentile = "",
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    Factor = names(scores),
    Percentile = paste0(theta_to_percentile(unlist(scores)), "%"),
    stringsAsFactors = FALSE
  )
}

# Superhero data with scores in percentiles (1-99%)
superhero_data <- data.frame(
  name = c(
    # Extreme High Stabilizing
    "Superman", "Captain America", "Professor X", "Jean Grey",
    
    # High Stabilizing
    "Wonder Woman", "Spider-Man", "Nightcrawler", "Storm",
    
    # Moderate-High Stabilizing
    "Daredevil", "Black Panther", "Captain Marvel", "Cyclops",
    
    # Moderate Stabilizing  
    "Batman", "Flash", "Green Lantern", "Aquaman",
    
    # Moderate-Low Stabilizing
    "Iron Man", "Hawkeye", "Black Widow", "Thor",
    
    # Low Stabilizing
    "Wolverine", "Doctor Strange", "Ant-Man", "Gambit",
    
    # Very Low Stabilizing (Anti-heroes)
    "Deadpool", "Hulk", "Punisher", "Ghost Rider",
    
    # Extremely Low Stabilizing (Villains)
    "Joker", "Carnage", "Sabretooth", "Apocalypse",
    "Green Goblin", "Harley Quinn", "Venom", "Red Hulk"
  ),
  
  # Stabilizing percentiles (1-99%)
  Stabilizing = c(
    # Extreme high (95-99th percentile)
    99, 96, 94, 92,
    
    # High (85-90th percentile)
    89, 87, 85, 83,
    
    # Moderate-high (70-80th percentile)
    78, 76, 74, 72,
    
    # Moderate (50-65th percentile)
    63, 61, 59, 57,
    
    # Moderate-low (35-45th percentile)
    43, 41, 39, 37,
    
    # Low (20-30th percentile)
    28, 26, 24, 22,
    
    # Very low (5-15th percentile)
    14, 12, 10, 8,
    
    # Extremely low (1-3rd percentile)
    6, 4, 3, 2,
    2, 1, 1, 1
  ),
  
  # Dynamism percentiles (1-99%)
  Dynamism = c(
    # Moderate-high for high Stabilizing characters
    85, 88, 62, 58,
    
    # High dynamism heroes
    82, 78, 75, 80,
    
    # Moderate-high dynamism
    72, 78, 85, 70,
    
    # Moderate dynamism
    68, 82, 75, 65,
    
    # Variable for tech/strategic types
    88, 60, 68, 88,
    
    # Lower for more cerebral types
    55, 48, 52, 45,
    
    # Extreme high for chaotic characters
    98, 95, 92, 90,
    
    # Extreme range for villains
    99, 96, 85, 22,
    88, 94, 80, 3
  ),
  
  # Openness percentiles (1-99%)
  Openness = c(
    # Traditional heroes - moderate openness
    68, 55, 95, 82,
    
    # Moderate-high openness
    75, 88, 72, 78,
    
    # Variable openness
    85, 72, 70, 65,
    
    # High for strategic minds
    92, 78, 80, 68,
    
    # Very high for innovators
    98, 62, 75, 58,
    
    # Lower for traditional fighters
    52, 98, 48, 60,
    
    # Extreme for chaos/creativity
    90, 42, 35, 88,
    
    # Extreme range for villains
    99, 28, 15, 85,
    80, 88, 32, 1
  ),
  
  # Character type for color coding
  type = c(
    rep("Hero", 24), 
    rep("Anti-Hero", 4), 
    rep("Villain", 8)
  )
)

# Global variable to store IRT models
irt_models <- readRDS("irt_models.rds")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Personality Assessment & Superhero Mapping"),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Introduction", tabName = "setup", icon = icon("info-circle")),
                menuItem("Assessment", tabName = "assessment", icon = icon("clipboard-list")),
                menuItem("Results", tabName = "results", icon = icon("chart-line")),
                menuItem("3D Visualization", tabName = "viz3d", icon = icon("cube"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Setup Tab
      tabItem(tabName = "setup",
              fluidRow(
                box(
                  title = "Welcome to the Personality Assessment", status = "primary", solidHeader = TRUE,
                  width = 12,
                  h4("About This Assessment"),
                  p("This assessment will measure your personality across multiple dimensions. As a fun wrinkle, it will also show you how you compare to popular fictional comic characters (as rated by Claude)."),
                  br(),
                  p("These measures are based on the methods from my dissertation applied to a dataset of ~345 respondents from the Eugene-Springfield sample. These methods use a psycholexical basis for a structure of personality. Measures were refined using IRT analyses."),
                  h5("The Three Main Dimensions:"),
                  HTML("<ul>
              <li><strong>Stabilizing:</strong> Your ability to manage emotions and maintain harmonious relationships</li>
              <li><strong>Dynamism:</strong> Your level of energy, assertiveness, and boldness</li>
              <li><strong>Openness:</strong> Your willingness to try new ideas and embrace innovation vs. tradition</li>
            </ul>"),
                  br(),
                  p("Press Start to jump right in, or, keep reading to learn about the sub-factors!"),
                  div(style = "text-align: center;",
                      actionButton("start_assessment", "Start Assessment →", class = "btn-primary btn-lg")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Factor Structure", status = "info", solidHeader = TRUE,
                  width = 12,
                  h4("More Factors"),
                  p("In addition to Openness, Stabilzing, and Dynamism, we also measure 8 sub-factors comprising those three main factors. Their descriptions follow."),
                  br(),
                  h5("The 8 Lower Dimensions:"),
                  HTML("<ul>
              <li><strong>Boldness:</strong> Your willingness to take calculated risks and pursue challenging goals with confidence and determination</li>
              <li><strong>Dominant:</strong> Your tendency to take charge in situations, assert your preferences, and influence others to follow your lead</li>
              <li><strong>Traditional:</strong> Your preference for established customs, conventional approaches, and maintaining social norms over radical change</li>
              <li><strong>Conscientious:</strong> Your commitment to completing tasks thoroughly, maintaining order, and approaching work with methodical precision</li>
              <li><strong>Curious:</strong> Your drive to explore new ideas, engage with creative pursuits, and seek out novel experiences and knowledge</li>
              <li><strong>Expressive:</strong> Your inclination to engage others in conversation and share your thoughts and feelings openly</li>
              <li><strong>Emotional:</strong> Your tendency to experience intense feelings, worry about outcomes, and feel overwhelmed by stress or uncertainty</li>
              <li><strong>Warm:</strong> Your natural inclination to care for others' wellbeing, show empathy, and offer support to those in need</li>
            </ul>"),
                  br()
                )
              )
      ),
      
      # Assessment Tab
      tabItem(tabName = "assessment",
              fluidRow(
                box(
                  title = "Personality Assessment", status = "primary", solidHeader = TRUE,
                  width = 12, height = "700px",
                  div(style = "height: 600px; overflow-y: auto;",
                      h4("Please rate each trait on a scale from 1 (Extremely Inaccurate) to 8 (Extremely Accurate):"),
                      br(),
                      lapply(1:length(all_items), function(i) {
                        item <- all_items[i]
                        div(style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;",
                            h5(paste0(i, ". ", gsub("_", " ", item))),
                            radioButtons(
                              inputId = paste0("item_", item),
                              label = NULL,
                              choices = setNames(1:8, c("1 - Extremely Inaccurate", "2", "3", "4", "5", "6", "7", "8 - Extremely Accurate")),
                              selected = if(DEBUG_MODE) sample(1:8, 1) else character(0),
                              inline = TRUE
                            )
                        )
                      })
                  ),
                  div(style = "text-align: center; margin-top: 20px; padding: 20px;",
                      actionButton("calculate_scores", "Calculate My Personality Profile", 
                                   class = "btn-primary btn-lg"),
                      br(), br(),
                      # Conditional next button
                      conditionalPanel(
                        condition = "input.calculate_scores > 0",
                        actionButton("go_to_results", "View My Results →", 
                                     class = "btn-success btn-lg")
                      )
                  )
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                box(
                  title = "Your Main Factor Scores", status = "success", solidHeader = TRUE,
                  width = 6,
                  tableOutput("main_factor_table")
                ),
                box(
                  title = "Your Subfactor Scores", status = "info", solidHeader = TRUE,
                  width = 6,
                  tableOutput("subfactor_table")
                )
              ),
              fluidRow(
                box(
                  title = "Character Comparison", status = "warning", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("superhero_comparison")
                )
              ),
              fluidRow(
                box(
                  title = "", status = "primary", solidHeader = FALSE,
                  width = 12,
                  div(style = "text-align: center; padding: 10px;",
                      actionButton("go_to_3d", "Explore 3D Visualization →", 
                                   class = "btn-primary btn-lg")
                  )
                )
              )
      ),
      
      # 3D Visualization Tab
      tabItem(tabName = "viz3d",
              fluidRow(
                box(
                  title = "3D Personality Space", status = "primary", solidHeader = TRUE,
                  width = 12, height = "700px",
                  plotlyOutput("plot3d", height = "600px")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store scores
  user_scores <- reactiveValues()
  
  # Navigation buttons
  observeEvent(input$start_assessment, {
    updateTabItems(session, "sidebar", selected = "assessment")
  })
  
  observeEvent(input$go_to_results, {
    updateTabItems(session, "sidebar", selected = "results")
  })
  
  observeEvent(input$go_to_3d, {
    updateTabItems(session, "sidebar", selected = "viz3d")
  })
  
  # Calculate scores when button is clicked
  observeEvent(input$calculate_scores, {
    if (is.null(irt_models)) {
      showNotification("IRT models not loaded. Please check your setup.", type = "message")
      return()
    }
    
    # Check if all items are answered (skip check in debug mode)
    responses <- sapply(all_items, function(item) {
      input[[paste0("item_", item)]]
    })
    
    if (!DEBUG_MODE && any(sapply(responses, is.null))) {
      showNotification("Please answer all questions before calculating scores.", type = "message")
      return()
    }
    
    # Convert to numeric and handle debug mode
    if (DEBUG_MODE) {
      # Fill any missing responses with random values for testing
      responses <- sapply(responses, function(x) if(is.null(x)) sample(1:8, 1) else x)
    }
    responses <- as.numeric(responses)
    names(responses) <- all_items
    new_data <- data.frame(t(responses))
    
    # Calculate IRT theta scores using your fitted models
    theta_scores <- score_new_data(irt_models, new_data, method = "EAP", return_se = FALSE)
    
    # Convert theta scores to lists for easier handling
    subfactor_scores <- as.list(theta_scores)
    
    # Calculate main factor scores - these are direct IRT factors, not aggregated
    main_scores <- list(
      Openness = subfactor_scores$Openness,
      Dynamism = subfactor_scores$Dynamism, 
      Stabilizing = subfactor_scores$Stabilizing
    )
    
    # Remove the 3 main factors from subfactor_scores to show only the 8 subfactors
    subfactor_only <- subfactor_scores
    subfactor_only$Openness <- NULL
    subfactor_only$Dynamism <- NULL
    subfactor_only$Stabilizing <- NULL
    
    # Store scores
    user_scores$subfactor_scores <- subfactor_scores
    user_scores$main_scores <- main_scores
    user_scores$Stabilizing <- main_scores$Stabilizing
    user_scores$Dynamism <- main_scores$Dynamism
    user_scores$Openness <- main_scores$Openness
    
    # Switch to results tab automatically
    updateTabItems(session, "sidebar", selected = "results")
    
    showNotification("Personality profile calculated successfully!", type = "message")
  })
  
  # Main factor score table with percentiles only
  output$main_factor_table <- renderTable({
    create_score_summary(user_scores$main_scores, "Main Factor")
  })
  
  # Subfactor score table with percentiles only
  output$subfactor_table <- renderTable({
    create_score_summary(user_scores$subfactor_scores, "Subfactor")
  })
  
  # Character comparison table
  output$superhero_comparison <- DT::renderDataTable({
    if (is.null(user_scores$Stabilizing)) {
      data.frame(Message = "Complete the assessment to see character comparisons")
    } else {
      # Convert user theta scores to percentiles
      user_percentiles <- list(
        Stabilizing = theta_to_percentile(user_scores$Stabilizing),
        Dynamism = theta_to_percentile(user_scores$Dynamism),
        Openness = theta_to_percentile(user_scores$Openness)
      )
      
      superhero_distances <- superhero_data %>%
        mutate(
          Distance = round(sqrt(
            (Stabilizing - user_percentiles$Stabilizing)^2 +
              (Dynamism - user_percentiles$Dynamism)^2 +
              (Openness - user_percentiles$Openness)^2
          )
          ),1) %>%
        arrange(Distance) %>%
        select(name, type, Stabilizing, Dynamism, Openness, Distance) %>%
        rename(
          Character = name,
          Type = type,
          "Stabilizing %" = Stabilizing,
          "Dynamism %" = Dynamism,
          "Openness %" = Openness,
          "Distance from You" = Distance
        )
      
      superhero_distances
    }
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # 3D Plot
  output$plot3d <- renderPlotly({
    if (is.null(user_scores$Stabilizing)) {
      plot_ly() %>%
        add_text(x = 50, y = 50, z = 50, text = "Complete the assessment to see your 3D position",
                 textfont = list(size = 16)) %>%
        layout(
          scene = list(
            xaxis = list(title = "Stabilizing (%)", range = c(0, 100)),
            yaxis = list(title = "Dynamism (%)", range = c(0, 100)),
            zaxis = list(title = "Openness (%)", range = c(0, 100))
          )
        )
    } else {
      # Convert user theta scores to percentiles for plotting
      user_percentiles <- list(
        Stabilizing = theta_to_percentile(user_scores$Stabilizing),
        Dynamism = theta_to_percentile(user_scores$Dynamism),
        Openness = theta_to_percentile(user_scores$Openness)
      )
      
      p <- plot_ly() %>%
        # Add character points with labels
        add_markers(
          data = superhero_data,
          x = ~Stabilizing, y = ~Dynamism, z = ~Openness,
          color = ~type,
          colors = c("Hero" = "blue", "Anti-Hero" = "orange", "Villain" = "red"),
          size = I(16),
          text = ~paste("Character:", name, "(", type, ")",
                        "<br>Stabilizing:", paste0(Stabilizing, "%"),
                        "<br>Dynamism:", paste0(Dynamism, "%"),
                        "<br>Openness:", paste0(Openness, "%")),
          hovertemplate = "%{text}<extra></extra>",
          name = ~type
        ) %>%
        add_text(
          data = superhero_data,
          x = ~Stabilizing, y = ~Dynamism, z = ~Openness,
          text = ~name,
          textfont = list(size = 8, color = "darkblue"),
          showlegend = FALSE,
          hoverinfo = "none"
        ) %>%
        # Add user point with label
        add_markers(
          x = user_percentiles$Stabilizing,
          y = user_percentiles$Dynamism,
          z = user_percentiles$Openness,
          color = I("black"),
          size = I(15),
          symbol = I("x"),
          text = paste("You<br>Percentiles:",
                       "<br>Stabilizing:", paste0(user_percentiles$Stabilizing, "%"),
                       "<br>Dynamism:", paste0(user_percentiles$Dynamism, "%"),
                       "<br>Openness:", paste0(user_percentiles$Openness, "%")),
          hovertemplate = "%{text}<extra></extra>",
          name = "You"
        ) %>%
        add_text(
          x = user_percentiles$Stabilizing,
          y = user_percentiles$Dynamism,
          z = user_percentiles$Openness,
          text = "YOU",
          textfont = list(size = 12, color = "black"),
          showlegend = FALSE,
          hoverinfo = "none"
        ) %>%
        layout(
          scene = list(
            xaxis = list(title = "Stabilizing (%)", range = c(0, 100)),
            yaxis = list(title = "Dynamism (%)", range = c(0, 100)),
            zaxis = list(title = "Openness (%)", range = c(0, 100)),
            camera = list(
              eye = list(x = 1.5, y = 1.5, z = 1.5)
            )
          ),
          title = "Your Personality in 3D Space with Fictional Characters"
        )
      
      p
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)


