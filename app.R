
library(dplyr)
library(tidyr)
library(forcats)
library(scales)
library(shiny)
library(purrr)
library(DT)
library(GenSA)

NYRAVaryingTidySummaryFull <- read.csv("bayesian_combined_Cleaned.csv") %>% select(-1)

fixed <- NYRAVaryingTidySummaryFull %>% 
  filter(ran_val == "fixed") %>% 
  select(-group, -level, -ran_val, -mean, -lower, -upper) %>%
  rename(estimate_fixed = median, std.error_fixed = sd)

random <- NYRAVaryingTidySummaryFull %>% 
  filter(ran_val == "random") %>% 
  select(-mean, -lower, -upper, -ran_val) %>%
  rename(estimate_random = median, std.error_random = sd)

effects <- left_join(fixed, random, by = "term")

shinyTable <- effects %>% 
  mutate(estimate_full = estimate_fixed + estimate_random,
         std.error_full = sqrt(std.error_fixed^2 + std.error_random^2)) %>% 
  filter(term != "more_than_one_favorite") %>%
  filter(term != "(Intercept)") %>% 
  na.omit() %>%
  group_by(level) %>%
  mutate(total_perc_effect = (estimate_full / std.error_full) / sum(abs(estimate_full) / std.error_full, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  mutate(term = fct_recode(term,
                           "Purse" = "purse_usa",
                           "Number of Runners 4" = "number_of_runners_4",
                           "Number of Runners 5" = "number_of_runners_5",
                           "Number of Runners 6" = "number_of_runners_6",
                           "Number of Runners 7" = "number_of_runners_7",
                           "Number of Runners 8" = "number_of_runners_8",
                           "Number of Runners 9" = "number_of_runners_9",
                           "Number of Runners 10" = "number_of_runners_10",
                           "Number of Runners 11" = "number_of_runners_11",
                           "Age 2" = "age_restriction_02",
                           "Age 3" = "age_restriction_03",
                           "Age 3U" = "age_restriction_3U",
                           "Sex Restriction: Fillies and Mares" = "sex_restriction_2",
                           "Sex Restriction: Fillies" = "sex_restriction_3",
                           "Surface Dirt Instead of Turf" = "surface_D",
                           "Favorites Odds" = "favorite_odds",
                           "NY Bred" = "all_NY",
                           "Race Type: Allowance" = "race_type_Allowance",
                           "Race Type: Claiming" = "race_type_Claiming",
                           "Race Type: Maiden Claiming" = "race_type_Maiden_Claiming",
                           "Race Type: Maiden Special Weight" = "race_type_Maiden_Special_Weight"
  )) %>%
  separate(level, c("track_id", "day_of_week", "race_number"), sep = ":") %>%
  mutate(race_number = as.integer(race_number))

get_effect <- function(term, track_id, day_of_week, race_number) {
  effect <- shinyTable %>%
    filter(term == !!term, track_id == !!track_id, day_of_week == !!day_of_week, race_number == !!race_number) %>%
    pull(total_perc_effect)
  return(ifelse(length(effect) == 0, 0, effect))
}

calculate_effect_matrix <- function(race_data, track_id, day_of_week) {
  num_races <- nrow(race_data)
  effect_matrix <- matrix(0, nrow = num_races, ncol = num_races)
  
  for (race_num in 1:num_races) {
    for (order in 1:num_races) {
      effect_matrix[race_num, order] <- sum(
        round(get_effect(paste0("Number of Runners ", race_data$FieldSize[race_num]), track_id, day_of_week, order), 2),
        round(race_data$Purse[race_num] * get_effect("Purse", track_id, day_of_week, order) / 100, 2),
        round(get_effect(paste0("Race Type: ", race_data$RaceType[race_num]), track_id, day_of_week, order), 2),
        round(get_effect(paste0("Age ", race_data$Age[race_num]), track_id, day_of_week, order), 2),
        round(race_data$Distance[race_num] * get_effect("Race Distance", track_id, day_of_week, order) / 100, 2),
        round(ifelse(race_data$Surface[race_num] == "Dirt", get_effect("Surface Dirt Instead of Turf", track_id, day_of_week, order), 0), 2),
        round(get_effect(paste0("Sex Restriction: ", race_data$SexRestriction[race_num]), track_id, day_of_week, order), 2),
        round(ifelse(race_data$NYBred[race_num] == "Yes", get_effect("NY Bred", track_id, day_of_week, order), 2), 2)
      )
    }
  }
  
  return(effect_matrix)
}

optimize_race_order <- function(effect_matrix) {
  num_races <- nrow(effect_matrix)
  all_orders <- gtools::permutations(num_races, num_races, 1:num_races)
  max_effect <- -Inf
  best_order <- NULL

  all_combinations <- list()

  for (i in 1:nrow(all_orders)) {
    order <- all_orders[i, ]
    total_effect <- sum(effect_matrix[cbind(1:num_races, order)])
    all_combinations[[i]] <- list(order = order, total_effect = total_effect)

    if (total_effect > max_effect) {
      max_effect <- total_effect
      best_order <- order
    }
  }

  all_combinations <- all_combinations[order(sapply(all_combinations, function(x) x$total_effect), decreasing = TRUE)]
  top_combinations <- all_combinations[1:min(1000, length(all_combinations))]
  return(list(best_order = best_order, max_effect = max_effect, all_combinations = top_combinations))
}

# optimize_race_order <- function(effect_matrix, num_combinations = 1000) { 
#   num_races <- nrow(effect_matrix)
#   sa_result <- GenSA( 
#     lower = rep(1, num_races), 
#     upper = rep(num_races, num_races), 
#     fn = function(order) -sum(effect_matrix[cbind(1:num_races, order)]), 
#                       control = list(max.call = 10000) 
#   ) 
#   best_order <- sa_result$par 
#   max_effect <- -sa_result$value 
#   all_combinations <- replicate(num_combinations, { 
#     order <- sample(1:num_races) 
#     total_effect <- sum(effect_matrix[cbind(1:num_races, order)]) 
#     list(order = order, total_effect = total_effect) 
#   }, 
#   simplify = FALSE) 
#   all_combinations <- all_combinations[order(sapply(all_combinations, function(x) x$total_effect), decreasing = TRUE)] 
#   return(list(best_order = best_order, max_effect = max_effect, all_combinations = all_combinations)) 
#   }

# Shiny UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  titlePanel("NYRA Race Order Optimization (Multi Bets Considered)"),
  tags$div(
    style = "position: absolute; right: 10px; top: 10px; width: 600px; padding: 10px; background-color: #f9f9f9; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
    tags$p("This app provides the optimal race order by using NYRA's race information. It will provide a recommendation about the race order for the provided races in order to maximize handle. Select a Track, Day of the Week, and input the race information for each race in order to see how the race order changes and their corresponding total percent effect."),
    tags$p("All categorical variables are measured relative to the base level. The base levels are:"),
    tags$ul(
      tags$li("Number of Runners: 12"),
      tags$li("Age Restriction: 4U"),
      tags$li("Sex Restriction: Open"),
      tags$li("Race Type: Stake")
    )
  ),
  tags$div(
    style = "width: 600px; padding: 10px; background-color: #f9f9f9; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
    tags$p("Each combination of Track ID and Day of the Week has a different maximum amount input based from the database. The corresponding information is listed below:"),
    tableOutput("raceTable")
  ),
  
  selectInput("track_id", "Choose a Track ID:",
              choices = unique(shinyTable$track_id)),
  
  selectInput("day_of_week", "Choose Day of the Week:",
              choices = unique(shinyTable$day_of_week)),
  
  h3("Enter race details:"),
  
  fluidRow(
    column(1, p("Race")),
    column(1, p("Field Size")),
    column(1, p("Purse")),
    column(2, p("Race Type")),
    column(1, p("Age (4 and 4U are Combined Together)")),
    column(1, p("Distance (Equibase Unit)")),
    column(1, p("Surface")),
    column(1, p("Sex Restriction")),
    column(1, p("NY Bred"))
  ),
  
  map(1:12, ~ fluidRow(
    column(1, textOutput(paste0("race_", .x))),
    column(1, numericInput(paste0("field_size_", .x), label = NULL, value = 0, min = 4, max = 12, step = 1)),
    column(1, numericInput(paste0("purse_", .x), label = NULL, value = 0)),
    column(2, selectInput(paste0("race_type_", .x), label = NULL, choices = c("Allowance" = "Allowance", "Claiming" = "Claiming", "Maiden Claiming" = "Maiden Claiming", "Maiden Special Weight" = "Maiden Special Weight", "Stakes" = "Stakes", "Other" = "Other"))),
    column(1, selectInput(paste0("age_", .x), label = NULL, choices = c("2", "3", "3U", "4U"))),
    column(1, numericInput(paste0("distance_", .x), label = NULL, value = 0)),
    column(1, selectInput(paste0("surface_", .x), label = NULL, choices = c("Dirt", "Turf"))),
    column(1, selectInput(paste0("sex_restriction_", .x), label = NULL, choices = c("Open", "Fillies", "Fillies and Mares", "Colts and Geldings"))),
    column(1, selectInput(paste0("ny_bred_", .x), label = NULL, choices = c("No", "Yes")))
  )),
  
  actionButton("submit", "Optimize Race Order"),
  
  
  
  
  # h3("Effect Matrix:"),
  # tableOutput("effect_matrix"),
  
  
  
  
  h3("Top 1000 Combinations:"),
  dataTableOutput("all_combinations_table"),
  
  
  
  
  
  h3("Suggested Race Order:"),
  dataTableOutput("optimized_table")
  # HTML('<style>.rChart {width: 100%; height: 500px}</style>')
)






# Shiny Server
server <- function(input, output, session) {
  output$raceTable <- renderTable({
    data.frame(
      DayOfWeek = c("Wed", "Thu", "Fri", "Sat", "Sun"),
      AQU = as.integer(c(9, 9, 10, 10, 10)),
      BEL = as.integer(c(9, 9, 9, 12, 10)),
      BAQ = as.integer(c(NA, 9, 9, 6, 6)),
      SAR = as.integer(c(10, 10, 11, 12, 11))
    )
  })
  
  race_details <- reactive({
    map_dfr(1:12, function(i) {
      data.frame(
        Race = LETTERS[i],
        FieldSize = input[[paste0("field_size_", i)]],
        Purse = input[[paste0("purse_", i)]],
        RaceType = input[[paste0("race_type_", i)]],
        Age = input[[paste0("age_", i)]],
        Distance = input[[paste0("distance_", i)]],
        Surface = input[[paste0("surface_", i)]],
        SexRestriction = input[[paste0("sex_restriction_", i)]],
        NYBred = input[[paste0("ny_bred_", i)]],
        RaceNumber = i
      )
    }) %>% filter(FieldSize != 0)
  })
  
  for (i in 1:12) {
    local({
      j <- i
      output[[paste0("race_", j)]] <- renderText({
        LETTERS[j]
      })
    })
  }
  
  observeEvent(input$submit, {
    race_data <- race_details()
    
    if (nrow(race_data) > 0) {
      effect_matrix <- calculate_effect_matrix(race_data, input$track_id, input$day_of_week)
      
      output$effect_matrix <- renderTable({
        as.data.frame(effect_matrix) %>% 
          mutate_all(~ round(., 2))
      })
      
      optimized_result <- optimize_race_order(effect_matrix)
      best_order <- optimized_result$best_order
      max_effect <- optimized_result$max_effect
      
      best_order_data <- race_data[best_order, ]
      best_order_data <- best_order_data %>% 
        mutate(Order = 1:nrow(best_order_data),
               Total_Effect = round(effect_matrix[cbind(1:nrow(effect_matrix), best_order)], 2)) 
      
      all_combinations_data <- data.frame(
        Order = sapply(optimized_result$all_combinations, function(x) paste(LETTERS[x$order], collapse = " -> ")),
        Total_Effect = sapply(optimized_result$all_combinations, function(x) round(x$total_effect, 2))
      )
      
      output$all_combinations_table <- renderDataTable({
        datatable(all_combinations_data, options = list(
          pageLength = 12,
          columnDefs = list(
            list(width = '150px', targets = "_all")
          ),
          autoWidth = TRUE
        ), colnames = c("Order", "Total Effect")) %>%
          formatStyle(
            'Total_Effect',
            backgroundColor = styleInterval(c(-1, 1), c('red', 'white', 'green'))
          ) %>%
          formatStyle(
            'Total_Effect', 
            background = styleColorBar(range(as.numeric(all_combinations_data$Total_Effect)), 'lightblue'), 
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      })
      
      output$optimized_table <- renderDataTable({
        datatable(best_order_data[, c("Order", "Race", "Total_Effect")], options = list(
          pageLength = 12,
          columnDefs = list(
            list(width = '150px', targets = "_all")
          ),
          autoWidth = TRUE
        ), colnames = c("Order", "Race", "Total Effect")) %>% 
          formatStyle(
            'Total_Effect',
            backgroundColor = styleInterval(c(-1, 1), c('red', 'white', 'green'))
          ) %>%
          formatStyle(
            'Total_Effect', 
            background = styleColorBar(range(as.numeric(best_order_data$Total_Effect)), 'lightblue'), 
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      })
    } else {
      output$effect_matrix <- renderTable({
        as.data.frame(matrix(ncol = 0, nrow = 0))
      })
      output$all_combinations_table <- renderDataTable({
        datatable(data.frame(), options = list(
          pageLength = 12,
          columnDefs = list(
            list(width = '150px', targets = "_all")
          ),
          autoWidth = TRUE
        ), colnames = c("Order", "Total Effect"))
      })
      output$optimized_table <- renderDataTable({
        datatable(data.frame(), options = list(
          pageLength = 12,
          columnDefs = list(
            list(width = '150px', targets = "_all")
          ),
          autoWidth = TRUE
        ), colnames = c("Order", "Race", "Total Effect"))
      })
    }
  })
}

shinyApp(ui, server)



