library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinythemes)

df <- read.csv("JoinedDF.csv")

ui <- fluidPage(
    theme = shinytheme("darkly"),
    navbarPage("Seattle Tree Canopy Coverage Analysis",
        tabPanel("Introduction",
            fluidPage(
                tags$div(class = "jumbotron text-center",
                    tags$h2("Exploring Seattle's Urban Greenery"),
                    tags$p("Discover how tree canopy coverage is linked to socio-economic factors across Seattle.")
                ),
                tags$p("Trees and urban greenery play a crucial role in enhancing the quality of life in urban areas. They provide numerous environmental benefits, including air quality improvement, carbon sequestration, and temperature regulation. In a city like Seattle, known for its natural beauty and green spaces, understanding the distribution and factors affecting tree canopy coverage is essential."),
                tags$p("This Shiny application delves into the intricate relationship between urban greenery and socio-economic aspects of Seattle's neighborhoods. Our analysis aims to unravel the dynamics of urban tree coverage, exploring how it varies across different areas and what factors might contribute to these variations."),
                tags$p("Using comprehensive data on Seattle's tree canopy, we investigate patterns and trends in canopy coverage. We explore questions such as:"),
                tags$ul(
                    tags$li("How has the tree canopy coverage in Seattle's neighborhoods changed over time?"),
                    tags$li("Is there a connection between tree canopy coverage and neighborhood socio-economic factors, such as average income or population demographics?"),
                    tags$li("Which neighborhoods in Seattle boast the best tree coverage, and which ones lag behind?")
                ),
                tags$p("Navigate through the tabs to explore interactive visualizations and analyses, and uncover the story behind Seattle's urban canopy. Whether you're a city planner, environmental enthusiast, or a curious resident, this application offers valuable insights into the green landscape of Seattle.")
            )
        ),
        tabPanel("Income vs. Tree Canopy",
            fluidPage(
                tags$h3("Per Capita Income vs. Tree Canopy Coverage: 2021"),
                tags$p("Select a neighborhood to focus on specific data or choose 'All' to view city-wide trends."),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("neighborhood", 
                                    "Choose Neighborhood", 
                                    choices = c("All", unique(df$GEN_ALIAS)))
                    ),
                    mainPanel(
                        plotlyOutput("income_canopy_plot"),
                        tags$p("Points in the scatter plot represent individual neighborhoods. The plot demonstrates the relationship between per capita income and tree canopy coverage percentage in 2021. Hover over points for more details.")
                    )
                )
            )
        ),
        tabPanel("Socio-economic Correlation",
            fluidPage(
                tags$h3("Percentage of People of Color vs. Tree Canopy Coverage"),
                tags$p("This plot shows the correlation between the percentage of people of color in a neighborhood and tree canopy coverage."),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("neighborhoodPage2", "Choose Neighborhood", 
                                    choices = c("All", unique(df$GEN_ALIAS)))
                    ),
                    mainPanel(
                        plotlyOutput("socioeconomic_plot"),
                        tags$p("Explore how the demographic composition relates to environmental factors in different neighborhoods.")
                    )
                )
            )
        ),
        tabPanel("Neighborhood Comparison",
            fluidPage(
                tags$h3("Comparative Analysis of Neighborhoods"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("comparisonFactor", "Select Factor for Comparison",
                                    choices = list("Tree Canopy 2021 (%)" = "TreeCanopy_2021_Percent",
                                                   "Tree Canopy 2016 (%)" = "TreeCanopy_2016_Percent",
                                                   "Per Capita Income" = "Per_Capita_Income",
                                                   "Total Population" = "Total_Population"
                                    ))
                    ),
                    mainPanel(
                        plotlyOutput("neighborhood_plot"),
                        tags$p("This plot compares different neighborhoods based on the selected factor."),
                        tags$hr(),
                        tags$div(
                            tags$h4("Analysis Wrap-Up"),
                            tags$p("The development of tree canopy coverage across Seattle's neighborhoods shows a diverse pattern. Some areas have experienced significant growth in canopy coverage, likely due to effective urban greening policies and community initiatives. In contrast, other neighborhoods, particularly those with higher urban density, have seen a decline, underscoring the challenges of maintaining green spaces in rapidly developing areas."),
                            tags$p("A key insight is the relationship between tree canopy coverage and average neighborhood income. Generally, neighborhoods with higher per capita income tend to have more extensive tree coverage. This correlation might reflect socio-economic disparities in access to green spaces, a critical factor for urban planners and policymakers to address."),
                            tags$p("Neighborhoods with the best tree coverage often boast large parks and natural reserves, while those with the least coverage are typically more urbanized with limited green space. This contrast highlights the importance of equitable urban planning that ensures all neighborhoods, regardless of economic status, have access to sufficient green spaces."),
                            tags$p("Overall, the analysis suggests a need for targeted efforts to enhance tree canopy coverage in under-greened neighborhoods. It also emphasizes the role of socio-economic factors in shaping the urban landscape, providing valuable insights for future urban development and environmental policies.")
                        )
                    )
                )
            )
        )
    )
)


# Server setup
server <- function(input, output) {
  output$income_canopy_plot <- renderPlotly({
    # Filter data based on user input
    filtered_data <- if (input$neighborhood != "All") {
      subset(df, GEN_ALIAS == input$neighborhood)
    } else {
      df
    }
    
    plot <- ggplot(filtered_data, aes(x = Per_Capita_Income, y = TreeCanopy_2021_Percent)) +
      geom_point(aes(color = GEN_ALIAS, text = paste("Neighborhood:", GEN_ALIAS, "<br>Per Capita Income:", Per_Capita_Income, "<br>2021 Canopy %:", TreeCanopy_2021_Percent)), alpha = 0.6) +
      labs(title = "Per Capita Income vs. Tree Canopy Coverage (2021)", x = "Per Capita Income", y = "2021 Canopy Coverage (%)") 
    
  })
  
  output$socioeconomic_plot <- renderPlotly({
    # Filter data based on user input for neighborhood
    filtered_data <- if (input$neighborhoodPage2 != "All") {
      subset(df, GEN_ALIAS == input$neighborhoodPage2)
    } else {
      df
    }
    
    # Check if the filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(NULL)  
    }
    
    plot <- ggplot(filtered_data, aes(x = PCT_PEOPLE_OF_COLOR, y = TreeCanopy_2021_Percent)) +
      geom_point(aes(color = GEN_ALIAS), alpha = 0.6) +
      labs(title = "Percentage of People of Color vs. Tree Canopy Coverage", 
           x = "Percentage of People of Color", 
           y = "2021 Canopy Coverage (%)")
  })
  
  output$neighborhood_plot <- renderPlotly({
    # Dynamically select the column based on input
    selected_factor <- input$comparisonFactor
    
    df_reordered <- df %>%
      mutate(Reordered_Neighborhood = reorder(GEN_ALIAS, .data[[selected_factor]]))
    
    plot <- ggplot(df_reordered, aes(x = Reordered_Neighborhood, y = .data[[selected_factor]], fill = GEN_ALIAS)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      labs(title = paste("Neighborhood Comparison by", selected_factor), 
           x = "Neighborhood", 
           y = selected_factor) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
