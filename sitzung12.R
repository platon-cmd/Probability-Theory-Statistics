install.packages("shiny")
#mercenery enrollement ! 
library(shiny)
library(igraph)
library(ggraph)
library(ggplot2)
library(DT)



# daten laden 
florence_URL <- "https://raw.githubusercontent.com/kosukeimai/qss/master/DISCOVERY/florentine.csv"

florence <- read.csv(florence_URL, row.names = 1, check.names = FALSE)

florence <- as.matrix(florence)

florence_net <- graph_from_adjacency_matrix(florence, mode = "undirected", diag = FALSE)

degree(florence_net)


florence_net <- delete_vertices(florence_net, which(degree(florence_net) == 0))

ui <- fluidPage(
  tags$span("Hallo Seminar!", style = "color = firebrick; font-size = 24px;")
)

server <- function(input, output) {}

shinyApp(ui, server)


ui <- fluidPage(
  titlePanel("Florentiner Familien"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("measure", "Zentralitätsmaß auswählen:", 
                  choices = c("Degree", "Closeness", "Betweenness"), 
                  selected = "Degree")
    ),
    
    mainPanel(
      plotOutput("networkPlot", height = "500px")
    )
  )
)

server <- function(input, output) {
  centrality_fun <- reactive({
    switch (input$measure,
      "Degree" = degree,
      "Closeness" = closeness, 
      "Betweenness" = betweenness)})
  centrality_valus <- reactive({centrality_fun()(florence_net)})
  
  output$networkPlot <- renderPlot({
    
    node_color <- switch (input$measure,
                          "Degree" = "steelblue",
                          "Closeness" = "darkred",
                          "Betweenness" = "darkgreen"
    )
    
    ggraph(florence_net, layout = "fr") + 
      geom_edge_link(alpha = 0.4) + 
      geom_node_point(aes(size = centrality_valus()), color = node_color) + 
      geom_node_text(aes(label = name), repel = TRUE) + 
      theme_graph() + 
      labs(size = paste(input$measure, "Zentralität"))
    
  })}

shinyApp(ui = ui, server = server)
