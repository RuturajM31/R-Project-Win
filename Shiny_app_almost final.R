#Defining the shiny plot for Casino 

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("FOAvgStake", "LAAvgStake", "FOAvgTotalWinnings", "LAAvgTotalWinnings",
                              "FOTotalActiveDays", "LATotalActiveDays", "Recency_bet_sportsFO", "Recency_bet_sportsLA",
                              "Gender","Continent"), 
                  selected = "FOTotalActiveDays"),
      
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("FOAvgStake", "LAAvgStake", "FOAvgTotalWinnings", "LAAvgTotalWinnings",
                              "FOTotalActiveDays", "LATotalActiveDays", "Recency_bet_sportsFO", "Recency_bet_sportsLA",
                              "Gender","Continent"), 
                  selected = "FOAvgStake"),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Gender", "Continent"),
                  selected = "Gender"),
      
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      #selecting the check box for male and female
      checkboxGroupInput(inputId = "gender_type",
                         label = "Select gender:",
                         choices = c("Male", "Female"),
                         selected = "Male"),
      
      #selecting the check box for continents
      checkboxGroupInput(inputId = "continent_type",
                         label = "Select Continent:",
                         choices = c("Europe","Asia","Oceania","Others","Americas","Africa"),
                         selected = "Europe")

   ),
  
    
    # Outputs
    mainPanel(
      # Tab 1:Sports
      tabsetPanel(type = "tabs",
                  tabPanel("Sports",    
                           plotOutput(outputId = "scatterplot"),
                           plotOutput(outputId = "hist4"),
                           plotOutput(outputId = "hist5"),
                           br()),
      # Tab 2: Sports1
           tabPanel("Sports(StakesVSAge)",    
                 plotOutput(outputId = "plot1"),
                 plotOutput(outputId = "plot3"),
                 br()),
                  
       # Tab 3:Poker
      
                tabPanel("Poker",
                 plotOutput(outputId = "plot"),
                 plotOutput(outputId = "hist3"),
                 br()),
                              
      
      # Tab 4: Casino
          tabPanel("Casino",
                           plotOutput(outputId = "scatterplot1"),
                           plotOutput(outputId = "hist"),
                           br()),
      # Tab 5: Games
         tabPanel("Games",
         plotOutput(outputId = "scatterplot2"),plotOutput(outputId = "hist1"),br()),

    # Tab 6: Supertoto
          tabPanel("Supertoto",
         plotOutput(outputId = "scatterplot3"),plotOutput(outputId = "hist2"),br()),
    
    # Tab 7: Demographics
    tabPanel("Demographics",
             plotOutput(outputId = "demoplot"),plotOutput(outputId = "demoplot1"),br())
    )
  )
)
)


# Define server function required to create the scatterplot
server <- function(input, output) {
  
  
  #Creating a subset for filtering the gender
  Casinosubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Casino %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  Gamessubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Games %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  Supertotosubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Supertoto %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  Sportssubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Sports %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  Pokersubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Poker %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  # Create the plot for casino
  
  output$scatterplot1 <- renderPlot({
    ggplot(Casinosubset(), aes(x=Casino_max_stakes, y=Casino_mean_Winnings)) +  geom_point() 
  })
  
  output$hist <- renderPlot({
    hist(Casino$Casino_LOS,col = "blue")
  })
  
  #create the  plot for Games
  output$scatterplot2 <- renderPlot({
    ggplot(Gamessubset(), aes(x=Games_max_stakes, y=Games_mean_Winnings)) +  geom_point() 
  })
  
  output$hist1 <- renderPlot({
    hist(Games$Games_LOS,col="blue")
  })
  
  #create the  plot for Supertoto
  output$scatterplot3 <- renderPlot({
    ggplot(Supertotosubset(), aes(x=Supertoto_max_stakes, y=Supertoto_mean_Winnings)) +  geom_point() 
  })
  
  output$hist2 <- renderPlot({
    hist(Supertoto$Supertoto_LOS,col="blue")
  })
  
  
  #Create a line plot for poker
  output$plot <- renderPlot({
    ggplot(data = Pokersubset(), aes(x = Profit_Poker)) + geom_line(aes(y = Poker_Sell,colour = "Poker_Sell")) +
              geom_line(aes(y = Poker_Buy,colour = "Poker_Buy"))
      
  })
  
  output$hist3 <- renderPlot({
    hist(Poker$Recency_Poker,col="blue")
  })
  
  
  #Create plot for Sports
  output$scatterplot <- renderPlot({
    ggplot(data = Sportssubset(), aes_string(x = input$x, y = input$y,
                                    color = input$z)) +
      geom_point(alpha=input$alpha) + facet_grid(cols = vars(Continent)) + theme_bw()
  })
  
  output$hist4 <- renderPlot({
    hist(Sports$Recency_bet_sportsFO,col="blue")
  })
  
  output$hist5 <- renderPlot({
    hist(Sports$Recency_bet_sportsLA,col="blue")
  })
  
  #Create plot for Sports(StakeVSAge)
  output$plot1 <- renderPlot({
    ggplot(data = Sportssubset(), aes(x = AGE)) + geom_line(aes(y = FOAvgStake,colour = "FOAvgStake" )) +
      geom_line(aes(y = FOAvgTotalWinnings,colour = "FOAvgTotalWinnings" ))
    
  })
  output$plot3 <- renderPlot({
    ggplot(data = Sportssubset(), aes(x = AGE)) + geom_line(aes(y = LAAvgStake,colour = "LAAvgStake" )) +
      geom_line(aes(y = LAAvgTotalWinnings,colour = "LAAvgTotalWinnings" ))
    
  })
  
  #Create a demographic plot
    output$demoplot <- renderPlot({
    barplot(table(Demographic$Continent),col="blue")
  })
    
    output$demoplot1 <- renderPlot({
      barplot(table(Demographic$Gender),col="blue")
    })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)