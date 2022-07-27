PokemonMain <- read.csv("C:/Users/bblue/Downloads/tabbed-pokemon-app-main/tabbed-pokemon-app-main/pokemon.csv")
hearthstone <- read.csv("C:/Users/bblue/OneDrive/Documents/MachineLearning/hearthstone/hearthstone.csv")
Pokemon <- PokemonMain[-c(800),-c(1:5,12:13)]

ui<- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "Jonathan, Sean, and Yuhao's Charts",
    tabPanel("Legendary Pokemon - J",
             sidebarPanel(
               tags$h3("Choose a Stat"),
               selectInput("legstat1", label="Pokemon Stat", choices =colnames(Pokemon), selected="HP"),
               
              ),#create a list of choices based on pokemon types )
             sidebarPanel(
               tags$h3("Choose a Stat"),
               selectInput("legstat2", label="Pokemon Stat", choices =colnames(Pokemon), selected="Attack"),#create a list of choices based on pokemon types )
               sliderInput(inputId = "limit",
                           label = "Limit:",
                           min = 1,
                           max = max(PokemonMain$HP),
                           value = 1)
              ),
             mainPanel(
               h1("Stat chart"),
               #verbatimTextOutput("txtout")
               plotOutput(outputId = "legendaryPokemon")
             )    
    ),
    tabPanel("All Pokemon - J",
             sidebarPanel(
               tags$h3("Choose a Stat"),
               selectInput("stat1", label="Pokemon Stat", choices =colnames(Pokemon), selected="HP")#create a list of choices based on pokemon types )
             ),
             sidebarPanel(
               tags$h3("Choose a Stat"),
               selectInput("stat2", label="Pokemon Stat", choices =colnames(Pokemon), selected="Attack")#create a list of choices based on pokemon types )
             ),
             mainPanel(
               h1("Stat chart"),
               #verbatimTextOutput("txtout")
               plotOutput(outputId = "allPokemon")
             )    
    
    ),
    tabPanel("Pokemon type2 - S",
             sidebarPanel(
               tags$h3("select a type"),
               selectInput("type2", label="Pokemon type2", choices =rownames(table(PokemonMain$Type.2)), selected="Poison")
               
             ),
             mainPanel(
               h1("Attack chart"),
               plotOutput(outputId = "pokemonType2")
             )
             
    ),
    tabPanel("Pokemon generation - S",
             sidebarPanel(
               tags$h3("select a generation"),
               selectInput("generation", label="Pokemon generation", choices =rownames(table(PokemonMain$Generation)), selected="1")
               
             ),
             mainPanel(
               h1("HP chart"),
               plotOutput(outputId = "pokemon_generation")
             )
             
    ),
    tabPanel("Mage class card - Y",
             sidebarPanel(
               
               tags$h3("Input:"),
               textInput("txt1","Card Name","")
             ),
             sidebarPanel(
               tags$h3("Input:"),
               textInput("txt1","Card Cost","")
             ),
             sidebarPanel(
               tags$h3("Choose a class"),
               selectInput("class", label="Class of card", choices =rownames(table(hearthstone$playerClass)), selected="Mage")#create a list of choices based on pokemon types )
             ),
             
             mainPanel(
               h1("health Chart"),
               #verbatimTextOutput("txtout")
               plotOutput(outputId = "Mageclass")
             )    
    ),
    tabPanel("Priest class card - Y",
             sidebarPanel(
               tags$h3("Input:"),
               textInput("txt1","Card Name","")
             ),
             mainPanel(
               h1("health Chart"),
               #verbatimTextOutput("txtout")
               plotOutput(outputId = "Priestclass")
             )    
    ),
    tabPanel("All Cards - Y",
             sidebarPanel(
               tags$h3("Choose a class"),
               selectInput("class", label="Class of card", choices =rownames(table(hearthstone$playerClass)), selected="Mage")#create a list of choices based on pokemon types )
             ),
             mainPanel(
               h1("health Chart"),
               #verbatimTextOutput("txtout")
               plotOutput(outputId = "allcard")
             )    
    )
  )          
)

    
           

server<- function(input, output){
  PokemonMain %>% filter(PokemonMain$Legendary=="True")->legendary_pokemon
  
  output$allPokemon<- renderPlot({
    Pokemon ->all_pokemon
    ggplot(data=all_pokemon, aes_string(x=input$stat1, y=input$stat2))+ geom_point(color="orange")
  })
  
  output$legendaryPokemon<- renderPlot({


    ggplot(data=legendary_pokemon, aes_string(x=input$legstat1, y=input$legstat2), aes(label= Name))+ geom_point(color="purple")+ theme_classic()+geom_label_repel(data = subset(legendary_pokemon, get(input$legstat1) < input$limit), aes(label = Name))

    })
  
  output$txtout <- renderText({
    paste(input$txt1)
  })
  
  output$pokemonType2 <- renderPlot({
    PokemonMain %>% filter(PokemonMain$Type.2==input$type2) ->pokemon_type2
    ggplot(data=pokemon_type2, aes(x=Attack, y=Defense)) + geom_point(color="purple")
  })
  
  output$pokemon_generation <- renderPlot({
    PokemonMain %>% filter(PokemonMain$Generation==input$generation) ->pokemonGeneration
    ggplot(data=pokemonGeneration, aes(x=HP, y=Speed)) + geom_point(color="green")
  })
  
  hearthstone %>% filter(hearthstone$playerClass=="Mage")->Mage_class
  hearthstone %>% filter(hearthstone$playerClass=="Priest")->Priest_class
  
  
  output$Priestclass <- renderPlot({
    ggplot(data = Priest_class, aes(x=attack,y=cost))+geom_point(color="yellow")
  })
  
  output$Mageclass <- renderPlot({
    ggplot(data = Mage_class, aes(x=attack,y=cost))+geom_point(color="purple")
  })
  
  output$allcard<- renderPlot({
    hearthstone %>% filter(hearthstone$playerClass==input$class)->all_cards
    ggplot(data=all_cards, aes(x=attack, y=cost))+ geom_point(color="blue")
  })
  
  output$txtout <- renderText({
    paste(input$txt1)
  })
  
  
}

shinyApp(ui = ui, server = server)


runExample("01_hello")
