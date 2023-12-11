library(plotly)
library(shiny)
library(ggplot2)
library(dplyr)
library(fmsb)
library(shinythemes)

finaldf <- read.csv("df.csv")

wealth_over_time <- ggplot(finaldf, aes(x = Year, y = earnings....million., color = Year)) + geom_line() +
  labs(x = "Year", y = "Earnings in Millions", color = "Year")


Home <- fluidPage(fluidPage(theme = shinytheme("superhero"),
  mainPanel(
    h1("Sports And Wealth"),
    p("Our goal with this project is to showcase the relationship 
      between the difficulties of sports and wealth earned from these sports."),
  p("Following our goal we want to show people who do not play sports at this high of a level
    how tough it is to do these sports and often times how much you get rewarded for it."),
  p("Some sports can be exteremly dangeours and have life altering injuries. To compensate for these
    risk, athletes are paid an extraordinary amount of money, money that most people can not fathom."),
  imageOutput("image"),
  p("Created by: Nate Valdez, Arjan Sethi, and Justin Hing"),
  p('R Packages: plotly, shiny, ggplot2, dplyr, fmsb, and shiny themes '),
  uiOutput("url")
 #uiOutput
  
    )
)
)

SportsTime <- fluidPage(
          
  
  tabPanel(
    title = "Sports and Wealth Over Time"
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("graph",  width = 20,
                 h1("Wealth Over Time"),
                 p("In the line plot you can see the trends over the years and how much money is made
        through the last three decades.As you bring your cursor up and down through the lines 
        it will show you how much some athletes have made in millions. You can use the popularity
        of sports lineplot to correlate which years are the most popular and how wealth 
        works with that. The table is where the information
                 from the lineplot is from."),
                 plotlyOutput("lineplot")),
        tabPanel("table", tableOutput(outputId = "table")),
        
      )
    ),
   
   
     #mainPanel(
       # width = 20,
       #p("In the line plot you can see the trends over the years and how much money is made
       
  mainPanel(
    tabsetPanel(
      tabPanel("graph",width = 20,
               h1("Popularity of Sports Over Time"),
               p("As you can see in this line plot it shows data on what sports people have played throughout the years, with
          this lineplot it allows you to see which sport people are consistently playing. There are some 
          outliers in this data. Such as Martial Arts only being played in 2018. While a sport such as Basketball
          has people consistenly playing from 1990 to 2020. The table is where the information
                 from the lineplot is from."),
               plotlyOutput("line")),
      tabPanel("table", tableOutput(outputId = "tableline")),
    )
  ),
      # mainPanel(
        # width = 20,
       

    
  )
  
WealthVsSports <- fluidPage (
  
  tabPanel(
    title = "Which Sport is Easiest to Make the Most Money? "
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("graph", width = 20,
               h1 ("Sports vs Wealth"),
               p("In this plot you can see the trends of how sports correlate with wealth
                 and which sport ends up paying the most based on average.As we can see through this plot, it is clear that Boxing extends beyond any other sport in regards to wealth. 
                 Other notable appearances we can take away from is that baseball/softball, ice hockey, and martial arts make the least amount of money."),
               plotlyOutput("barplot"))
    )
  ))
ui <- fluidPage(
titlePanel("Correlation between Sport Difficulty and Sport Salaries"),
sidebarLayout(
  sidebarPanel(
    selectInput(
      inputId = "year",
      label = "Select a year",
      choices = 1990:2020
    )
  ),
  mainPanel(
    plotOutput(outputId = "correlation_plot")
  )
),
p("As we can see in these plots showing the correlation between ranking of athlete's yearly salaries and sport difficulties,") ,
 p ("there is somewhat of a correlation, with many of these athletes playing sports that are difficult according to the rankings."),
p ( "While the sports most of these top-earning athletes play are considered to be hard, sometimes racers and golfers show up and"), 
 p   ("skew the difficulty, meaning that other factors such as sport popularity are likely influencing these salaries as well.")
)


ui <- navbarPage(
  "Final Project",
  tabPanel("Home", Home),
  tabPanel("Sports and Wealth Over Time", SportsTime),
  tabPanel("Which Sport is Easiest to Make the Most Money?", WealthVsSports),
  tabPanel("Correlation between Sport Difficulty and Sport Salaries", ui)
)



server <- function(input, output){

  url <- a ("Highest Paid Athletes and Toughest Sports By Skill", href = "https://docs.google.com/document/d/158S5HVhCIMWCw97nO1Zx9xCffFgsykfq01wH4txwxkc/edit?usp=sharing")
  output$url <- renderUI({
    tagList("Sources:", url)
  })
  
  
  output$image <- renderImage({
    list(src = "www/imageforinfo.jpeg",
         width = "50%",
         height = 400)
  }, deleteFile = FALSE)
    output$table <- renderTable({
      data_pt <- select(finaldf, c(Year, earnings....million.))
    })
    
    output$tableline <- renderTable({
      data_pt <- select(finaldf, c(Sport, Year))
    })
  
  output$lineplot <- renderPlotly({
    wealth_over_time <- ggplot(finaldf, aes(x = Year, y = earnings....million., color = Year)) + geom_line() +
      labs(x = "Year", y = "Earnings in Millions", color = "Year")
    
    return(wealth_over_time)
  })
    
  output$line <- renderPlotly({
     pop_sports <- ggplot(finaldf, aes(x = Year, y = Sport, color = Sport)) + geom_line() +
      labs(x = "Year", y = "Sport", color = "Sport")
    return(pop_sports)
  })
  
  output$source <- renderUI({
  })
  output$table <- renderTable({
    data <- select(finaldf, c(Sport, earnings....million.))
  })
  
  output$barplot <- renderPlotly({
    wealthandsports <- ggplot(finaldf, aes( x = Sport, y = earnings....million., color = Sport)) + geom_line() +
      labs(x = "Sport", y = "Earnings in Millions", color = "Sport")
    return(wealthandsports)
  })
  
  output$correlation_plot <- renderPlot({
    correlation_df <- filter(finaldf, Year == input$year)
    p <- ggplot(correlation_df, aes(x = reorder(Current.Rank, desc(Current.Rank)), y = reorder(RANK, desc(RANK)), color = Sport)) +
      geom_point() +
      geom_label(
        label=correlation_df$Name,
      ) +
      labs(x = "Rank of Athlete Salary for Year", y = "Rank of Sport Difficulty")
    return(p)
  })
  
  
}



  
  

  
shinyApp(ui = ui, server = server)
