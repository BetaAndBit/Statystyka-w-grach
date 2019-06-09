library(shiny)

rzut_kostkami = function(liczba_kostek = 1)
  sample(1:6, size = liczba_kostek, replace = TRUE)


ui <- fluidPage(
   titlePanel("Wojny czołgów"),

    sidebarLayout(
      sidebarPanel(
        sliderInput("atak_1",
                    "czołg_1 atak:",
                    min = 1,
                    max = 10,
                    value = 5),
        sliderInput("obrona_1",
                    "czołg_1 obrona:",
                    min = 1,
                    max = 10,
                    value = 2),
        sliderInput("atak_2",
                    "czołg_2 atak:",
                    min = 1,
                    max = 10,
                    value = 6),
        sliderInput("obrona_2",
                    "czołg_2 obrona:",
                    min = 1,
                    max = 10,
                    value = 2)
      ),
      
      mainPanel(
        plotOutput("wynik_1"),
        plotOutput("wynik_2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$wynik_1 <- renderPlot({

     rozgrywki_1 = replicate(10000, {
       wynik_ataku = rzut_kostkami(input$atak_1) 
       liczba_trafien = sum(wynik_ataku > 3)
       
       wynik_obrony = rzut_kostkami(input$obrona_2)
       liczba_obron = sum(wynik_obrony > 3)
       
       if (liczba_trafien > liczba_obron) {
         trafień = liczba_trafien - liczba_obron
       } else {
         trafień = 0
       }
       
       trafień
     })
     
      barplot(table(rozgrywki_1), col = 'darkgray', border = 'white', main = "Czołg 1 trafień")
   })
   
   output$wynik_2 <- renderPlot({
     
     rozgrywki_2 = replicate(10000, {
       wynik_ataku = rzut_kostkami(input$atak_2) 
       liczba_trafien = sum(wynik_ataku > 3)
       
       wynik_obrony = rzut_kostkami(input$obrona_1)
       liczba_obron = sum(wynik_obrony > 3)
       
       if (liczba_trafien > liczba_obron) {
         trafień = liczba_trafien - liczba_obron
       } else {
         trafień = 0
       }
       
       trafień
     })
     
     barplot(table(rozgrywki_2), col = 'darkgray', border = 'white', main = "Czołg 2 trafień")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

