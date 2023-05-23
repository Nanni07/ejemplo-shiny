install.packages("bslib")
library(shiny)
library(bslib)
install.packages('rsconnect')

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty",primary="pink"),
  
      sidebarLayout(
    
            sidebarPanel(
    
                    selectInput("Ind","Variable independiente",choices = names(iris)),
                    selectInput('Dep','variable dependiente',choices = names(iris)),
                    helpText("Seleccione el color del histograma"),
                    radioButtons(input="color","Color",
                    list("Rosa"='pink',"Magenta"='mediumvioletred',"Morado"='purple'),
                                        "pink"),
                  
                     textInput("titulo","Introduzca el titulo","Histograma"),
                   
                        ),
            
            
            mainPanel(
             
              #Graficas
              plotOutput('Hist'),
              plotOutput("BoxPlot"),
              
              # Resumen de datos
              h4("Summary"),
              verbatimTextOutput("summary"),
              
              #Tabla de datos
              h4("Observations"),
              tableOutput("view")
              
              )
            
      )

     )





server <- function(input, output, session) {
  
  data1 <- reactive({
    input$Ind
  })
  data2 <- reactive({
    input$Dep
  })
  
  output$BoxPlot <- renderPlot({
    boxplot(get(data2()) ~ get(data1()) , data=iris,xlab=input$Ind,ylab=input$Dep,
            border = par("fg"), col = "red")
  })
  
  output$Hist <- renderPlot({
    req(data1())
    hist(iris[[data1()]],col=input$color,
         main=input$titulo,xlab=input$Ind,ylab=input$Dep)
  }) 
  #mostrar los tadatos de la tabla
  output$summary <- renderPrint({
  
    summary(iris)
  })
  
  #mostrar la tabla
  output$view <- renderTable({
    head(iris, n = 10)
  })
  
}

shinyApp(ui, server)