# inspirations
# https://riptutorial.com/shiny/example/32449/uploading-csv-files-to-shiny
# https://shiny.rstudio.com/gallery/ncaa-swim-team-finder.html


library(shiny)
library(shinyWidgets)
library(ggplot2)

ggtheme = theme_bw() +
  theme(
    plot.title = element_text(
      size = 12,
      face = "bold",
      margin = margin(2, 2, 2, 2)
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(0, 0, 0, 0)
    ),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y.right = element_text(angle = 90),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12, color = "black"),
    axis.line = element_line(size = 0.25, color = "black"),
    axis.ticks = element_line(size = 0.25, color = "black"),
    strip.text = element_text(size = 12, margin = margin(1, 1, 2, 2)),
    strip.background = element_blank(),
    legend.margin = margin(2, 2, 2, 2),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(0.4, "cm"),
    legend.background = element_rect(colour = "transparent", fill = ggplot2::alpha("white", 0)),
    legend.position = "none"
  )


distributions = c("Normal", "Beta", "Gamma")

alpha = 0.5

dataA = NULL

ui <- fluidPage(#titlePanel("doseR | Alpha release"),
  
  navbarPage(
    "doseR | Alpha release",
    
    tabPanel(
      "Home",
      fluid = TRUE,
      
      h2("Welcome to doseR"),
      p(
        "This app allows you to explore explore, fit and assess dose-response models to better understand and quantify host-pathogen interactions.", 
        br(),
        "doseR is part of the Pyterverse."
      ),
      p(
        "Authors: Amandine Gamble, Philip Lee, Dylan H. Morris, James O. Lloyd-Smith", 
        br(),  
        "Affiliation: University of California Los Angeles"
      ),
      p(
        "Questions and comments: ",
        a("amandine.gamble@gmail.com", href = "mailto:aamandine.gamble@gmail.com")
      ), 
      p(
        "Code: ",
        a("github.com/LloydSmithLab/doseR", href = "https://github.com/LloydSmithLab/doseR")
      ), 
      
      h2("Time-dependent dose-response model"), 
      #withMathJax("$$\\text{Display formula in heading }X_n=X_{n-1}-\\varepsilon$$")
      img(src='doseR_model.png', height="50%", width="50%", align = "center"), 
      p(
        "References: ", 
        br(),
        "- Buchholz D. W.*, Gamble A.*, Morris D. H., Sahler J., Jager M., Martins M., Monreal I. A., Imbiakha B., Borremans B., Snedden C. E., Kushner N. L., Diel D., Van de Walle G., August A., Lloyd-Smith J. O. and Aguilar H. C. Viral exposure dose predicts incubation period: an illustration with SARS-CoV-2 in mice. In preparation. (* Equal contribution)", 
        br(),  
        "- Gamble A.*, Buchholz D. W.*, Imbiakha B., Sahler J., Morris D. H., Jager M., Martins M., Kushner N. L., Diel D., Van de Walle G., August A., Aguilar H. C. and Lloyd-Smith J. O. Faster replication rate is associated with shorter incubation period in infection with the Delta lineage of SARS-CoV-2. In preparation. (* Equal contribution)"
      )
    ),
    
    
    tabPanel(
      "Data simulation",
      fluid = TRUE,
      
      sidebarLayout(
        sidebarPanel(
          strong("Pick a distribution, its parameters, and a plot below"),
          br(),
          br(),
          
          # inputs
          selectizeInput(
            "disA",
            "Distribution",
            choices = distributions,
            selected = "Normal",
            multiple = FALSE
          ),
          sliderInput(
            "parA1",
            "Parameter 1",
            min = 0,
            max = 10,
            value = 1,
            step = 0.1
          ),
          sliderInput(
            "parA2",
            "Parameter 2",
            min = 0,
            max = 10,
            value = 1,
            step = 0.1
          ),
          sliderInput(
            "nn",
            "Sample size",
            min = 0,
            max = 5000,
            value = 1000
          ),
          selectizeInput(
            "plot",
            "Plot",
            choices = c(
              "Mean and standard deviation",
              "Box plot",
              "Violin plot",
              "Scatter plot",
              "Jittered scatter plot",
              "Box plot + jittered scatter plot",
              "Violin plot + jittered scatter plot"
            ),
            multiple = FALSE
          )
        ),
        
        mainPanel(
          h3("Suggested distributions"),
          p("Beta(0.2, 0.2) and Gamma(1, 0.1)"),
          h3("Selected plot"),
          plotOutput("visual"),
          h3("Histogram"),
          plotOutput("histogram")
        )
      )
    ),
    
    tabPanel(
      "Model fitting",
      fluid = TRUE,
      
      sidebarLayout(
        sidebarPanel(
          strong("Pick a distribution, its parameters, and a plot below"),
          br(),
          br(),
          
          fileInput(
            'target_upload',
            'Choose file to upload',
            accept = c('text/csv',
                       'text/comma-separated-values',
                       '.csv')
          ),
          radioButtons(
            "separator",
            "Separator: ",
            choices = c(";", ",", ":"),
            selected = ";",
            inline = TRUE
          )
        ),
        
        mainPanel(DT::dataTableOutput("sample_table"))
      )
      
    )
  ))

server <- function(input, output) {
  # Data generation
  distribution = reactive(input$disA)
  dataA <- reactive({
    if (distribution() == "Normal") {
      data.frame(Value = rnorm(input$nn, input$parA1, input$parA2),
                 Dataset = "Dataset A")
    } else if (distribution() == "Beta") {
      data.frame(
        Value = rbeta(
          input$nn,
          shape1 = input$parA1,
          shape2 = input$parA2
        ),
        Dataset = "Dataset A"
      )
    } else if (distribution() == "Gamma") {
      data.frame(
        Value = rgamma(
          input$nn,
          shape = input$parA1,
          rate = input$parA2
        ),
        Dataset = "Dataset A"
      )
    }
  })
  
  # Plots
  
  output$visual <- renderPlot({
    p_mean = ggplot(data = dataA(), aes(x = Dataset)) +
      geom_point(y = mean(dataA()$Value), size = 10) +
      geom_errorbar(aes(
        ymin = mean(dataA()$Value) - sd(dataA()$Value),
        ymax = mean(dataA()$Value) + sd(dataA()$Value)
      ),
      width = .2) +
      ylab("Value (mean and standard deviation)") +
      ggtheme + theme(axis.text.x = element_blank())
    
    p_box = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
      geom_boxplot() +
      ggtheme + theme(axis.text.x = element_blank())
    
    p_violin = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
      geom_violin() +
      ggtheme + theme(axis.text.x = element_blank())
    
    p_scatter = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
      geom_point(alpha = alpha) +
      ggtheme + theme(axis.text.x = element_blank())
    
    p_jitter = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
      geom_jitter(alpha = alpha,
                  width = 0.1,
                  height = 0) +
      ggtheme + theme(axis.text.x = element_blank())
    
    p_box_jitter = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
      geom_boxplot() +
      geom_jitter(alpha = alpha,
                  width = 0.1,
                  height = 0) +
      ggtheme + theme(axis.text.x = element_blank())
    
    p_violin_jitter = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
      geom_violin() +
      geom_jitter(alpha = alpha,
                  width = 0.1,
                  height = 0) +
      ggtheme + theme(axis.text.x = element_blank())
    
    if (input$plot == "Mean and standard deviation")  {
      print(p_mean)
    }
    if (input$plot == "Box plot")  {
      print(p_box)
    }
    if (input$plot == "Violin plot")  {
      print(p_violin)
    }
    if (input$plot == "Scatter plot")  {
      print(p_scatter)
    }
    if (input$plot == "Jittered scatter plot")  {
      print(p_jitter)
    }
    if (input$plot == "Box plot + jittered scatter plot")  {
      print(p_box_jitter)
    }
    if (input$plot == "Violin plot + jittered scatter plot")  {
      print(p_violin_jitter)
    }
    
  })
  
  output$histogram <- renderPlot({
    ggplot(data = dataA(), aes(x = Value)) +
      geom_histogram(fill = "gray", color = "black") +
      ylab("Count") +
      ggtheme
  })
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <-
      read.csv(inFile$datapath,
               header = TRUE,
               sep = input$separator)
    return(df)
  })
  
  output$sample_table <- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
}

shinyApp(ui = ui, server = server)
