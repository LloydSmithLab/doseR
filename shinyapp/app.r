# inspirations
# https://riptutorial.com/shiny/example/32449/uploading-csv-files-to-shiny
# https://shiny.rstudio.com/gallery/ncaa-swim-team-finder.html


library(shiny)
library(shinyWidgets)
library(ggplot2)

packages = c(
  "kableExtra",
  "ggplot2",
  "survival",
  "survminer",
  "patchwork",
  "cowplot",
  "PropCIs",
  "rstatix",
  "rstan",
  "shinystan",
  "RColorBrewer",
  "bayesplot",
  "rstanarm"
)

suppressMessages(invisible(lapply(packages,
                                  function(x) {
                                    if (!require(x, character.only = TRUE)) {
                                      install.packages(x, dependencies = TRUE)
                                      library(x, character.only = TRUE)
                                    }
                                  })))

ggtheme = theme_bw() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(2, 2, 2, 2)
    ),
    plot.subtitle = element_text(
      size = 16,
      hjust = 0.5,
      margin = margin(0, 0, 0, 0)
    ),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y.right = element_text(angle = 90),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16, color = "black"),
    axis.line = element_line(size = 0.25, color = "black"),
    axis.ticks = element_line(size = 0.25, color = "black"),
    strip.text = element_text(size = 12, margin = margin(1, 1, 2, 2)),
    strip.background = element_blank(),
    legend.margin = margin(2, 2, 2, 2),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(0.4, "cm"),
    legend.background = element_rect(colour = "transparent", fill = ggplot2::alpha("white", 0)),
    legend.position = "none"
  )


# alpha = 0.5

targets = c(10 ^ 1, 10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)
nn = 20
tt = seq(0, 14, 1)

f_infection_exp_cont = function(D_target, pinf, pcont, kk, tt, cc) {
  V_0 = rpois(1, D_target * pinf)
  V_eff = rbinom(1, V_0, (1 - pcont))
  V_t = V_eff * exp(kk * tt)
  lambda_t = cc * V_t
  P_transition_t = lambda_t * exp(-lambda_t)
  P_disease_t = 1 - exp(-(1 / exp(cc) * V_0 / kk) * (exp(kk * tt) - 1))
  
  data = as.data.frame(
    cbind(
      D_target,
      pinf,
      pcont,
      kk,
      tt,
      cc,
      V_0,
      V_eff,
      V_t,
      lambda_t,
      P_transition_t,
      P_disease_t
    )
  )
  
  return(data)
  
}

f_simulate = function(targets, nn, pinf, pcont, kk, tt, cc) {
  parameters = c(
    pinf = pinf,
    pcont = pcont,
    kk = kk,
    cc = cc,
    nn = nn
  )
  
  data = NULL
  
  for (ii in 1:length(targets)) {
    D_target = targets[ii]
    
    for (jj in 1:nn) {
      data_temp = f_infection_exp_cont(
        D_target,
        pinf = pinf,
        pcont = pcont,
        kk = kk,
        tt = tt,
        cc = cc
      )
      data_temp$sim = paste(targets[ii], jj, sep = "_")
      data_temp$nn = nn
      
      data = rbind(data, data_temp)
      
    }
    
  }
  
  # Approximated by histology
  data[data$V_0 > 0, "infected"] = 1
  data[data$V_0 == 0, "infected"] = 0
  
  # From weight loss and symptoms
  data$diseased = NA
  for (ii in 1:nrow(data)) {
    data$diseased[ii] = rbinom(1, 1, data$P_disease_t[ii])
    if (ii > 1) {
      if (data$sim[ii] == data$sim[ii - 1] &
          data$diseased[ii - 1] == 1 &
          !is.na(data$diseased[ii - 1] == 1)) {
        data$diseased[ii] = 1
      }
    }
  }
  
  data[is.na(data$diseased), "diseased"] = 0
  
  data[data$lambda_t < 0 | is.na(data$lambda_t < 0), "lambda_t"] = 0
  #data[data$Lambda_t<0 | is.na(data$Lambda_t<0), "Lambda_t"] = 0
  data[data$P_disease_t < 0 |
         is.na(data$P_disease_t < 0), "P_disease_t"] = 0
  
  return(data)
  
}

f_summary = function(data) {
  infected = unique(data[data$infected == 1, "sim"])
  diseased = unique(data[data$diseased == 1, "sim"])
  
  summary = unique(data[, c("D_target", "sim")])
  
  summary$infected = summary$diseased = 0
  
  summary[is.element(summary$sim, infected), "infected"] = 1
  summary[is.element(summary$sim, diseased), "diseased"] = 1
  
  summary$incubation = NA
  for (ii in 1:nrow(summary)) {
    if (nrow(data[data$sim == summary$sim[ii] &
                  data$diseased == 1,]) > 0) {
      summary$incubation[ii] = min(data[data$sim == summary$sim[ii] &
                                          data$diseased == 1, "tt"])
    } else{
      summary$incubation[ii] = Inf
    }
  }
  
  return(summary)
}


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
        "- Explore, and inform the design of your experiments and analytical pipelines using the Data simulation tab.",
        br(),
        "- Fit improved dose-response models and extract maximum insights from your data, and assess which model best capture the mechanisms at play using the Model fitting tab",
        br(),
        "doseR is part of the Pyterverse (under development)."
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
      img(
        src = 'doseR_model.png',
        height = "100%",
        width = "100%",
        align = "center"
      ),
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
          strong("Pick a dose-response model and its parameters"),
          br(),
          br(),
          
          # inputs
          selectizeInput(
            "DRmodel",
            "Dose-response model",
            choices = c("Time-dependent"),
            selected = "Time-dependent",
            multiple = FALSE
          ),
          sliderInput(
            "pinf",
            "Infection probability",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.05
          ),
          sliderInput(
            "pcont",
            "Infection control probability",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.05
          ),
          sliderInput(
            "kk",
            "Pathogen replication rate",
            min = 0,
            max = 10,
            value = 1,
            step = 0.05
          ),
          sliderInput(
            "cc",
            "Tolerance to disease",
            min = 0,
            max = 20,
            value = 10,
            step = 1
          )
        ),
        
        mainPanel(
          h3("Individual status"),
          plotOutput("plot_individual", height = "800px"),
          h3("Data summary"),
          splitLayout(
            cellWidths = c("50%", "50%"),
            plotOutput("plot_DR"),
            plotOutput("plot_incub")
          )
        )
      )
    ),
    
    tabPanel(
      "Model fitting",
      fluid = TRUE,
      
      sidebarLayout(
        sidebarPanel(
          strong("Import your data, pick a model to fit, and tune model priors"),
          br(),
          br(),
          
          fileInput(
            'target_upload',
            'Choose file to upload',
            accept = c('text/csv',
                       'text/comma-separated-values',
                       '.csv')
          ),
          
          selectizeInput(
            "DRmodel",
            "Dose-response model",
            choices = c("Time-dependent"),
            selected = "Time-dependent",
            multiple = FALSE
          ),
          
          
          
          sliderInput(
            "pinf_mean",
            "Priors on infection probability mean and standard deviation",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.05
          ),
          
          sliderInput(
            "pcont",
            "Priors on infection control probability mean",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.05
          ),
          
          sliderInput(
            "pcont",
            "Priors on infection control probability standard deviation",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.05
          ),
          
          sliderInput(
            "kk",
            "Priors on pathogen replication rate mean",
            min = 0,
            max = 10,
            value = 1,
            step = 0.05
          ),
          
          sliderInput(
            "kk",
            "Priors on pathogen replication rate standard deviation",
            min = 0,
            max = 10,
            value = 1,
            step = 0.05
          ),
          
          sliderInput(
            "cc",
            "Priors on tolerance to disease mean",
            min = 0,
            max = 20,
            value = 10,
            step = 1
          ),
          
          sliderInput(
            "cc",
            "Priors on tolerance to disease standard deviation",
            min = 0,
            max = 20,
            value = 10,
            step = 1
          )
          
        ),
        
        mainPanel(
          h2("Data"),
          p(
            "Download the ",
            a(
              href = "doseR_data_template.csv",
              "doseR data template",
              download = NA,
              target = "_blank"
            ),
            "to record your data."
          ),
          br(),
          br(),
          
          DT::dataTableOutput("sample_table"),
          br(),
          br(),
          h2("Output"),
          #withMathJax("$$\\text{Display formula in heading }X_n=X_{n-1}-\\varepsilon$$")
          # img(
          #   src = 'doseR_output.png',
          #   height = "100%",
          #   width = "100%",
          #   align = "center"
          # ),
          p()
        )
      )
      
    )
  ))

server <- function(input, output) {
  # Data generation
  # distribution = reactive(input$disA)
  # dataA <- reactive({
  #   if (distribution() == "Normal") {
  #     data.frame(Value = rnorm(input$nn, input$parA1, input$parA2),
  #                Dataset = "Dataset A")
  #   } else if (distribution() == "Beta") {
  #     data.frame(
  #       Value = rbeta(
  #         input$nn,
  #         shape1 = input$parA1,
  #         shape2 = input$parA2
  #       ),
  #       Dataset = "Dataset A"
  #     )
  #   } else if (distribution() == "Gamma") {
  #     data.frame(
  #       Value = rgamma(
  #         input$nn,
  #         shape = input$parA1,
  #         rate = input$parA2
  #       ),
  #       Dataset = "Dataset A"
  #     )
  #   }
  # })
  
  DRmodel = reactive(input$DRmodel)
  data <- reactive({
    if (DRmodel() == "Time-dependent") {
      data.frame(f_simulate(
        targets,
        nn,
        input$pinf,
        input$pcont,
        input$kk,
        tt,
        input$cc
      ))
    }
  })
  
  summary <- reactive({
    data.frame(f_summary(data()))
  })
  
  
  
  output$plot_individual <- renderPlot({
    #   plot_virus_log = ggplot(data(), aes(x = tt, y = log(V_t), group = sim)) +
    #   geom_line(aes(color = as.factor(D_target)), size = 1) +
    #   geom_point(aes(fill = as.factor(D_target)), color = "black", shape = 21) +
    #   scale_x_continuous(breaks = seq(0, 14, 4)) +
    #   scale_y_continuous(limits = c(0, NA)) +
    #   ggtheme +
    #   xlab("Day post-challenge") + ylab("Log(viral load)") +
    #   scale_color_brewer(name = "Inoculum (PFU)", labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)), palette = "Purples") +
    #   scale_fill_brewer(name = "Inoculum (PFU)", labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)), palette = "Purples")
    #
    # plot_virus = ggplot(data(), aes(x = tt, y = (V_t), group = sim))  +
    #   geom_line(aes(color = as.factor(D_target)), size = 1) +
    #   geom_point(aes(fill = as.factor(D_target)), color = "black", shape = 21) +
    #   scale_x_continuous(breaks = seq(0, 14, 4)) +
    #   scale_y_continuous(limits = c(0, NA)) +
    #   ggtheme +
    #   xlab("Day post-challenge") + ylab("Viral load") +
    #   scale_color_brewer(name = "Inoculum (PFU)", labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)), palette = "Purples") +
    #   scale_fill_brewer(name = "Inoculum (PFU)", labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)), palette = "Purples")
    #
    # plot_infected = ggplot(data(), aes(x = tt, y = infected, group = sim)) +
    #   geom_line(aes(color = as.factor(D_target)), size = 1) +
    #   geom_point(aes(fill = as.factor(D_target)), color = "black", shape = 21) +
    #   scale_x_continuous(breaks = seq(0, 14, 4)) +
    #   scale_y_continuous(limits = c(0,1)) +
    #   ggtheme +
    #   xlab("Day post-challenge") + ylab("Infection status") +
    #   scale_color_brewer(name = "Inoculum (PFU)", labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)), palette = "Purples") +
    #   scale_fill_brewer(name = "Inoculum (PFU)", labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)), palette = "Purples")
    #
    # plot_p_disease = ggplot(data(), aes(x = tt, y = P_disease_t, group = sim)) +
    #   geom_line(aes(color = as.factor(D_target)), size = 1) +
    #   geom_point(aes(fill = as.factor(D_target)), color = "black", shape = 21) +
    #   scale_x_continuous(breaks = seq(0, 14, 4)) +
    #   ggtheme +
    #   xlab("Day post-challenge") + ylab("Probability of\ntransition to symptomatic") +
    #   scale_color_brewer(name = "Inoculum (PFU)", labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)), palette = "Purples") +
    #   scale_fill_brewer(name = "Inoculum (PFU)", labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5)), palette = "Purples")
    #
    plot_diseased = ggplot(data(), aes(x = tt, y = diseased, group = sim)) +
      geom_line(#aes(color = as.factor(D_target)),
        color = "#9e9ac8",
        size = 1) +
      geom_point(
        #aes(fill = as.factor(D_target)),
        fill = "#54278f",
        color = "#54278f",
        shape = 21,
        size = 2
      ) +
      scale_x_continuous(breaks = seq(0, 14, 4)) +
      scale_y_continuous(breaks = c(0, 1)) +
      ggtheme  +
      xlab("Day post-challenge") + ylab("Symptomatic status") +
      scale_color_brewer(
        name = "Inoculum (PFU)",
        labels = c(
          expression(10 ^ 1),
          expression(10 ^ 2),
          expression(10 ^ 3),
          expression(10 ^ 4),
          expression(10 ^ 5)
        ),
        palette = "Purples"
      ) +
      scale_fill_brewer(
        name = "Inoculum (PFU)",
        labels = c(
          expression(10 ^ 1),
          expression(10 ^ 2),
          expression(10 ^ 3),
          expression(10 ^ 4),
          expression(10 ^ 5)
        ),
        palette = "Purples"
      )
    
    #(plot_virus | plot_p_disease | plot_diseased) + plot_layout(guides = "collect")
    
    print(plot_diseased + facet_grid(D_target ~ .))
    
    
    
    #print(input$pinf)
    #print(data())
    #print(summary())
    
  })
  
  output$plot_DR <- renderPlot({
    plot_DR = ggplot(summary(), aes(x = log(D_target), y = diseased)) +
      geom_jitter(
        width = 0.05,
        height = 0.05,
        fill = "#54278f",
        color = "#54278f",
        shape = 21,
        size = 2
      ) +
      stat_smooth(
        method = "glm",
        se = F,
        fullrange = T,
        method.args = list(family = binomial),
        color = "#9e9ac8"
      ) +
      xlab("Dose") + ylab("Sympatomatic status") +
      scale_x_continuous(breaks = c(log(targets)), labels = c(targets)) +
      scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0, 1)) +
      ggtheme
    
    print(plot_DR)
  })
  
  output$plot_incub <- renderPlot({
    plot_incub = ggplot(summary(), aes(x = log(D_target), y = incubation)) +
      geom_jitter(
        width = 0.05,
        height = 0.05,
        fill = "#54278f",
        color = "#54278f",
        shape = 21,
        size = 2
      ) +
      stat_smooth(
        method = "lm",
        se = F,
        fullrange = T,
        color = "#9e9ac8"
      ) +
      xlab("Dose") + ylab("Incubation time (day)") +
      scale_x_continuous(breaks = c(log(targets)), labels = c(targets)) +
      scale_y_continuous(limits = c(-0.1, 14.1), breaks = seq(0, 14, 2)) +
      ggtheme
    
    print(plot_incub)
  })
  
  
  
  
  # Plots
  
  # output$visual <- renderPlot({
  #   p_mean = ggplot(data = dataA(), aes(x = Dataset)) +
  #     geom_point(y = mean(dataA()$Value), size = 10) +
  #     geom_errorbar(aes(
  #       ymin = mean(dataA()$Value) - sd(dataA()$Value),
  #       ymax = mean(dataA()$Value) + sd(dataA()$Value)
  #     ),
  #     width = .2) +
  #     ylab("Value (mean and standard deviation)") +
  #     ggtheme + theme(axis.text.x = element_blank())
  #
  #   p_box = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
  #     geom_boxplot() +
  #     ggtheme + theme(axis.text.x = element_blank())
  #
  #   p_violin = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
  #     geom_violin() +
  #     ggtheme + theme(axis.text.x = element_blank())
  #
  #   p_scatter = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
  #     geom_point(alpha = alpha) +
  #     ggtheme + theme(axis.text.x = element_blank())
  #
  #   p_jitter = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
  #     geom_jitter(alpha = alpha,
  #                 width = 0.1,
  #                 height = 0) +
  #     ggtheme + theme(axis.text.x = element_blank())
  #
  #   p_box_jitter = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
  #     geom_boxplot() +
  #     geom_jitter(alpha = alpha,
  #                 width = 0.1,
  #                 height = 0) +
  #     ggtheme + theme(axis.text.x = element_blank())
  #
  #   p_violin_jitter = ggplot(data = dataA(), aes(x = Dataset, y = Value)) +
  #     geom_violin() +
  #     geom_jitter(alpha = alpha,
  #                 width = 0.1,
  #                 height = 0) +
  #     ggtheme + theme(axis.text.x = element_blank())
  #
  #   if (input$plot == "Mean and standard deviation")  {
  #     print(p_mean)
  #   }
  #   if (input$plot == "Box plot")  {
  #     print(p_box)
  #   }
  #   if (input$plot == "Violin plot")  {
  #     print(p_violin)
  #   }
  #   if (input$plot == "Scatter plot")  {
  #     print(p_scatter)
  #   }
  #   if (input$plot == "Jittered scatter plot")  {
  #     print(p_jitter)
  #   }
  #   if (input$plot == "Box plot + jittered scatter plot")  {
  #     print(p_box_jitter)
  #   }
  #   if (input$plot == "Violin plot + jittered scatter plot")  {
  #     print(p_violin_jitter)
  #   }
  #
  # })
  
  # output$histogram <- renderPlot({
  #   ggplot(data = dataA(), aes(x = Value)) +
  #     geom_histogram(fill = "gray", color = "black") +
  #     ylab("Count") +
  #     ggtheme
  # })
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <-
      read.csv(inFile$datapath,
               header = TRUE,
               sep = ";")
    return(df)
  })
  
  output$sample_table <- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
}

shinyApp(ui = ui, server = server)
