#https://github.com/gpilgrim2670/SwimMap/blob/master/app.R

#Navbar structure for UI
navbarPage("doseR | Alpha release", theme = shinytheme("lumen"),
           tabPanel("Home", fluid = TRUE, icon = icon("globe-americas"),
                    tags$style(button_color_css),
                    # Sidebar layout with a input and output definitions
                    sidebarLayout(
                      sidebarPanel(
                        
                        titlePanel("Desired Program Characteristics"),
                        #shinythemes::themeSelector(),
                        fluidRow(column(3,
                                        
                                        # Select which Gender(s) to plot
                                        checkboxGroupInput(inputId = "GenderFinder",
                                                           label = "Select Gender(s):",
                                                           choices = c("Male" = "M", "Female" = "F"),
                                                           selected = "M"),
                                        
                                        # Select which Division(s) to plot
                                        checkboxGroupInput(inputId = "DivisionFinder",
                                                           label = "Select Division(s):",
                                                           choices = c("DI", "DII", "DIII"),
                                                           selected = "DI")
                        ),
                        column(6, offset = 2,
                               # Select which Region(s) to plot
                               checkboxGroupInput(inputId = "RegionFinder",
                                                  label = "Select Region(s):",
                                                  choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific", "Alaska", "Hawaii"),
                                                  selected = "NewEngland")
                        )),
                        # Select Event
                        selectInput(inputId = "EventFinder",
                                    label = "Select Event",
                                    choices = levels(Events),
                                    selected = "50 Free",
                                    width = "220px"
                        ),
                        # Set Time Range
                        fluidRow(column(5,
                                        textInput(inputId = "TimeFinderMin",
                                                  label = "From:",
                                                  value = "19.00",
                                                  width = "100px")
                        ),
                        column(5, ofset = 3,
                               textInput(inputId = "TimeFinderMax",
                                         label = "To:",
                                         value = "22.00",
                                         width = "100px")
                        )),
                        helpText("Format example: 1:39.99"),
                        actionButton(inputId = "EnterTimes", label = "Enter Times"),
                        hr(),
                        sliderInput(inputId = "RankOnTeam",
                                    label = "Select Swimmer Rank On Team",
                                    min = 1,
                                    max = 10,
                                    value = c(1,6),
                                    width = "220px"),
                        helpText("For example: Find 1st fastest through 6th fastest athletes on a given team"),
                        hr(),
                        titlePanel("School Characteristics"),
                        # Select which School Type to plot
                        checkboxGroupInput(inputId = "School_TypeFinder",
                                           label = "Select School Type(s):",
                                           choices = c("National University", "Regional University", "National Liberal Arts College", "Regional College"),
                                           selected = c("National University", "Regional University", "National Liberal Arts College", "Regional College")),
                        
                        sliderInput(inputId = "School_RankFinder",
                                    label = "School Rank",
                                    min = 1,
                                    max = 250,
                                    value = c(1,250),
                                    width = "220px")
                      ),
                      mainPanel(
                        fluidRow(
                          column(3, offset = 9,
                                 
                                 radioButtons(inputId = "show_NamesFinder",
                                              label = "Display:",
                                              choices = c("School Names", "City Names", "Neither"),
                                              selected = "School Names")
                          )),
                        # hr(),
                        withSpinner(plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder"
                        )),
                        hr(),
                        fluidRow(column(7,
                                        helpText("Tip: Click locations to populate table below with information on schools in a specific area")
                                        #actionButton(inputId = "draw", label = "Input Event and Times")
                                        
                        ),
                        column(width = 2, offset = 2, conditionalPanel(
                          condition = "output.schoolstableFinder",
                          actionButton(inputId = "FinderClear", label = "Clear Table")))),
                        br(),
                        fluidRow(
                          withSpinner(dataTableOutput(outputId = "schoolstableFinder"))))
                    )
           ),
           
           tabPanel("Simulation", fluid = TRUE, icon = icon("swimmer"),
                    titlePanel("Program Comparisons"),
                    fluidRow(
                      column(6,
                             selectizeInput(inputId = "SchoolSelectA",
                                            label = "Select Schools (Max 4)",
                                            choices = unique(BigTop100$Team),
                                            multiple = TRUE,
                                            options = list(maxItems = 4, placeholder = 'Enter school name',
                                                           onInitialize = I('function() { this.setValue(""); }'))
                             ),
                             selectInput(inputId = "SchoolCompRace",
                                         label = "Select Event",
                                         choices = levels(Events),
                                         selected = "50 Free"),
                             helpText("Select school and event to create plots")
                      ),
                      column(6,
                             checkboxGroupInput(inputId = "SchoolCompGender",
                                                label = "Select Gender(s):",
                                                choices = c("Male" = "M", "Female" = "F"),
                                                selected = "M"),
                             radioButtons(inputId = "TuitionType",
                                          label = "Use In-State Tution?",
                                          choices = c("Yes", "No"),
                                          selected = "Yes"),
                             helpText("Note: In-state tuition will only apply to public schools")
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(6,
                             withSpinner(plotOutput(outputId = "SchoolCompPlotEvent"
                                                    # brush = "brush_SchoolComp"
                             )),
                             br(),
                             dataTableOutput(outputId = "SchoolCompDT")
                      ),
                      column(6,
                             dataTableOutput(outputId = "SchoolCompStats"),
                             helpText("For more information on school types and US News rankings please see More > About > School Types & Rankings")
                      )
                    )
           ),
           
           navbarMenu("Fitting", icon = icon("chart-bar"),
                      tabPanel("Times Comparision Between Divisions", fluid = TRUE,
                               tags$style(button_color_css),
                               titlePanel("Times Comparision Between Divisions"),
                               fluidRow(
                                 column(4,
                                        selectInput(inputId = "DivCompRaceA",
                                                    label = "Select Event",
                                                    choices = levels(Events),
                                                    selected = "50 Free")),
                                 column(4,
                                        sliderInput(inputId = "DivCompRankA",
                                                    label = "Top Times Range:",
                                                    min = 1, max = 3500,
                                                    value = c(1,250))),
                                 column(4,
                                        checkboxGroupInput(inputId = "DivCompGenderA",
                                                           label = "Select Gender(s):",
                                                           choices = c("Male" = "M", "Female" = "F"),
                                                           selected = "M"))),
                               hr(),
                               helpText("Tip: Highlight points to populate table"),
                               br(),
                               fluidRow(
                                 column(6,
                                        withSpinner(plotOutput(outputId = "DivCompPlotA",
                                                               brush = "brush_plotDiv"
                                                               #click = "click_plotDiv"
                                        ))),
                                 # hr(),
                                 # conditionalPanel(
                                 #   condition = "output.DivCompTable",
                                 # column(1.5, offset = 10.5, actionButton(inputId = "DivCompClear", label = "Clear Table"))
                                 # ),
                                 #br(),
                                 column(6,
                                        dataTableOutput(outputId = "DivCompTable")
                                 ))),
                      tabPanel("NCAA Regulation Differences By Division", fluid = TRUE,
                               
                               column(6,
                                      br(),
                                      h4("Differences Between NCAA Divisions"),
                                      h5(p(
                                        "The NCAA rules regarding eligibility of student athletes, scholarships, transfers, time commitments, etc. can be quite complex.  This is intended only as a general primer.  For more information please visit the ",
                                        a("NCAA.",
                                          href = "http://www.ncaa.org/about/frequently-asked-questions-about-ncaa"))),
                                      h5(p("There are three divisions in the NCAA.  They differ in their makeup, in terms of which types of schools choose to participate in which division, but the most significant differences between the divisions concern athletic scholarships."
                                      )),
                                      h5(p(
                                        "Put most simply schools that compete in Division I and Division II are allowed to offer athletic scholarships.  Division III schools are not.  The number of scholarships available differ by gender, with limits imposed by the NCAA.  For men’s  swimming and diving (taken together) Division I schools are allowed to offer a total of 9.9 full scholarships, whereas Division II schools can only offer 8.1 full scholarships.  For women’s swimming and diving the limits are 14 and 8.1 respectively.  These scholarships can be split into partials, with multiple student athletes receiving a portion of a full scholarship.  How scholarships are doled out is usually up to the coach. Coaches might attempt to recruit a few high powered athletes by offering them full scholarships, and give less to others, or they might distribute the scholarship portions more evenly.  In swimming Division I is generally faster than Division II, which in turn is faster than Division III, at least at the faster end.  Performance differences between the divisions can be explored by event using the plot at left."
                                      )),
                                      h5(p(
                                        "While upper limits on scholarships are imposed by the NCAA, actual scholarships available also depend on the financial circumstances of the school and the swimming/diving program.  Schools may be allowed to offer more scholarships than they can afford."
                                      )),
                                      h5(p(
                                        "Schools can also choose not to offer athletic scholarships, either in a particular sport, or across the board.   The eight Ivy League schools for example compete in Division I but as a policy do not offer any athletic scholarships."
                                      )),
                                      h5(p(
                                        "Regarding time commitments, Division I and II teams are permitted by the NCAA to practice out of season.  Division III teams may only practice during the season.  In all cases seasons are dined by NCAA rules, with strict limits for what is and isn’t in-season.  All divisions are bound by the “20-hour” rule, where athletes are only permitted to practice for 20 hours a week during the season.  In reality athletes practice often practice much more, especially in ",
                                        a("Division I.",
                                          href = "https://www.businessinsider.com/college-student-athletes-spend-40-hours-a-week-practicing-2015-1"))
                                      ))),
                      
                      tabPanel("Division I Swimming Makeup", fluid = TRUE,
                               titlePanel("Division I School Types"),
                               sidebarLayout(
                                 sidebarPanel(
                                   # Select which Gender(s) to plot
                                   checkboxGroupInput(inputId = "GenderDI",
                                                      label = "Select Gender(s):",
                                                      choices = c("Male" = "M", "Female" = "F"),
                                                      selected = "M"),
                                   # Select which Region(s) to plot
                                   checkboxGroupInput(inputId = "RegionDI",
                                                      label = "Select Region:",
                                                      choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific", "Alaska", "Hawaii"),
                                                      selected = c("NewEngland", "MidAtlantic", "MidWest", "South", "West", "SouthWest", "Pacific", "Alaska", "Hawaii")),
                                   # Set Top X Rank
                                   sliderInput(inputId = "RankDI",
                                               label = "Top Times Range:",
                                               min = 1, max = 3500,
                                               value = c(1,250)),
                                   # Set school rank
                                   sliderInput(inputId = "School_RankDI",
                                               label = "School Rank",
                                               min = 1,
                                               max = 250,
                                               value = c(1,250))
                                 ),
                                 mainPanel(
                                   withSpinner(plotOutput(outputId = "barplotDI")),
                                   textOutput(outputId = "description_DI")
                                   #plotOutput(outputId = "scatterplotDI")
                                 )
                               )
                      ),
                      tabPanel("Division II Swimming Makeup", fluid = TRUE,
                               titlePanel("Division II School Types"),
                               sidebarLayout(
                                 sidebarPanel(
                                   # Select which Gender(s) to plot
                                   checkboxGroupInput(inputId = "GenderDII",
                                                      label = "Select Gender(s):",
                                                      choices = c("Male" = "M", "Female" = "F"),
                                                      selected = "M"),
                                   # Select which Region(s) to plot
                                   checkboxGroupInput(inputId = "RegionDII",
                                                      label = "Select Region:",
                                                      choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific", "Alaska", "Hawaii"),
                                                      selected = c("NewEngland", "MidAtlantic", "MidWest", "South", "West", "SouthWest", "Pacific", "Alaska", "Hawaii")),
                                   # Set Top X Rank
                                   sliderInput(inputId = "RankDII",
                                               label = "Top Times Range:",
                                               min = 1, max = 3500,
                                               value = c(1,250)),
                                   # Set school rank
                                   sliderInput(inputId = "School_RankDII",
                                               label = "School Rank",
                                               min = 1,
                                               max = 250,
                                               value = c(1,250))
                                 ),
                                 mainPanel(
                                   withSpinner(plotOutput(outputId = "barplotDII")),
                                   textOutput(outputId = "description_DII")
                                 )
                               )
                      ),
                      tabPanel("Division III Swimming Makeup", fluid = TRUE,
                               titlePanel("Division III School Types"),
                               sidebarLayout(
                                 sidebarPanel(
                                   # Select which Gender(s) to plot
                                   checkboxGroupInput(inputId = "GenderDIII",
                                                      label = "Select Gender(s):",
                                                      choices = c("Male" = "M", "Female" = "F"),
                                                      selected = "M"),
                                   # Select which Region(s) to plot
                                   checkboxGroupInput(inputId = "RegionDIII",
                                                      label = "Select Region:",
                                                      choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific", "Alaska", "Hawaii"),
                                                      selected = c("NewEngland", "MidAtlantic", "MidWest", "South", "West", "SouthWest", "Pacific", "Alaska", "Hawaii")),
                                   # Set Top X Rank
                                   sliderInput(inputId = "RankDIII",
                                               label = "Top Times Range:",
                                               min = 1, max = 3500,
                                               value = c(1,250)),
                                   # Set school rank
                                   sliderInput(inputId = "School_RankDIII",
                                               label = "School Rank",
                                               min = 1,
                                               max = 250,
                                               value = c(1,250))
                                 ),
                                 mainPanel(
                                   withSpinner(plotOutput(outputId = "barplotDIII")),
                                   textOutput(outputId = "description_DIII")
                                 )
                               )
                      )
           )
)

# https://riptutorial.com/shiny/example/32449/uploading-csv-files-to-shiny

library(shiny)
library(DT)

# Define UI
ui <- shinyUI(fluidPage(
  
  fileInput('target_upload', 'Choose file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )),
  radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
  DT::dataTableOutput("sample_table")
)
)

# Define server logic
server <- shinyServer(function(input, output) {
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    return(df)
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
}
)

# Run the application 
shinyApp(ui = ui, server = server)
