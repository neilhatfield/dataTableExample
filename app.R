library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(DT)
library(palmerpenguins)
library(psych)
library(tibble)
library(dplyr)


# App Meta Data-----------------------------------------------------------------
APP_TITLE  <<- "DT-Data Tables"
APP_DESCP  <<- paste(
  "This app is to test and demonstrate the useage of the DT package to render",
  "data tables in a shiny app."
)
# End App Meta Data-------------------------------------------------------------

# User Interface ----
ui <- list(
  dashboardPage(
    skin = "blue",
    # Header ----
    dashboardHeader(
      title = "DT Examples",
      titleWidth = 250,
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    # Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem(
          text = "Overview",
          tabName = "overview",
          icon = icon("tachometer-alt")
        ),
        menuItem(
          text = "Raw Data Table",
          tabName = "rawData",
          icon = icon("table")
        ),
        menuItem(
          text = "Descriptive Statistics",
          tabName = "descStats",
          icon = icon("table")
        ),
        menuItem(
          text = "ANOVA Table",
          tabName = "anovaTable",
          icon = icon("table")
        ),
        menuItem(
          text = "Explore Options",
          tabName = "explore",
          icon = icon("wpexplorer")
        ),
        menuItem(
          text = "References",
          tabName = "references",
          icon = icon("leanpub")
        )
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Body ----
    dashboardBody(
      tabItems(
        #### Overview ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Demostration of DT Package in Shiny"),
          p("The purpose of this app is to demonstrate the code for various types
            of data tables using the DT package. Recall that we want you to use
            the DT package rather than the built in ones for several reasons
            including:"),
          tags$ul(
            tags$li("The DT package gives us many more control choices for tables
                    than other packages and functions."),
            tags$li("The DT package provides automatic accessibility for users."),
            tags$li("Consistency across all of the apps.")
          ),
          h2("Key Pages"),
          p("There are four key pages to this app: three examples and a DT Options
            Exploration page. Each example will show you what a table might look
            like for a different purpose. Currently, I have coded the following
            examples:"),
          tags$ul(
            tags$li("Displaying raw data"),
            tags$li("Displaying a set of values of several descriptive statistics"),
            tags$li("Displaying a classical ANOVA table")
          ),
          p("While there are many more usages of tables, these three cover many
            of the basics and can be extended to cover other usages."),
          p("For each example, I provide what the table should look like for the
            user (the target display), the code from the UI and the code from the
            server. The code examples should work the same, regardless of whether
            you're using the single file approach (i.e., app.R) or the two file
            approach (i.e., ui.R & server.R)."),
          p("The final page is where a user can explore several of the DT options
            and see a real time of those options on both the code and the rendered
            product."),
          h2("Background Note"),
          p("An important background note is that this app is meant to be used
            with students (and others) who are building/improving apps within the",
            tags$a(href = "https://sites.psu.edu/shinyapps/",
                   "Book of Apps for Statistics Teaching (BOAST)",
                   class = "bodylinks"),
            " as well as the associated ",
            tags$a(href = "https://educationshinyappteam.github.io/Style_Guide/",
                   "Style Guide.",
                   class = "bodylinks")
          ),
          p(
            div(class = "updated", "Last Update: 10/26/2020 by NJH.")
          ),
        ),
        #### Raw Data Table ----
        tabItem(
          tabName = "rawData",
          withMathJax(),
          h2("Displaying a Raw Data Table"),
          p("One common use of a data table is to present raw data. Ensure that
            showing the raw data is important for the learning outcomes associated
            with your app. That is, don't include a raw data table just because
            you can."),
          p("When presenting raw data is important for your user to develop
            productive meanings, then you'll want to reference this example to
            help you format your display."),
          hr(),
          h3("Target Display"),
          p("This is what you'll want to aim for. Keep in mind that depending on
            the number of rows and columns, you might want to turn on/off features
            such as searching, paging, etc."),
          DT::DTOutput("rawTable1"),
          hr(),
          h3("UI Code"),
          p("In your UI portion, all you need to include is the following line
            at the place where you want to put the table:"),
          code('DT::DTOutput("rawTable1")'),
          hr(),
          h3("Server Code"),
          p("This is the code that you would need to include in your server
            section:"),
          pre(
            code('
  output$rawTable1 <- DT::renderDT(
    expr = carData,
    caption = "Motor Trend US Data, 1973-1974 Models",
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-center", targets = 1:ncol(carData))
      )
    )
  )'
            )
          )
        ),
        # Descriptive Statistics Table ----
        tabItem(
          tabName = "descStats",
          withMathJax(),
          h2("Displaying a Table of Descriptive Statistics"),
          p("One of the more powerful uses of a table is to display a
            summary table of various descriptive statistics. While this might seem
            like overkill when you just want to display Tukey's Five Number Summary,
            we want to have a professional look to our apps. Thus, using ",
            tags$code("renderPrint"), " with the ", tags$code("summary"),
            " function doesn't cut it."),
          p("You will need to make several choices such as: which statistics to
            include values for and whether you want to use a vertical or horizontal
            layout. I have provided an example that is on the more complicated
            end of the descriptive statistics layout."),
          p(tags$em("Note: "), "This is also an example of the ",
            tags$code("psych::describeBy"), " function."),
          hr(),
          h3("Target Display"),
          p("This is what you'll want to aim for:"),
          DT::DTOutput("penguinSummary"),
          hr(),
          h3("UI Code"),
          p("In your UI portion, all you need to include is the following line
            at the place where you want to put the table:"),
          code('DT::DTOutput("penguinSummary")'),
          hr(),
          h3("Server Code"),
          p("This is the code that you would need to include in your server
            section:"),
          pre(
            code('
# Prepare the data frame ----
  penStats <- psych::describeBy(
    x = penguins$body_mass_g,
    group = penguins$species,
    mat = TRUE, # Formats output appropriate for DT
    digits = 3 # sets the number of digits retained
  )

# Picking which columns to keep
  penStats <- penStats[, c("group1", "n", "mean", "sd", "median", "mad", "min",
                           "max", "skew", "kurtosis")]

# Make the group1 column the row names
penStats <- tibble::remove_rownames(penStats)
penStats <- tibble::column_to_rownames(penStats, var = "group1")

# Improve column names
names(penStats) <- c("Count", "SAM (g/penguin)", "SASD (g)", "Median (g)",
                       "MAD (g)", "Min (g)", "Max (g)", "Sample Skewness (g^3)",
                       "Sample Excess Kurtosis (g^4)")

# Render the table ----
  output$penguinSummary <- DT::renderDT(
    expr = penStats,
    caption = "Descriptive Stats for Palmer Penguins",
    style = "bootstrap4",
    rownames = TRUE,
    autoHideNavigation = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      paging = FALSE,
      searching = FALSE,
      columnDefs = list(
        list(className = "dt-center",
             targets = 1:ncol(penStats))
      )
    )
  )'
            )
          )
        ),
        # ANOVA Table Example ----
        tabItem(
          tabName = "anovaTable",
          withMathJax(),
          h2("Displaying an ANOVA Table"),
          p("Another powerful usage of tables is to format results tables. Again,
            this comes back to creating professional looking displays that are
            accessible. Displaying raw output (or console output) is NOT what we
            want to have here."),
          p("For this example, I'm going to display a traditional/classical
            Oneway ANOVA results table."),
          hr(),
          h3("Target Display"),
          p("This is what you'll want to aim for:"),
          DT::DTOutput("penguinAnova"),
          hr(),
          h3("UI Code"),
          p("In your UI portion, all you need to include is the following line
            at the place where you want to put the table:"),
          code('DT::DTOutput("penguinAnova")'),
          hr(),
          h3("Server Code"),
          p("This is the code that you would need to include in your server
            section:"),
          pre(
            code('
# Prepare the data frame ----
  penModel <- aov(body_mass_g ~ species*sex, data = penguins)
  anovaPen <- anova(penModel)
  rowNames <- row.names(anovaPen)
  anovaPen <- apply(anovaPen,
                    MARGIN = 2,
                    FUN = "prettyNum",
                    big.mark = ",",
                    digits = 4)
  anovaPen <- as.data.frame(anovaPen) %>%
    dplyr::mutate(
      `p value` = ifelse(`Pr(>F)` < 0.0001, "< 0.0001", `Pr(>F)`)
    ) %>%
    dplyr::select(-`Pr(>F)`) %>%
    dplyr::na_if("NA")
  row.names(anovaPen) <- rowNames

# Render the table ----
  output$penguinAnova <- DT::renderDT(
    expr = anovaPen,
    caption = "(Classical) ANOVA Table for Palmer Pengins",
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      ordering = FALSE,
      columnDefs = list(
        list(className = "dt-center",
             targets = 1:ncol(anovaPen))
      )
    )
  )'
            )
          )
        ),
        ## Explore Options ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the Data Table Options"),
          p("There are a variety of options built into DataTables Javascript
            library which the DT R package allows us to access. While I won't
            highlight all of the options, I will highlight some of the most useful
            and important ones to consider as you create a data table for a
            Shiny app."),
          p("All you need to put in the UI portion of your app's code is",
            code('DT::DTOutput("optsTable")')
            ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Options"),
                textInput(
                  inputId = "caption",
                  label = "Caption text",
                  value = "Motor Trend US Data, '73-'74 Models"
                ),
                p(id = "labelRowNames", "Show row names"),
                switchInput(
                  inputId = "rowNames",
                  label = NULL,
                  value = TRUE,
                  onLabel = "TRUE",
                  offLabel = "FALSE",
                  size = "large"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('rowNames').setAttribute('aria-labelledby',
                  'labelRowNames')
                  })"
                )),
                p(id = "labelScrollX", "Scroll horizontal"),
                switchInput(
                  inputId = "scrollX",
                  label = NULL,
                  value = TRUE,
                  onLabel = "TRUE",
                  offLabel = "FALSE",
                  size = "large"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('scrollX').setAttribute('aria-labelledby',
                  'labelScrollX')
                  })"
                )),
                p(id = "labelOrdering", "Order columns"),
                switchInput(
                  inputId = "ordering",
                  label = NULL,
                  value = TRUE,
                  onLabel = "TRUE",
                  offLabel = "FALSE",
                  size = "large"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('ordering').setAttribute('aria-labelledby',
                  'labelOrdering')
                  })"
                )),
                p(id = "labelPaging", "Use pages"),
                switchInput(
                  inputId = "paging",
                  label = NULL,
                  value = TRUE,
                  onLabel = "TRUE",
                  offLabel = "FALSE",
                  size = "large"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('paging').setAttribute('aria-labelledby',
                  'labelPaging')
                  })"
                )),
                p(id = "labelLengthChange", "Allow page length change"),
                switchInput(
                  inputId = "lengthChange",
                  label = NULL,
                  value = TRUE,
                  onLabel = "TRUE",
                  offLabel = "FALSE",
                  size = "large"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('lengthChange').setAttribute('aria-labelledby',
                  'labelLengthChange')
                  })"
                )),
                numericInput(
                  inputId = "pageLength",
                  label = "Set page length",
                  value = 10,
                  min = 1,
                  max = 100,
                  step = 1
                ),
                p(id = "labelSearching", "Allow searching"),
                switchInput(
                  inputId = "searching",
                  label = NULL,
                  value = TRUE,
                  onLabel = "TRUE",
                  offLabel = "FALSE",
                  size = "large"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('searching').setAttribute('aria-labelledby',
                  'labelSearching')
                  })"
                )),
                p(id = "labelInfo", "Show information"),
                switchInput(
                  inputId = "info",
                  label = NULL,
                  value = TRUE,
                  onLabel = "TRUE",
                  offLabel = "FALSE",
                  size = "large"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('info').setAttribute('aria-labelledby',
                  'labelInfo')
                  })"
                )),
                p(id = "labelResponsive", "Make table responsive"),
                switchInput(
                  inputId = "responsive",
                  label = NULL,
                  value = TRUE,
                  onLabel = "TRUE",
                  offLabel = "FALSE",
                  size = "large"
                ),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('responsive').setAttribute('aria-labelledby',
                  'labelResponsive')
                  })"
                )),
                selectInput(
                  inputId = "colAlign",
                  label = "Column alignment",
                  choices = list(
                    "left" = "dt-left",
                    "center" = "dt-center",
                    "right" = "dt-right"
                  ),
                  selected = "dt-center"
                )
              )
            ),
            column(
              width = 8,
              h3("Server Code"),
              verbatimTextOutput("exploreCode"),
              h3("Options Meanings"),
              tags$ul(
                tags$li(strong("Caption text: "), "this is the text that serves
                        as the built-in title of the table."),
                tags$li(strong("Show row names: "), "toggles whether or not row
                        names are listed on the left edge of the table. Notice
                        that sometimes this can cause rows to be filtered out of
                        the table. Use with caution."),
                tags$li(strong("Scroll horizontal: "), "toggles whether or not
                        you let the user scroll the table horizontallly (left and
                        right). Set to TRUE if you have a lot of columns,
                        otherwise your user might not be able to see some columns.
                        Note: there is also a scroll option for vertical, ",
                        code("scrollY"), "."),
                tags$li(strong("Order columns: "), "allows the user to sort
                        columns in ascending or descending order."),
                tags$li(strong("Use pages: "), "allows for a data table with
                        many rows to be broken across several pages. Use this
                        option when you have many rows that do not need to
                        displayed all at the same time."),
                tags$li(strong("Allow page length change: "), "this options
                        allows the user to set how many entries are displayed
                        at once through the dropdown menu. Default values are 10,
                        25, 50, and 100."),
                tags$li(strong("Set page length: "), "this lets you change the
                        default number of rows showing."),
                tags$li(strong("Allow search: "), "this adds/removes a search bar,
                        allowing your user to search through the data table."),
                tags$li(strong("Show info: "), "this adds/removes the 'Showing #
                        to # of # entries` line just below the table. This isn't
                        needed for small data tables nor summary tables."),
                tags$li(strong("Make responsive: "), "this will enable the table
                        to adapt to various screensizes such as on a mobile phone."),
                tags$li(strong("Column alignment: "), "this gives the user control
                        over how values in the columns get aligned. Notice that
                        we target columns by index where the first column is NOT
                        the left-most column (which is the row names).")
              )
            )
          ),
          hr(),
          h3("Resulting Table"),
          DT::DTOutput("optsTable")
        ),
        # References ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities
            R package version 0.1.7.
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J.,Allaire, JJ , Xie, Y. and McPherson, J. (2020).
            shiny: Web Application Framework for R. R package version 1.5.0.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Ribeiro, B. B. (2018). shinydashboard: Create Dashboards
            with 'Shiny'. R package version 0.7.1.
            Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Henderson and Velleman (1981). Motor trend car road tests data in
            Building multiple regression models interactively. Biometrics, 37,
            391–411."
          ),
          p(
            class = "hangingindent",
            "Horst, A. M., Hill, A. P., and Gorman, K. B. (2020). palmerpenguins:
            Palmer Archipelago (Antartica) penguin data. R package version 0.1.0.
            Available from https://allisonhorst.github.io/palmerpenguins/"
          ),
          p(
            class = "hangingindent",
            "Müller, K. and Wickham, H. (2020). tibble: Simple Data Frames.
            R package version 3.0.4.
            Available from https://CRAN.R-project.org/package=tibble"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F. and Granjon, D. (2020). shinyWidgets: Custom
            Inputs Widgets for Shiny. R package version 0.5.4.
            Available from https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Revelle, W. (2020). psych: Procedures for psychological, psychometric,
            and personality research. R package version 2.0.9.
            Available from https://CRAN.R-project.org/package=psych"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., and Müller, K. (2020).
            dplyr: A Grammar of data manipulation. R package version 1.0.2.
            Available from https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2020). DT: A Wrapper of the JavaScript
            library 'DataTables'. R package version 0.16.
            Available from https://CRAN.R-project.org/package=DT"
          )
        )
      )
    )
  )
)

# Define the Server ------------------------------------------------------------
server <- function(input, output, session){
  # Raw Data Table Server ----

  ## Prepare the data frame ----
  carData <- mtcars[,c("mpg", "cyl", "hp", "gear", "wt")]
  names(carData) <- c("MPG", "Num. of Cylinders", "Horsepower", "Num of Gears",
                      "Weight")

  ## Render the table ----
  output$rawTable1 <- DT::renderDT(
    expr = carData,
    caption = "Motor Trend US Data, 1973-1974 Models",
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-center", targets = 1:ncol(carData))
      )
    )
  )

  # Descriptive Statistics Table ----
  ## Prepare the data frame ----
  penStats <- psych::describeBy(
    x = penguins$body_mass_g,
    group = penguins$species,
    mat = TRUE, # Formats output appropriate for DT
    digits = 3 # sets the number of digits retained
  )

  # Picking which columns to keep
  penStats <- penStats[, c("group1", "n", "mean", "sd", "median", "mad", "min",
                           "max", "skew", "kurtosis")]
  # Make the group1 column the row names
  penStats <- tibble::remove_rownames(penStats)
  penStats <- tibble::column_to_rownames(penStats, var = "group1")
  # Improve column names
  names(penStats) <- c("Count", "SAM (g/penguin)", "SASD (g)", "Median (g)",
                       "MAD (g)", "Min (g)", "Max (g)", "Sample Skewness (g^3)",
                       "Sample Excess Kurtosis (g^4)")

  ## Render the table ----
  output$penguinSummary <- DT::renderDT(
    expr = penStats,
    caption = "Descriptive Stats for Palmer Penguins",
    style = "bootstrap4",
    rownames = TRUE,
    autoHideNavigation = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      paging = FALSE, # Set to False for small tables
      searching = FALSE, # Set to False to turn of the search bar
      columnDefs = list(
        list(className = 'dt-center',
             targets = 1:ncol(penStats))
      )
    )
  )

  # ANOVA Table Example ----
  ## Prepare the data frame ----
  penModel <- aov(body_mass_g ~ species*sex, data = penguins)
  anovaPen <- anova(penModel)
  rowNames <- row.names(anovaPen)
  anovaPen <- apply(anovaPen,
                    MARGIN = 2,
                    FUN = "prettyNum",
                    big.mark = ",",
                    digits = 4)
  anovaPen <- as.data.frame(anovaPen) %>%
    dplyr::mutate(
      `p value` = ifelse(`Pr(>F)` < 0.0001, "< 0.0001", `Pr(>F)`)
    ) %>%
    dplyr::select(-`Pr(>F)`) %>%
    dplyr::na_if("NA")
  row.names(anovaPen) <- rowNames

  ## Render the table ----
  output$penguinAnova <- DT::renderDT(
    expr = anovaPen,
    caption = "(Classical) ANOVA Table for Palmer Pengins",
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      ordering = FALSE,
      columnDefs = list(
        list(className = 'dt-center',
             targets = 1:ncol(anovaPen))
      )
    )
  )

  ## Options Demonstration ----
  output$optsTable <- renderDT(
    expr = carData,
    caption = input$caption,
    style = "bootstrap4",
    rownames = input$rowNames,
    options = list(
      responsive = TRUE,
      lengthChange = input$lengthChange,
      pageLength = input$pageLength,
      scrollX = input$scrollX,
      ordering = input$ordering,
      paging = input$paging,
      searching = input$searching,
      info = input$info,
      columnDefs = list(
        list(className = input$colAlign, targets = 1:ncol(carData))
      )
    )
  )

  output$exploreCode <- renderText({
    paste0(
      'output$optsTable <- renderDT(
    expr = carData,
    caption = "', input$caption, '",
    style = "bootstrap4",
    rownames = ', input$rowNames, ',
    options = list(
      responsive = ', input$responsive, ',
      scrollX = ', input$scrollX, ',
      ordering = ', input$ordering, ',
      paging = ', input$paging, ',
      lengthChange = ', input$lengthChange, ',
      pageLength = ', input$pageLength, ',
      searching = ', input$searching, ',
      info = ', input$info, ',
      columnDefs = list(
        list(className = ', input$colAlign, ', targets = 1:ncol(carData))
      )
    )
  )'
    )
  })

}


# App Call
boastApp(ui = ui, server = server)