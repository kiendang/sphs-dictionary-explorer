library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)


data_dir <- file.path("data")


dictionary_keys <- c("main", "revisit", "screening")


dictionary_keys <- c(
  "Main" = "main",
  "Revisit - main questionaire" = "revisit",
  "Revisit - health screening" = "screening"
)


combined_dictionary <- readRDS(file.path(data_dir, "dictionary.rds")) |>
  mutate(id = 1:n(), .before = everything()) |>
  mutate_if(is.factor, as.character)


filter_columns <- c("questionaire", "section", "subsection")


column_values <- filter_columns |>
  add_names() |>
  map(function(col) {
    combined_dictionary |>
      pull(!!sym(col)) |>
      unique() |>
      remove_na() |>
      add_names()
    })


column_value_names <- column_values
column_value_names$questionaire <- dictionary_keys


ui <- dashboardPage(
  dashboardHeader(
    title = "SPHS dictionary explorer",
    titleWidth = 280
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(
        width = 3,
        box(
          width = NULL,
          solidHeader = TRUE,
          filter_columns |> map(function(col) {
            selectizeInput(
              sprintf("%s-select", col),
              label = tools::toTitleCase(col),
              choices = column_value_names[[col]],
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )
          }),
          downloadButton("download", "Download")
        )
      ),
      column(
        width = 9,
        box(
          width = NULL,
          background = "purple",
          htmlOutput("filter-render")
        ),
        box(
          width = NULL,
          solidHeader = TRUE,
          dataTableOutput("dictionary")
        )
      )
    )
  ),
  skin = "yellow"
)


# ui <- fluidPage(
#   titlePanel("SPHS dictionary explorer"),
#   sidebarLayout(
#     sidebarPanel(
#       filter_columns |> map(function(col) {
#         selectizeInput(
#           sprintf("%s-select", col),
#           label = tools::toTitleCase(col),
#           choices = column_value_names[[col]],
#           multiple = TRUE,
#           options = list(plugins = list("remove_button"))
#         )
#       }),
#       downloadButton("download", "Download")
#     ),
#     mainPanel(
#       wellPanel(htmlOutput("filter-render")),
#       dataTableOutput("dictionary")
#       , verbatimTextOutput("out")
#     )
#   )
# )


server <- function(input, output, session) {
  values <- reactiveValues(
    filters = list(),
    selected = character()
  )

  filters <- reactive(values$filters |> keep(\(x) as.logical(length(x))))

  filter_columns |> walk(function(col) {
    observe({
      includes <- input[[sprintf("%s-select", col)]]

      isolate({
        values$filters[[col]] <- includes
      })
    })
  })

  dictionary <- reactive({
    combined_dictionary |> select(-variables) |> evaluate_filter(filters())
  })

  proxy <- dataTableProxy("dictionary")

  observe({
    df <- dictionary()

    isolate({
      replaceData(proxy, df)
      selectRows(proxy, which(df$id %in% values$selected))
    })
  })

  filter_columns |> walk(function(col) {
    observe({
      filters <- filters()
      filters[[col]] <- NULL

      df <- combined_dictionary |>
        select(-variables) |>
        evaluate_filter(filters)

      isolate({
        col_sym <- sym(col)

        name_df <- data.frame(x = column_value_names[[col]])
        names(name_df) <- col

        choices_df <- df |>
          group_by(!!sym(col)) |>
          summarise(n = n()) |>
          right_join(name_df, by = col) |>
          mutate(n = ifelse(is.na(n), 0L, n)) |>
          mutate(n = sprintf("%s (%d)", !!col_sym, n))

        choices <- choices_df[[col]]
        names(choices) <- choices_df$n

        updateSelectizeInput(
          session,
          sprintf("%s-select", col),
          label = tools::toTitleCase(col),
          choices = choices,
          selected = input[[sprintf("%s-select", col)]]
        )
      })
    })
  })

  observe({
    clicked <- input$dictionary_row_last_clicked

    isolate({
      values$selected <-
        if (
          length(n <- dictionary()$id[clicked]) &&
          n %in% values$selected
        ) {
          setdiff(values$selected, n)
        } else {
          unique(c(values$selected, n))
        }
    })
  })

  output$out <- renderPrint(values$filters)

  output$download <- downloadHandler(
    filename = "variables.csv",
    content = function(file) {
      data.table::fwrite(
        combined_dictionary |>
          filter(id %in% values$selected) |>
          # rename(group = name) |>
          unnest() |>
          rename(group = name, name = name1, details = definition1),
        file
      )
    }
  )

  output$dictionary <- DT::renderDataTable({
    isolate({
      df <- dictionary()

      datatable(
        df,
        filter = "none",
        colnames = tools::toTitleCase(names(df)),
        options = list(
          columnDefs = list(
            list(targets = c(1:(ncol(df))), className = "dt-head-left"),
            list(targets = 1, visible = FALSE)
          )
        ),
        selection = list(selected = which(df$name %in% values$selected)),
        style = "bootstrap4"
      )
    })
  })

  output$`filter-render` <- renderUI(
    if (length(filters <- filters())) {
      p(HTML(render_filter(filters)))
    } else {
      p("Use the filter panel to customize your search.")
    }
  )
}


shinyApp(ui = ui, server = server)
