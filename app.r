library(DT)
library(dplyr)
library(tidyr)


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


# dictionaries <- dictionary_keys |>
#   (\(x) setNames(x, x))() |>
#   sapply(
#     \(x) readRDS(file.path(data_dir, sprintf("%s-dictionary.rds", x))) |> select(-variables),
#     simplify = FALSE
#   )


ui <- fluidPage(
  titlePanel("SPHS dictionary explorer"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "dictionary-select",
        label = "Questionaire",
        choices = dictionary_keys,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      selectizeInput(
        "section-select",
        label = "Section",
        choices = combined_dictionary |>
          pull(section) |>
          as.character() |>
          unique() |>
          na.exclude(),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      selectizeInput(
        "subsection-select",
        label = "Subsection",
        choices = combined_dictionary |>
          pull(subsection) |>
          as.character() |>
          unique() |>
          na.exclude(),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      downloadButton("download", "Download")
    ),
    mainPanel(
      dataTableOutput("dictionary")
      # , verbatimTextOutput("out")
    )
  )
)


server <- function(input, output, session) {
  questionaire_dictionary <- reactive({
    df <- combined_dictionary

    if (length(input$`dictionary-select`)) {
      df |>
        filter(questionaire %in% input$`dictionary-select`)
    } else df
  })

  section_dictionary <- reactive({
    sections <- input$`section-select`
    input$`dictionary-select`

    isolate({
      df <- questionaire_dictionary()

      if (length(input$`section-select`)) {
        df |>
          filter(section %in% sections)
      } else df
    })
  })

  subsection_dictionary <- reactive({
    subsections <- input$`subsection-select`
    input$`section-select`

    isolate({
      df <- section_dictionary()

      if (length(input$`subsection-select`)) {
        df |>
          filter(subsection %in% subsections)
      } else df
    })
  })

  dictionary <- reactive({
    subsection_dictionary() |> select(-variables) |> rename_all(tools::toTitleCase)
  })

  proxy <- dataTableProxy("dictionary")

  observe({
    df <- dictionary()

    isolate({
      replaceData(proxy, df)
      selectRows(proxy, which(df$id %in% values$selected))
    })
  })

  values <- reactiveValues(
    selected = character()
  )

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

  # output$out <- renderPrint(names(dictionaries))

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
}


shinyApp(ui = ui, server = server)
