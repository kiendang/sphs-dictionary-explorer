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
  values <- reactiveValues(
    filters = list(),
    selected = character()
  )

  observe({
    includes <- input$`dictionary-select`

    isolate({
      values$filters$questionaire <- includes
    })
  })

  observe({
    includes <- input$`section-select`

    isolate({
      values$filters$section <- includes
    })
  })

  observe({
    includes <- input$`subsection-select`

    isolate({
      values$filters$subsection <- includes
    })
  })

  dictionary <- reactive({
    combined_dictionary |> select(-variables) |> evaluate_filter(values$filters)
  })

  proxy <- dataTableProxy("dictionary")

  observe({
    df <- dictionary()

    isolate({
      replaceData(proxy, df)
      selectRows(proxy, which(df$id %in% values$selected))
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
}


shinyApp(ui = ui, server = server)
