library(shiny.semantic)
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
  mutate(id = 1:n(), .before = everything())


# dictionaries <- dictionary_keys |>
#   (\(x) setNames(x, x))() |>
#   sapply(
#     \(x) readRDS(file.path(data_dir, sprintf("%s-dictionary.rds", x))) |> select(-variables),
#     simplify = FALSE
#   )


ui <- semanticPage(
  title = "SPHS dictionary explorer",
  sidebar_layout(
    sidebar_panel(
      dropdown_input(
        "dictionary-select",
        names(dictionary_keys),
        value = first(names(dictionary_keys))
      ),
      button("download-button", "",
             downloadLink("download", "Download"))
    ),
    main_panel(
      cards(
        class = "one",
        card(
          class = "blue",
          div(
            class = "content",
            dataTableOutput("dictionary")
          )
        )
      )
      , cards(
        class = "one",
        card(
          class = "blue",
          div(
            class = "content",
            verbatimTextOutput("out")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  dictionary_key <- reactive(dictionary_keys[[input$`dictionary-select`]])
  dictionary <- reactive(
    combined_dictionary |>
      filter(questionaire == dictionary_key()) |>
      select(-variables)
  )

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

  output$out <- renderPrint(names(dictionaries))

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
        filter = "bottom",
        options = list(
          columnDefs = list(
            list(targets = c(0:(ncol(df) - 1)), className = "dt-head-left")
            # list(targets = 4, visible = FALSE)
          )
        ),
        selection = list(selected = which(df$name %in% values$selected))
      )
    })
  })
}


shinyApp(ui = ui, server = server)
