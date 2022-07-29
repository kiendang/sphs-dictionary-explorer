library(tidyverse)


data_dir <- "data"


process_data <- function(df) {
  df <- df |>
    rename_with(tolower) |>
    select(section, subsection, no, definition, variables) |>
    fill(no) |>
    group_by(no) |>
    fill(variables) |>
    ungroup() |>
    rename(definition = definition) |>
    mutate(definition = trimws(definition))


  variable_descriptions <- df |>
    filter(!is.na(variables)) |>
    group_by(section, subsection, no, variables) |>
    summarise(definition = paste(definition, collapse = "\n"), .groups = "drop")


  chunks <- df |>
    group_by(section, subsection, no) |>
    filter(length(unique(variables[!is.na(variables)])) > 1L)


  chunk_variables <- chunks |>
    filter(!is.na(variables)) |>
    select(section, subsection, no, variables) |>
    distinct()


  chunk_description <- chunks |>
    group_by(section, subsection, no) |>
    summarise(definition = first(definition), .groups = "drop")


  results <- chunk_variables |>
    left_join(variable_descriptions) |>
    rename(name = variables) |>
    group_by(section, subsection, no) |>
    nest() |>
    rename(variables = data) |>
    left_join(chunk_description) |>
    rename(name = no) |>
    full_join(
      variable_descriptions |>
        select(section, subsection, variables, definition) |>
        rename(name = variables)
    ) |>
    select(section, subsection, name, definition, variables) |>
    ungroup()
}


files <- list(
  main = list(file.path(data_dir, "main-dictionary.xlsx"), sheet = 1),
  revisit = list(file.path(data_dir, "revisit-dictionary.xlsx"), sheet = 1),
  screening = list(file.path(data_dir, "revisit-dictionary.xlsx"), sheet = 2)
)


dictionaries <- files |>
  map(function(args) exec(readxl::read_excel, !!!args)) |>
  map(process_data)


combined_dictionary <- dictionaries |>
  imap(\(df, i) df |> mutate(questionaire = i, .before = everything())) |>
  (\(dfs) exec(bind_rows, !!!dfs))() |>
  mutate_at(vars(all_of(c("section", "subsection"))), factor)


combined_dictionary |> saveRDS(file.path(data_dir, "dictionary.rds"))


dictionaries |> iwalk(function(df, i) {
  df |>
    mutate_at(vars(all_of(c("section", "subsection"))), factor) |>
    saveRDS(file.path(data_dir, sprintf("%s-dictionary.rds", i)))
})


dictionaries |> iwalk(function(df, i) {
  df |> jsonlite::write_json(
    file.path(data_dir, sprintf("%s-dictionary.json", i)), null = "list"
  )
})
