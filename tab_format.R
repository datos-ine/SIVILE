### Función para formato de tablas interactivas
### Autora: Tamara Ricardo
### Fecha:
# 2025-09-25 10:49:16

tab_format <- function(x) {
  library(DT)
  x |>
    rename(
      Frecuencia = n,
      Porcentaje = pct
    ) |>

    datatable(
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      escape = FALSE,
      options = list(
        searching = FALSE,
        info = FALSE,
        paging = FALSE,
        autoWidth = FALSE,
        dom = 'Bftlp',
        buttons = list(
          list(
            extend = "excel",
            text = as.character(bs_icon("file-earmark-spreadsheet")),
            title = paste0("tab_", Sys.Date())
          ),
          list(
            extend = "csv",
            text = as.character(bs_icon("filetype-csv")),
            title = paste0("tab_", Sys.Date())
          )
        )
      )
    ) |>

    formatPercentage(columns = "Porcentaje", digits = 2)
}
