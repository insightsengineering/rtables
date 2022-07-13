#' Display an \code{\link{rtable}} object in the Viewer pane in RStudio or in a
#' browser
#'
#' The table will be displayed using the bootstrap styling for tables.
#'
#' @param x object of class \code{rtable} or \code{shiny.tag} (defined in \code{htmltools})
#' @param y optional second argument of same type as \code{x}
#' @param row.names.bold row.names.bold boolean, make rownames bold
#' @param ... arguments passed to \code{as_html}
#'
#'
#' @export
#'
#' @return not meaningful. Called for the side effect of opening a browser or viewer pane.
#'
#' @examples
#'
#' if(interactive()) {
#' sl5 <- factor(iris$Sepal.Length > 5, levels = c(TRUE, FALSE),
#'    labels = c("S.L > 5", "S.L <= 5"))
#'
#' df <- cbind(iris, sl5 = sl5)
#'
#' tbl <- basic_table() %>%
#'    split_cols_by("sl5") %>%
#'    analyze("Sepal.Length") %>%
#'    build_table(df)
#'
#' Viewer(tbl)
#' Viewer(tbl, tbl)
#'
#'
#' tbl2 <-htmltools::tags$div(
#'   class = "table-responsive",
#'   as_html(tbl, class_table = "table")
#' )
#'
#' Viewer(tbl, tbl2)
#'
#' }
Viewer <- function(x, y = NULL, row.names.bold = FALSE, ...) {

  check_convert <- function(x, name, accept_NULL = FALSE) {
    if (accept_NULL && is.null(x)) {
      NULL
    } else if (is(x, "shiny.tag")) {
      x
    } else if (is(x, "VTableTree")) {
      as_html(x, ...)
    } else {
      stop("object of class rtable or shiny tag excepted for ", name)
    }
  }

  x_tag <- check_convert(x, "x", FALSE)
  y_tag <- check_convert(y, "y", TRUE)

  html_output <- if (is.null(y)) {
    x_tag
  } else {
    htmltools::tags$div(class = "container-fluid",  htmltools::tags$div(class = "row",
      htmltools::tags$div(class= "col-xs-6", x_tag),
      htmltools::tags$div(class= "col-xs-6", y_tag)
    ))
  }

  sandbox_folder <- file.path(tempdir(), "rtable")

  if (!dir.exists(sandbox_folder)) {
    dir.create(sandbox_folder, recursive = TRUE)
    pbs <- file.path(path.package(package = "rtables"), "bootstrap/")
    file.copy(list.files(pbs, full.names = TRUE, recursive = FALSE), sandbox_folder, recursive = TRUE)
    # list.files(sandbox_folder)
  }

  # get html name
  n_try <- 10000
  for (i in seq_len(n_try)) {
    htmlFile <- file.path(sandbox_folder, paste0("table", i, ".html"))

    if (!file.exists(htmlFile)) {
      break
    } else if (i == n_try) {
      stop("too many html rtables created, restart your session")
    }
  }

  html_bs <- tags$html(
    lang="en",
    tags$head(
      tags$meta(charset="utf-8"),
      tags$meta("http-equiv"="X-UA-Compatible", content="IE=edge"),
      tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
      tags$title("rtable"),
      tags$link(href="css/bootstrap.min.css", rel="stylesheet")
    ),
    tags$body(
      html_output
    )
  )

  cat(
    paste("<!DOCTYPE html>\n",  htmltools::doRenderTags(html_bs)),
    file = htmlFile, append = FALSE
  )

  viewer <- getOption("viewer")

  if (!is.null(viewer)) {
    viewer(htmlFile)
  } else {
    utils::browseURL(htmlFile)
  }

}

