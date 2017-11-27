#' Dispaly an \code{\link{rtable}} object in the Viewer pane in RStudio or in a
#' browser
#' 
#' The table will be displayed using the bootstrap styling for tables.
#' 
#' @inheritParams dim.rtable
#' @param y optional second rtable object
#' @param row.names.bold row.names.bold boolean, make rownames bold
#' 
#' 
#' @export
Viewer <- function(x, y = NULL, row.names.bold = FALSE) {
  
  if (!is(x, "rtable")) stop("x is expected to be an rtable")
  if (!is.null(y) && !is(y, "rtable")) stop("y is expected to be an rtable if specified")
  
  viewer <- getOption("viewer")
  
  tbl_html <- if (is.null(y)) {
    as_html(x)
  } else {
    htmltools::tags$div(
      class = ".container-fluid",
      htmltools::tags$div(
        class= "col-xs-6",
        as_html(x)
      ),
      htmltools::tags$div(
        class= "col-xs-6",
        as_html(y)
      )
    )
  }
  
  sandbox_folder <- file.path(tempdir(), "rtable")
  
  if (!dir.exists(sandbox_folder)) {
    dir.create(sandbox_folder, recursive = TRUE)
    pbs <- file.path(path.package(package = "rtables"), "bootstrap/")
    file.copy(list.files(pbs, full.names = TRUE, recursive = FALSE), sandbox_folder, recursive = TRUE)
    list.files(sandbox_folder)
  }
  
  # get html name
  for (i in 1:10000) {
    htmlFile <- file.path(sandbox_folder, paste0("table", i, ".html"))
    
    if (!file.exists(htmlFile)) {
      break
    } else if (i == 10000) {
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
      tbl_html
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

