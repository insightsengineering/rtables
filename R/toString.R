#' @export
toString.rtable <- function(x, gap = 8, indent.unit = 2, ...) {
  
  nchar_rownames <- max(vapply(x, function(row) {
    rn <- attr(row, "row.name")
    if (is.null(rn)) {
      0
    } else {
      nchar(rn) +  attr(row, "indent") * indent.unit
    }
  }, numeric(1)))
  
  header_row <- do.call(rrow, as.list(c(row.name="", format = "xx", names(x))))
  
  nchar_col <- ceiling(max(unlist(lapply(c(list(header_row), x), function(row) {
    lapply(row, function(cell) {
      nc <- nchar(unlist(strsplit(format_rcell(cell, output = "ascii"), "\n", fixed = TRUE)))
      nc[is.na(nc)] <- 0
      nc / attr(cell, "colspan")
    })
  }))))
  
  
  txt_header <- row_to_str(header_row, nchar_rownames, nchar_col, gap, indent.unit)
  
  txt_cells <- lapply(x, function(row) {
    row_to_str(row, nchar_rownames, nchar_col, gap, indent.unit)
  })
  
  
  paste(
    c(
      txt_header,
      paste(rep("-", nchar_rownames + ncol(x)*(nchar_col + gap)), collapse = ""),
      txt_cells
    ),
    collapse = "\n"
  )  
}


#' @export
print.rtable <- function(x, ...) {
  
  str <- toString.rtable(x, ...)
  
  cat(str)
  cat("\n")
}

row_to_str <- function(row, nchar_rownames, nchar_col, gap, indent.unit) {
  
  row.name <-  if (is.null(attr(row, "row.name"))) {
    ""
  } else {
    paste(c(rep(" ",  attr(row, "indent")*indent.unit),
            attr(row, "row.name")), collapse = "")    
  }
  
  if (length(row) == 0) {
    row.name
  } else {
    cells <- lapply(row, function(cell) {
      unlist(strsplit(format_rcell(cell, output = "ascii"), "\n", fixed = TRUE))
    })
    
    nlines <- max(vapply(cells,length, numeric(1)))
    
    cells_same_length <- lapply(cells, function(x) {
      if (length(x) == nlines) {
        x
      } else {
        c(x, rep(NA_character_, nlines -length(x)))
      }
    })
    
    colspans <- vapply(row, function(cell) attr(cell, "colspan"), numeric(1))
    lines <- as.matrix(Reduce(cbind, cells_same_length))
    
    row_char <- paste0(
      padstr(row.name, nchar_rownames, "left"),
      paste(unlist(Map(function(cell, colspan) {
        list(spaces(gap), padstr(cell, colspan*nchar_col+ (colspan-1)*gap))
      }, lines[1,], colspans)), collapse = "")
    )
    
    if (nrow(lines) > 1) {
      additional_rows <- apply(lines[-1, , drop=FALSE], 1, function(r) {
        paste0(
          spaces(nchar_rownames),
          paste(unlist(Map(function(cell, colspan) {
            list(spaces(gap), padstr(cell, colspan*nchar_col + (colspan-1)*gap))
          }, r, colspans)), collapse = "")
        )
      })
      row_char <- paste(c(row_char, additional_rows), collapse = "\n")
    }
    row_char
  } 
}


padstr <- function(x, n, just = c("center", "left", "right")) {
  
  just <- match.arg(just)
  
  if (length(x) != 1) stop("length of x needs to be 1 and not", length(x))
  if (is.na(n) || !is.numeric(n) || n < 0) stop("n needs to be numeric and > 0")
  
  if (is.na(x)) x <- ""
  
  nc <- nchar(x)
  
  if (n < nc) stop(x, " has more than ", n, " characters")
  
  switch(
    just,
    center = {
      pad <- (n-nc)/2  
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    left = paste0(x, spaces(n-nc)),
    right = paste0(spaces(n-nc), x)
  )
}

spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}
