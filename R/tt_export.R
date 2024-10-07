#' @importFrom tools file_ext
NULL

#' Create enriched flat value table with paths
#'
#' This function creates a flat tabular file of cell values and corresponding paths via [path_enriched_df()]. It then
#' writes that data frame out as a `tsv` file.
#'
#' By default (i.e. when `value_func` is not specified, list columns where at least one value has length > 1 are
#' collapsed to character vectors by collapsing the list element with `"|"`.
#'
#' @note
#' There is currently no round-trip capability for this type of export. You can read values exported this way back in
#' via `import_from_tsv` but you will receive only the `data.frame` version back, NOT a `TableTree`.
#'
#' @inheritParams gen_args
#' @inheritParams data.frame_export
#' @param file (`string`)\cr the path of the file to written to or read from.
#' @param sep (`string`)\cr defaults to `\t`. See [utils::write.table()] for more details.
#' @param ... (`any`)\cr additional arguments to be passed to [utils::write.table()].
#'
#' @return
#' * `export_as_tsv` returns `NULL` silently.
#' * `import_from_tsv` returns a `data.frame` with re-constituted list values.
#'
#' @seealso [path_enriched_df()] for the underlying function that does the work.
#'
#' @importFrom utils write.table read.table
#' @rdname tsv_io
#' @export
export_as_tsv <- function(tt, file = NULL, path_fun = collapse_path,
                          value_fun = collapse_values, sep = "\t", ...) {
  df <- path_enriched_df(tt, path_fun = path_fun, value_fun = value_fun)
  write.table(df, file, sep = sep, ...)
}

#' @rdname tsv_io
#' @export
import_from_tsv <- function(file) {
  rawdf <- read.table(file, header = TRUE, sep = "\t")
  as.data.frame(lapply(
    rawdf,
    function(col) {
      if (!any(grepl(.collapse_char, col, fixed = TRUE))) {
        col
      } else {
        I(strsplit(col, split = .collapse_char_esc))
      }
    }
  ))
}
# txt (formatters) --------------------------------------------------------------------
#' @importFrom formatters export_as_txt
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' cat(export_as_txt(tbl, file = NULL, paginate = TRUE, lpp = 8))
#'
#' \dontrun{
#' tf <- tempfile(fileext = ".txt")
#' export_as_txt(tbl, file = tf)
#' system2("cat", tf)
#' }
#'
#' @export
formatters::export_as_txt

# pdf (formatters) ----------------------------------------------------------
#' @importFrom formatters export_as_pdf
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' \dontrun{
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(tbl, file = tf, pg_height = 4)
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(tbl, file = tf, lpp = 8)
#' }
#'
#' @export
formatters::export_as_pdf

# docx (flextable) -----------------------------------------------------------
#' Export as word document
#'
#' From a table, produce a self-contained word document or attach it to a template word
#' file (`template_file`). This function is based on the [tt_to_flextable()] transformer and
#' the `officer` package.
#'
#' @inheritParams gen_args
#' @param file (`string`)\cr string that indicates the final file output. Must have `.docx` extension.
#' @param doc_metadata (`list` of `string`s)\cr any value that can be used as metadata by
#'   `?officer::set_doc_properties`. Important text values are `title`, `subject`, `creator`, and `description`,
#'   while `created` is a date object.
#' @inheritParams tt_to_flextable
#' @param template_file (`string`)\cr template file that `officer` will use as a starting point for the final
#'   document. Document attaches the table and uses the defaults defined in the template file.
#' @param section_properties (`officer::prop_section`)\cr an [officer::prop_section()] object which sets margins and
#'   page size. Defaults to `section_properties_default()`.
#' @param ... (`any`)\cr additional arguments passed to [tt_to_flextable()].
#'
#' @note `export_as_docx()` has few customization options available. If you require specific formats and details,
#'   we suggest that you use [tt_to_flextable()] prior to `export_as_docx`. Only the `titles_as_header` and
#'   `footer_as_text` parameters must be re-specified if the table is changed first using [tt_to_flextable()].
#'
#' @seealso [tt_to_flextable()]
#'
#' @examplesIf require(flextable)
#' library(flextable)
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' # See how section_properties_portrait function is built for custom
#' tf <- tempfile(fileext = ".docx")
#' export_as_docx(tbl,
#'   file = tf,
#'   section_properties = section_properties_default(orientation = "landscape")
#' )
#'
#' @export
export_as_docx <- function(tt,
                           file,
                           doc_metadata = NULL,
                           titles_as_header = FALSE,
                           footers_as_text = TRUE,
                           template_file = NULL,
                           section_properties = section_properties_default(),
                           ...) {
  # Checks
  check_required_packages(c("flextable", "officer"))
  if (inherits(tt, "VTableTree")) {
    flex_tbl <- tt_to_flextable(tt,
      titles_as_header = titles_as_header,
      footers_as_text = footers_as_text,
      ...
    )
    if (isFALSE(titles_as_header) || isTRUE(footers_as_text)) {
      # Ugly but I could not find a getter for font.size
      font_sz_body <- flex_tbl$header$styles$text$font.size$data[1, 1]
      font_size_footer <- flex_tbl$footer$styles$text$font.size$data
      font_sz_footer <- if (length(font_size_footer) > 0) {
        font_size_footer[1, 1]
      } else {
        font_sz_body - 1
      }
      font_fam <- flex_tbl$header$styles$text$font.family$data[1, 1]

      # Set the test as the tt
      fpt <- officer::fp_text(font.family = font_fam, font.size = font_sz_body)
      fpt_footer <- officer::fp_text(font.family = font_fam, font.size = font_sz_footer)
    }
  } else if (inherits(tt, "flextable")) {
    flex_tbl <- tt
  } else if (inherits(tt, "list")) {
    export_as_docx(tt[[1]], # First paginated table that uses template_file
      file = file,
      doc_metadata = doc_metadata,
      titles_as_header = titles_as_header,
      footers_as_text = footers_as_text,
      template_file = template_file,
      section_properties = section_properties,
      ...
    )
    if (length(tt) > 1) {
      out <- mapply(
        export_as_docx,
        tt = tt[-1], # Remaining paginated tables
        MoreArgs = list(
          file = file,
          doc_metadata = doc_metadata,
          titles_as_header = titles_as_header,
          footers_as_text = footers_as_text,
          template_file = file, # Uses the just-created file as template
          section_properties = section_properties,
          ...
        )
      )
    }
    return()
  } else {
    stop("The table must be a VTableTree, a flextable, or a list of VTableTree or flextable objects.")
  }
  if (!is.null(template_file) && !file.exists(template_file)) {
    template_file <- NULL
  }

  # Create a new empty Word document
  if (!is.null(template_file)) {
    doc <- officer::read_docx(template_file)
  } else {
    doc <- officer::read_docx()
  }

  # page width and orientation settings
  doc <- officer::body_set_default_section(doc, section_properties)
  if (flex_tbl$properties$layout != "autofit") { # fixed layout
    page_width <- section_properties$page_size$width
    dflx <- dim(flex_tbl)
    if (abs(sum(unname(dflx$widths)) - page_width) > 1e-2) {
      warning(
        "The total table width does not match the page width. The column widths",
        " will be resized to fit the page. Please consider modifying the parameter",
        " total_page_width in tt_to_flextable()."
      )

      final_cwidths <- page_width * unname(dflx$widths) / sum(unname(dflx$widths))
      flex_tbl <- flextable::width(flex_tbl, width = final_cwidths)
    }
  }

  # Extract title
  if (isFALSE(titles_as_header) && inherits(tt, "VTableTree")) {
    ts_tbl <- all_titles(tt)
    if (length(ts_tbl) > 0) {
      doc <- add_text_par(doc, ts_tbl, fpt)
    }
  }

  # Add the table to the document
  doc <- flextable::body_add_flextable(doc, flex_tbl, align = "left")

  # add footers as paragraphs
  if (isTRUE(footers_as_text) && inherits(tt, "VTableTree")) {
    # Adding referential footer line separator if present
    # (this is usually done differently, i.e. inside footnotes)
    matform <- matrix_form(tt, indent_rownames = TRUE)
    if (length(matform$ref_footnotes) > 0) {
      doc <- add_text_par(doc, matform$ref_footnotes, fpt_footer)
    }
    # Footer lines
    if (length(all_footers(tt)) > 0) {
      doc <- add_text_par(doc, all_footers(tt), fpt_footer)
    }
  }

  if (!is.null(doc_metadata)) {
    # Checks for values rely on officer function
    doc <- do.call(officer::set_doc_properties, c(list("x" = doc), doc_metadata))
  }

  # Save the Word document to a file
  print(doc, target = file)

  invisible(TRUE)
}

# Shorthand to add text paragraph
add_text_par <- function(doc, chr_v, text_format) {
  for (ii in seq_along(chr_v)) {
    cur_fp <- officer::fpar(officer::ftext(chr_v[ii], prop = text_format))
    doc <- officer::body_add_fpar(doc, cur_fp)
  }
  doc
}

#' @describeIn export_as_docx Helper function that defines standard portrait properties for tables.
#' @param page_size (`character(1)`) page size. Can be `"letter"` or `"A4"`. Defaults to `"letter"`.
#' @param orientation (`character(1)`) page orientation. Can be `"portrait"` or `"landscape"`. Defaults to
#'   `"portrait"`.
#'
#' @export
section_properties_default <- function(page_size = c("letter", "A4"),
                                       orientation = c("portrait", "landscape")) {
  page_size <- page_size[1]
  orientation <- orientation[1]
  checkmate::assert_choice(
    page_size,
    eval(formals(section_properties_default)$page_size)
  )
  checkmate::assert_choice(
    orientation,
    eval(formals(section_properties_default)$orientation)
  )

  if (page_size == "letter") {
    page_size <- officer::page_size(
      orient = orientation,
      width = 8.5, height = 11
    )
  } else { # A4
    page_size <- officer::page_size(
      orient = orientation,
      width = 8.27, height = 11.69
    )
  }

  # Final output
  officer::prop_section(
    page_size = page_size,
    type = "continuous",
    page_margins = margins_potrait()
  )
}

#' @describeIn export_as_docx Helper function that defines standard portrait margins for tables.
#' @export
margins_potrait <- function() {
  officer::page_mar(bottom = 0.98, top = 0.95, left = 1.5, right = 1, gutter = 0)
}
#' @describeIn export_as_docx Helper function that defines standard landscape margins for tables.
#' @export
margins_landscape <- function() {
  officer::page_mar(bottom = 1, top = 1.5, left = 0.98, right = 0.95, gutter = 0)
}
