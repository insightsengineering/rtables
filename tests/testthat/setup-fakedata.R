## Loading relevant libraries for tests
require(tibble, quietly = TRUE)
require(dplyr, quietly = TRUE)

# # Load and flag for pdftools to check for it
# check_pdf <- require(pdftools)


## eat the one-time warning
suppressWarnings(formatters::default_hsep())
makefakedat <- function(n = 1000) {
  datadf <- data.frame(
    stringsAsFactors = FALSE,
    ARM = c("ARM1", sample(c("ARM1", "ARM2"), n - 1, replace = TRUE)),
    SEX = c("M", sample(c("M", "F"), n - 1, replace = TRUE)),
    FACTOR2 = c("A", sample(c("A", "B", "C"), n - 1, replace = TRUE)),
    RACE = c("WHITE", sample(c("WHITE", "BLACK"), n - 1, replace = TRUE)),
    AGE = runif(n, 40, 70),
    VAR3 = c("level1", sample(c("level1", "level2"), n - 1,
      replace = TRUE
    ))
  )

  datadf$ethn_label <- c(WHITE = "Caucasian", BLACK = "African American")[datadf$RACE]
  datadf$fac2_label <- paste("Level", datadf$FACTOR2)
  datadf$gend_label <- c(M = "Male", F = "Female")[datadf$SEX]
  datadf
}



makefakedat2 <- function(n = 1000) {
  if (n %% 4 != 0) {
    stop("n not multiple of 4")
  }

  many2s <- rep(2, n / 2)
  datadf <- data.frame(
    stringsAsFactors = FALSE,
    ARM = rep(c("ARM1", "ARM2"), times = rep(n / 2, 2)),
    SEX = rep(
      sample(c("M", "F"), n / 2, replace = TRUE),
      many2s
    ),
    RACE = rep(sample(c("WHITE", "BLACK"), n / 2, replace = TRUE),
      times = many2s
    ),
    PATID = rep(seq(1, n / 2), many2s),
    VISIT = rep(c("BASELINE", "FOLLOWUP"))
  )
  datadf$ethn_label <- c(WHITE = "Caucasian", BLACK = "African American")[datadf$RACE]
  datadf$gend_label <- c(M = "Male", F = "Female")[datadf$SEX]
  mu <- 5 + (as.integer(factor(datadf$RACE)) + as.integer(factor(datadf$ARM)) + as.integer(factor(datadf$SEX))) / 2
  datadf$VALUE <- ifelse(
    datadf$VISIT == "BASELINE",
    5,
    5 + rnorm(n, mu, 4)
  )
  datadf$PCTDIFF <- NA_real_
  seconds <- seq(2, n, by = 2)
  datadf$PCTDIFF[seq(2, n, by = 2)] <- 100 * (datadf$VALUE[seconds] - datadf$VALUE[seconds - 1]) /
    datadf$VALUE[seconds - 1] # nolint

  datadf
}
set.seed(0)
rawdat <- makefakedat()
rawdat2 <- makefakedat2()

## used in multiple test files
refcompmean <- function(x, .ref_group, .in_ref_col, ...) {
  if (.in_ref_col) {
    val <- rcell(NULL)
  } else {
    val <- rcell(mean(x, ...) - mean(.ref_group, ...), format = "xx.xx")
  }

  in_rows(
    "Diff from reference - mean" = val
  )
}

complx_lyt_rnames <- c(
  "Caucasian (n)", "Level A", "Age Analysis", "mean", "median",
  "Age Analysis redux", "range", "Level B", "Age Analysis",
  "mean", "median", "Age Analysis redux", "range",
  "African American (n)", "Level A", "Age Analysis", "mean", "median",
  "Age Analysis redux", "range", "Level B", "Age Analysis",
  "mean", "median", "Age Analysis redux", "range",
  "level1", "level2"
)


make_big_lyt <- function() {
  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    ## add nested column split on SEX with value lables from gend_label
    split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
    ## No row splits have been introduced, so this adds
    ## a root split and puts summary content on it labelled Overall (N)
    ## add_colby_total(label = "All") %>%
    ##    summarize_row_groups(label = "Overall (N)", format = "(N=xx)") %>%
    ## add a new subtable that splits on RACE, value labels from ethn_label
    split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", label_pos = "hidden") %>%
    summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
    ##
    ## Add nested row split within Race categories for FACTOR2
    ## using a split function that excludes level C
    ## value labels from fac2_label
    split_rows_by("FACTOR2", "Factor2",
      split_fun = remove_split_levels("C"),
      labels_var = "fac2_label",
      label_pos = "hidden"
    ) %>%
    ## Add count summary within FACTOR2 categories
    summarize_row_groups("FACTOR2") %>%
    ## Add analysis/data rows by analyzing AGE variable
    ## Note afun is a function that returns 2 values in a named list
    ## this will create 2 data rows
    analyze(
      "AGE", "Age Analysis",
      afun = function(x) list(mean = mean(x), median = median(x)),
      format = "xx.xx"
    ) %>%
    ## adding more analysis vars "compounds them", placing them at the same
    ## level of nesting as all previous analysis blocks, rather than
    ## attempting to further nest them
    analyze("AGE",
      "Age Analysis redux",
      afun = range,
      format = "xx.x - xx.x",
      table_names = "AgeRedux"
    ) %>%
    ## Note nested=FALSE, this creates a NEW subtable directly under the
    ## root split
    ## afun of table() gives us k count rows, where k is the number of
    ## levels of VAR3, in this case 2.
    analyze("VAR3", "Var3 Counts", afun = list_wrap_x(table), nested = FALSE)
  lyt
}

export_fact <- function() {
  tbl2 <- NULL
  function() {
    if (is.null(tbl2)) {
      lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", split_fun = keep_split_levels(c("M", "F"))) %>%
        split_rows_by("STRATA1") %>%
        summarize_row_groups() %>%
        split_rows_by("RACE", split_fun = keep_split_levels(c("WHITE", "ASIAN"))) %>%
        analyze(c("AGE", "BMRKR2", "COUNTRY"))

      tbl2 <<- build_table(lyt, ex_adsl)
    }
    tbl2
  }
}

tt_to_export <- export_fact()

# Creating data-set with wide content to test wrapping
tt_to_test_wrapping <- function() {
  trimmed_data <- ex_adsl[(ex_adsl$SEX %in% c("M", "F")) & c(ex_adsl$RACE %in% levels(ex_adsl$RACE)[1:2]), ]

  levels(trimmed_data$ARM)[1] <- "Incredibly long column name to be wrapped"
  levels(trimmed_data$ARM)[2] <- "This_should_be_somewhere_split"

  basic_table(
    title = "Enough long title to be probably wider than expected",
    main_footer = "Also this seems quite wider than expected initially."
  ) %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = drop_split_levels) %>%
    analyze(c("AGE", "EOSDY"),
      na_str = "A very long content to_be_wrapped_and_splitted",
      inclNAs = TRUE
    ) %>%
    build_table(trimmed_data)
}

tt_for_wrap <- tt_to_test_wrapping()

tt_to_test_newline_chars <- function() {
  set.seed(1)
  DM_trick <- DM
  DM_trick$ARM <- "ARM \n\nA\n"
  DM_trick$ARM2 <- sample(c("TWO\nwords\n ", "A wo\n\nrd\n\n"),
    replace = TRUE, nrow(DM)
  ) # last \n is eaten up if no empty space
  levels(DM_trick$SEX)[3] <- "U\nN\nD\n"
  tbl <- basic_table() %>%
    split_rows_by("SEX",
      split_label = "m\nannaggia\nsda\n",
      label_pos = "visible"
    ) %>%
    split_cols_by("ARM") %>%
    split_cols_by("ARM2", split_label = "sda") %>%
    analyze("BMRKR1", na_str = "asd\nasd") %>%
    build_table(DM_trick)

  main_footer(tbl) <- c("main_footer: This", "is\na\n\nweird one\n")
  prov_footer(tbl) <- c("prov_footer: This", "is\na\n\nweird one\n")
  fnotes_at_path(tbl, rowpath = row_paths(tbl)[[6]]) <- c("a fancy footnote\ncrazy\n", "ahahha")
  top_left(tbl) <- c("\na", "b\nd\n\n", "c\n\n") # last \n is eaten up if empty line everywhere
  main_title(tbl) <- "why not\nalso here\n"
  tbl
}

tt_for_nl <- tt_to_test_newline_chars()

# Helper function in R base to count how many times a character appears in a string.
# W: this works only for counting single characters from a single string of txt
.count_chr_from_str <- function(str, chr, negate = FALSE) {
  if (negate) {
    nchar(gsub(chr, "", str, fixed = TRUE))
  } else {
    nchar(str) - nchar(gsub(chr, "", str, fixed = TRUE))
  }
}

# Utility function for section_div tests
check_pattern <- function(element, letter, len) {
  # Regular expression to match exactly len of the same letter
  regex <- paste0(rep(letter, len), collapse = "")
  return(grepl(regex, element, fixed = TRUE))
}

check_all_patterns <- function(elements, letters, len) {
  res <- mapply(check_pattern,
                element = elements,
                letter = letters,
                MoreArgs = list(len = len))
  all(res)
}
