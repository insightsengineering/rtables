# Pre-processing main table used for testing
adsl <- ex_adsl
levels(adsl$SEX) <- c(levels(ex_adsl$SEX), "OTHER")

lyt <- basic_table(title = "A",
                   subtitles = letters[1:2],
                   show_colcounts = TRUE,
                   main_footer = LETTERS[7:6],
                   prov_footer = LETTERS[4:5]) %>%
    split_cols_by("ARM") %>% 
    split_rows_by("SEX") %>% 
    summarize_row_groups() %>%
    split_rows_by("STRATA1") %>%
    summarize_row_groups() %>%
    analyze("AGE") %>%
    append_topleft(c("Some", "    thing"))

tbl_to_prune <- lyt %>% 
    build_table(adsl[-c(1:400),])

fnotes_at_path(tbl_to_prune, rowpath = c("F")) <- "agoraphobia"
fnotes_at_path(tbl_to_prune, rowpath = NULL,
               colpath = c("ARM", "A: Drug X")) <- "talassophobia"


test_that("prune_table produces a degenerate table with decoration 
          if used on empty tbl", {

pruned_tbl <- tbl_to_prune %>% prune_table

expect_identical(top_left(pruned_tbl), top_left(tbl_to_prune))
expect_identical(names(pruned_tbl), names(tbl_to_prune))
expect_identical(main_title(pruned_tbl), main_title(tbl_to_prune))
expect_identical(subtitles(pruned_tbl), subtitles(tbl_to_prune))
expect_identical(main_footer(pruned_tbl), main_footer(tbl_to_prune))
expect_identical(prov_footer(pruned_tbl), prov_footer(tbl_to_prune))
expect_equivalent(matrix_form(pruned_tbl)$ref_footnotes, character(0))
})

test_that("degenerate table can keep information if subset", {
    pruned_tbl <- tbl_to_prune %>% prune_table
    subset_tbl <- tbl_to_prune[-c(1:nrow(tbl_to_prune)), ,
                 keep_topleft = TRUE, 
                 keep_titles = TRUE, 
                 keep_footers = TRUE]
    
    expect_identical(pruned_tbl, subset_tbl) 
})
