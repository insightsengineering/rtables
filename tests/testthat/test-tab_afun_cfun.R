context("Analysis functions (afun)")

test_that(".spl_context contains information about the column split", {
    ## Duplication hack # xxx send it to Gabe
    DM_tmp <- DM %>% 
        mutate(method = factor("Mean")) 
    
    DM_tmp <- rbind(DM_tmp, DM_tmp %>% 
                        mutate(method = factor("SD")))
    
    analysis_fun_fin <- function(x, .spl_context, labelstr = "", ...) {
        
        # Very smart internal checks for name reconstruction from path
        stopifnot(length(.spl_context$cur_col_id[[1]]) == 1L)
        stopifnot(.spl_context$cur_col_id[[1]] %in% names(.spl_context))
        
        if (any(.spl_context$cur_col_split_val[[2]] == "SD")) {
            res <- list("SOMETHING" = sd(x))
        } else if (any(.spl_context$cur_col_split_val[[2]] == "Mean")) {
            res <- list("SOMETHING" = mean(x))
        }
            
        in_rows(.list = res)
    }
    
    lyt <- basic_table() %>% 
        split_rows_by("STRATA1") %>%
        split_cols_by(var = "method") %>%
        split_cols_by("SEX", split_fun = drop_split_levels) %>%
        analyze(vars = "BMRKR1", afun = analysis_fun_fin, format = "xx.xxx")
    
    expect_silent(tbl <- lyt %>% build_table(DM_tmp))
    
    tol <- 0.02
    DM_tmp_F <- DM_tmp %>% filter(SEX == "F", STRATA1 == "B")
    expect_equal(tbl[4, 1, drop = TRUE], mean(DM_tmp_F$BMRKR1), tolerance = tol)
    expect_equal(tbl[4, 3, drop = TRUE], sd(DM_tmp_F$BMRKR1), tolerance = tol) # xxx why -0.011?
})

test_that(".spl_context contains col information and multivars works", {
    skip()
    DM_tmp <- DM %>% # xxx file an issue for this 
                     # xxx add documentation for not supported + error
        mutate(SEX = factor(SEX))
    
    lyt <- basic_table() %>% 
        split_rows_by("STRATA1") %>%
        # split_cols_by_multivar(vars = c("BMRKR1", "BMRKR1"), 
        #                        varlabels = c("M", "SD")) %>%
        split_cols_by("ARM") %>% # Breaks
        split_cols_by("SEX") %>% # Breaks
        analyze(vars = c("BMRKR1"), afun = list("Mean" = mean, "Mean" = mean,
                                                "SD" = sd, "SD" = sd), 
                # xxx with 2 should work
                format = "xx.x")
        # analyze(vars = c("BMRKR1", "BMRKR1"), 
        #         afun = list("F" = mean, "SD" = min),
        #         show_labels = "hidden")
    
    lyt %>% build_table(DM_tmp)
})

test_that(".spl_context contains information about combo counts", {
    ## Fix for https://github.com/insightsengineering/rtables/issues/517
    combodf <- tribble(
        ~valname, ~label, ~levelcombo, ~exargs,
        "all_X", "All Drug X", c("A: Drug X", "C: Combination"), list(),
        "all_pt", "All Patients", c("A: Drug X", "B: Placebo", "C: Combination"), list()
    )
    
    n_wrapper_alt_df <- function(alt_counts_df) {
        function(x, .spl_context, .N_col, .alt_counts_df, .all_col_exprs, 
                 .all_col_counts, ...) { 
            if (.spl_context$cur_col_id[[1]] != "all_X") {
                in_rows("n" = .N_col, .formats = "xx")
            } else {
                # Normal execution - no use of cexpr
                alt_df1 <- .alt_counts_df %>% 
                    filter(ARM == "A: Drug X")
                alt_df2 <- .alt_counts_df %>% 
                    filter(ARM == "C: Combination")
                
                # Use of cexpr
                alt_df1c <- .alt_counts_df %>% 
                    filter(eval(.all_col_exprs[["A: Drug X"]]))
                alt_df2c <- .alt_counts_df %>% 
# xxx create convenience function from expr to col subset at certain level
                    filter(eval(.all_col_exprs[["C: Combination"]]))
                
                # Super manual extraction
                alt_df1b <- alt_counts_df %>% 
                    filter(ARM == "A: Drug X") %>% 
                    filter(STRATA1 == .spl_context$value[[2]])
                alt_df2b <- .alt_counts_df %>% 
                    filter(ARM == "C: Combination") %>% 
                    filter(STRATA1 == .spl_context$value[[2]])
                
                # This would break the tests if no match
                stopifnot(nrow(alt_df1) == nrow(alt_df1b))
                stopifnot(nrow(alt_df1) == nrow(alt_df1c))
                stopifnot(nrow(alt_df2) == nrow(alt_df2b))
                stopifnot(nrow(alt_df2) == nrow(alt_df2c))
                
                # General info
                stopifnot(.all_col_counts[["all_X"]] == .N_col)
                stopifnot(.all_col_exprs[["all_X"]] == .spl_context$cur_col_expr[[2]])
                
                # Fin needed output 
                in_rows("n" = c(nrow(alt_df1c), 
                                nrow(alt_df2c)), 
                        .formats = "xx - xx")
            }
        }
    }
    
    lyt <- basic_table(show_colcounts = TRUE) %>% 
        split_cols_by("ARM", split_fun = add_combo_levels(combodf, first = TRUE)) %>%
        split_rows_by("STRATA1", split_fun = drop_split_levels) %>%
        analyze(vars = "BMRKR1", afun = n_wrapper_alt_df(ex_adsl))
    
    # NB: If you add keep_levels = c("all_X") to add_combo_levels the other 
    #     column expressions are missing -> Expected!
    
    tbl <- lyt %>% build_table(DM, alt_counts_df = ex_adsl)
    
    spl_ctx_cnt <- lapply(seq(2, nrow(tbl), 2), function(x) tbl[x, 2, drop = TRUE])
    
    nrow_manual <- lapply(sort(unique(ex_adsl$STRATA1)), function(x) {
        tmp_strata <- ex_adsl %>% filter(STRATA1 == x)
        sapply(list(tmp_strata %>% filter(ARM == "A: Drug X"),
                 tmp_strata %>% filter(ARM == "C: Combination")), nrow)
    })
    
    expect_identical(nrow_manual, spl_ctx_cnt)
})

test_that("Error localization for missing split variable when done in alt_count_df", {
    # xxx - do the same for .ref_group?
    # xxx - trying to merge .if_in_formals, func_takes, .takes_df and match_args?
    
    # Error we want to happen
    afun_tmp <- function(x, .alt_counts_df, ...) mean(x)
    lyt_col <- basic_table() %>% split_cols_by("ARMCD") %>% analyze("BMRKR1", afun = afun_tmp)
    lyt_row <- basic_table() %>% split_rows_by("ARMCD") %>% analyze("BMRKR1", afun = afun_tmp)
    expect_error(lyt_col %>% build_table(ex_adsl, alt_counts_df = DM))
    expect_error(lyt_row %>% build_table(ex_adsl, alt_counts_df = DM), 
                 regexp = paste0("Following error encountered in splitting ",
                 "alt_counts_df:  variable\\(s\\) \\[ARMCD\\] ",
                 "not present in data. \\(VarLevelSplit\\)"))
    
    # Split not requested so no error
    expect_silent(lyt_row %>% build_table(ex_adsl))
    expect_silent(lyt_col %>% build_table(ex_adsl))
    
    # What if it is not asked by the function?
    afun_tmp <- function(x, ...) mean(x)
    lyt_col <- basic_table() %>% 
        split_cols_by("ARMCD") %>% 
        analyze("BMRKR1", afun = afun_tmp)
    lyt_row <- basic_table() %>% 
        split_rows_by("STRATA1", split_fun = keep_split_levels("A")) %>% 
        split_rows_by("ARMCD") %>% 
        analyze("BMRKR1", afun = afun_tmp)
    
    # Error on the columns should happen because it is used for counts on column space
    expect_error(lyt_col %>% build_table(ex_adsl, alt_counts_df = DM))
    expect_silent(lyt_row %>% build_table(ex_adsl, alt_counts_df = DM))
    
})

context("Content functions (cfun)")

test_that(".alt_counts_df appears in cfun but not in afun.", {
    # Adding STRATA2 col to DM for alt_counts_df col split
    alt_tmp <- DM %>% left_join(ex_adsl %>% 
                                    mutate(ID = paste0("S", seq_len(nrow(ex_adsl)))) %>% 
                                    select(ID, STRATA2))
    
    afun_tmp <- function(x, ...) rcell(mean(x), label = "MeAn", format = "xx.x")
    cfun_tmp <- function(x, .alt_counts_df, labelstr, .N_col, 
                         .spl_context,
                         .all_col_exprs,
                         .all_col_counts,
                         ...) {
        if (!missing(.alt_counts_df)) {
            # .alt_counts_df is only row splitted matter
            stopifnot(nrow(alt_tmp %>% filter(STRATA1 == "A")) == nrow(.alt_counts_df))
            
            # Filtered column number of elemens correspond to .N_col
            stopifnot(nrow(alt_tmp %>% 
                               filter(eval(.spl_context$cur_col_expr[1]))) == .N_col)
        } else {
            # Checking cur_col_n is the same as .N_col for root and length(x) for split
            stopifnot(identical(.spl_context$cur_col_n, c(.N_col, length(x))))
        }
        
        # Checking internal names for all column counts correspond to .spl_context
        stopifnot(all(names(.all_col_counts) %in% colnames(.spl_context)))
        
        # Checking that current col id and col counts agree with .N_col
        stopifnot(.all_col_counts[.spl_context$cur_col_id[1]] == .N_col)
        
        # Checking col expression
        stopifnot(identical(.all_col_exprs[.spl_context$cur_col_id[1]][[1]],
                            .spl_context$cur_col_expr[1])) # Uses the root one
        
        in_rows(c(length(x), length(x) / .N_col), 
                .names = labelstr, 
                .formats = c("xx (xx.xx)"))
    }
    
    lyt <- basic_table(show_colcounts = TRUE) %>% 
        split_cols_by("SEX", split_fun = keep_split_levels(c("F", "U"))) %>% 
        split_cols_by("STRATA2", split_fun = keep_split_levels("S1")) %>% 
        split_rows_by("STRATA1", split_fun = keep_split_levels("A")) %>% 
        summarize_row_groups(var = "BMRKR1", cfun = cfun_tmp) %>%
        split_rows_by("ARMCD") %>% 
        analyze("BMRKR1", afun = afun_tmp)
    
    expect_silent(lyt %>% build_table(ex_adsl))
    expect_error(lyt %>% build_table(ex_adsl, alt_counts_df = DM))
    
    expect_silent(lyt %>% build_table(ex_adsl, alt_counts_df = alt_tmp))
})
