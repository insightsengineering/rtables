
makefakedat = function(n  = 1000) {
    datadf = data.frame(stringsAsFactors = FALSE,
                        ARM = c("ARM1", sample(c("ARM1", "ARM2"), n - 1, replace = TRUE)),
                        SEX = c("M", sample(c("M", "F"), n - 1, replace = TRUE)),
                        FACTOR2 = c("A", sample(c("A", "B", "C"), n - 1, replace = TRUE)),
                        RACE = c("WHITE", sample(c("WHITE", "BLACK"), n - 1, replace = TRUE)),
                        AGE = runif(n, 40, 70),
                        VAR3 = c("level1", sample(c("level1", "level2"), n -1,
                                                  replace = TRUE)))

    datadf$ethn_label = c(WHITE = "Caucasian", BLACK = "African American")[datadf$RACE]
    datadf$fac2_label = paste("Level", datadf$FACTOR2)
    datadf$gend_label = c(M="Male", F="Female")[datadf$SEX]
    datadf
}



makefakedat2 =  function(n  = 1000) {
    if(n%%4 != 0) {
        stop("n not multiple of 4")
    }

    many2s = rep(2, n/2)
    datadf = data.frame(stringsAsFactors = FALSE,
                        ARM = rep(c("ARM1", "ARM2"), times = rep(n/2, 2)),
                        SEX = rep(sample(c("M", "F"), n/2, replace = TRUE),
                                  many2s),
                        RACE = rep(sample(c("WHITE", "BLACK"), n/2, replace = TRUE),
                                   times = many2s),
                        PATID = rep(seq(1, n/2), many2s),
                        VISIT = rep(c("BASELINE", "FOLLOWUP"))
                        )
    datadf$ethn_label = c(WHITE = "Caucasian", BLACK = "African American")[datadf$RACE]
    datadf$gend_label = c(M="Male", F="Female")[datadf$SEX]
    mu = 5 + (as.integer(factor(datadf$RACE)) + as.integer(factor(datadf$ARM)) + as.integer(factor(datadf$SEX)))/2
    datadf$VALUE =  ifelse(datadf$VISIT == "BASELINE",
                           5,
                           5 + rnorm(n, mu, 4))
    datadf$PCTDIFF = NA_real_
    seconds = seq(2, n, by =2)
    datadf$PCTDIFF[seq(2, n, by=2)] = 100*(datadf$VALUE[seconds] - datadf$VALUE[seconds-1]) / datadf$VALUE[seconds - 1]

    datadf
}
set.seed(0)
rawdat <- makefakedat()
rawdat2 <- makefakedat2()

## used in multiple test files
refcompmean = function(x, .ref_group, .in_ref_col, ...) {
    if(.in_ref_col)
        val <- rcell(NULL)
    else
        val <- rcell(mean(x, ...) - mean(.ref_group,...), format = "xx.xx")

    in_rows(
        "Diff from reference - mean" = val
    )
}

complx_lyt_rnames <- c("Caucasian (n)", "Level A", "Age Analysis", "mean", "median",
                 "Age Analysis redux", "range", "Level B", "Age Analysis",
                 "mean", "median", "Age Analysis redux", "range",
                 "African American (n)", "Level A", "Age Analysis", "mean", "median",
                 "Age Analysis redux", "range", "Level B", "Age Analysis",
                 "mean", "median", "Age Analysis redux", "range",
                 "level1", "level2")
