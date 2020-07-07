
iter_colby = function(lyt, cbys, ref_groups = vector("list", length(cbys))) {

    ##unpiped version
    for(i in seq_along(cbys)) {
        cb = cbys[[i]]
        if(!is.null(ref_groups[[i]]))
            lyt = split_cols_by(lyt, cb, lbl = cb, ref_group = ref_groups[[i]])
        else
            lyt = split_cols_by(lyt, cb, lbl = cb)
    }
    lyt
}

TF_counter = function(nms = NULL) {
    function(x, .N_col) {
    tcnt = sum(x)
    fcnt = .N_col - tcnt
    ret = list(c(tcnt, tcnt/.N_col),
               c(fcnt, fcnt/.N_col))
    if(!is.null(nms))
        names(ret) = nms
    ret

    }
}


.make_2xk_tab = function(coldat, bldat) {
    if(is.list(coldat) && length(coldat) == 1)
        coldat = coldat[[1]]
    if(is.list(bldat) && length(bldat) == 1)
        bldat = bldat[[1]]
    
    catvec = c(rep("ref_group", times = length(bldat)),
               rep("coldata", times = length(coldat)))
    tab = table(catvec, c(bldat, coldat))
    tab
}

## TODO: remove internal package-code use of %>% (maybe?)

tt_rsp_lyt = function(col_by, ref_groups = vector("list", length(col_by))) {
    NULL %>% iter_colby(cbys = col_by, ref_groups = ref_groups) %>%
        ## respodner and nonresponder counts
        add_analyzed_var("rsp", lbl = " ",
                         afun = TF_counter( nms = c("Responders", "Non-Responders")),
                         format = "xx (xx.%)") %>%
        
        ## response rate CI
        add_analyzed_var("rsp", lbl = " ",
                         afun = function(x) {
            list("95% CI for Response Rates (Clopper-Pearson)" = binom.test(sum(x), length(x))$conf.int * 100)
        }, format = "(xx.xx, xx.xx)", nested = TRUE) %>%
        
        ## Difference in Response Rates section
        ## I think this one should acttually e a content row?
        analyze_against_ref_group(var = "rsp", lbl =" ",
                               afun = function(x) c("Difference in Response Rates (%)" =  mean(x)*100), format = "xx.xx", nested = TRUE) %>%
        
        ## (both) CIs for diff in response rates
        analyze_against_ref_group_2dtable(var = "rsp", lbl = " ",
                              compfun = function(tab) {
            list("95% CI for difference (Wald without continuity correction)"=
                     prop.test(tab, correct = FALSE)$conf.int*100,
                 "95% CI for difference (Wald with continuity correction)" =
                     prop.test(tab, correct = TRUE)$conf.int*100)
        }, format = "(xx.xx, xx.xx)", nested = TRUE) %>%
        
        ## Odds ratio
        analyze_against_ref_group_2dtable(var = "rsp",
                              lbl = " ",
                              compfun = function(tab) {
            fit = tern:::odds_ratio(tab)
            list("Odds Ratio" = fit$estimator,
                 "95% CI" = fit$conf.int)
            }, format = c("xx.xx", "(xx.xx, xx.xx)"), nested = TRUE) %>%
        ## Response rates by type of response
        split_rows_by("AVALC", " ",
                            splfun = reord_levs_sfun(neworder = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE"), drlevels = TRUE), nested = TRUE) %>%
        ## counts are a summary content row
        summarize_row_groups("AVALC", "%s") %>%
        ## CIs are an analysis row
        add_analyzed_var("AVALC", lbl = " ", afun = function(x, .N_col) {
            list("95% CI" = binom.test(length(x), .N_col)$conf.int * 100)
        }, format = "(xx.xx, xx.xx)")

}
