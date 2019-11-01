
iter_colby = function(lyt, cbys) {

    ##unpiped version
    for(cb in cbys) {
        lyt = add_colby_varlevels(lyt, cb, lbl = cb)
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

tt_rsp_lyt = function(col_by) {
    NULL %>% iter_colby(cbys = col_by) %>%
        ## respodner and nonresponder counts
        add_analyzed_var("rsp", lbl = "Responders",
                         afun = TF_counter( nms = c("Responders", "Non-Responders")),
                         fmt = "xx (xx.%)") %>%
        ## response rate CI
        add_analyzed_var("rsp", lbl = "Response Rate",
                         afun = function(x) {
      list("95% CI for Response Rates (Clopper-Pearson)" = binom.test(sum(x), length(x))$conf.int * 100)
      }, fmt = "(xx.xx, xx.xx)", newtoplev = TRUE) %>%
        ## Response rates by type of response
        add_rowby_varlevels("AVALC", "",
                            splfun = reord_levs_sfun(neworder = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE"), drlevels = TRUE), newtoplev = TRUE) %>%
        add_summary_count("AVALC", "%s") %>%
        add_analyzed_var("AVALC", lbl = "", afun = function(x, .N_col) {
            list("95% CI" = binom.test(length(x), .N_col)$conf.int * 100)
        }, fmt = "(xx.xx, xx.xx)")

}
