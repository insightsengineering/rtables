## Rules for pagination
##
## 1. user defined number of lines per page
## 2. all lines have the same height
## 3. header always reprinted on all pages
## 4. "Label-rows", ie content rows above break in the nesting structure, optionaly reprinted (default TRUE)
## 5. Never (?) break on a "label"/content row
## 6. Never (?) break on the second (ie after the first) data row at a particular leaf Elementary table.
##
## Current behavior: paginate_ttree takes a TableTree object and
## returns a list of rtable (S3) objects for printing.






paginate_ttree = function(ttree, perpage = 80,
                          reprint.controws = TRUE) {


    hdr = .make_s3_header(ttree)
    ## +1 for the --------- divider line
    ncolheader = length(hdr) + 1L

    realppage = perpage - ncolheader
    
    allrows = collect_leaves(ttree, TRUE, TRUE)

    ##no pagination required
    if(length(allrows) <= realppage)
        return(to_s3compat(ttree))

    levs = sapply(allrows, tt_level)
    pos = realppage 
    lastpag = 0
    ret = list()
    while(lastpag < length(allrows)) {

        lst = .fixpaginatepos(pos, allrows, lastpag, perpage = realppage, incl.crows = reprint.controws )
        pos = lst[["pos"]]
        reprows = lst[["reprows"]]
        rws = c(reprows, allrows[seq(lastpag + 1, pos)])
        s3rtab = rtablel(header = hdr, lapply(rws, to_s3compat), format = obj_fmt(ttree))
        ret = c(ret, list(s3rtab))
        lastpag = pos
        pos = min(length(allrows), pos + realppage)
    }

    ret
}



.fixpaginatepos = function(p, rows, lastpag = 0, perpage, incl.crows) {

    if(lastpag >= length(rows)) {
        warning("extra iteration, lastpag is already > length(rows) - perpage")
        return(list(pos = p, reprows = list()))
    }
    ## if(p <= perpage)
    ##     return(list(pos = p, repros = list()))

    ## reprows only depends on the FIRST row on a page!
    reprows = getreprows(lastpag, rows, perpage, incl.crows)

    
    stopifnot(p > lastpag || p == length(rows))

    ## walk backwards until we are in an ok spot or there is nowhere else to look
    while( p > lastpag &&
           ## check total lines required
           ((p - lastpag) + length(reprows) > perpage ||
            ## check if we are at a content row
            is_content_pos(rows[[p]]) ||
            ## check if we're at a first and not only data row
            (is_content_pos(rows[[p - 1]]) && !is_content_pos(rows[[p + 1]])))
          ) {
              p = p -1
          }

    if(p == lastpag) stop("Unable to find pagination position between rows", lastpag, " and ", lastpag + perpage)
    
    ## while((p > lastpag &&
    ##        (p - lastpag) + length(reprows) >= perpage &&
    ##        cnt < 1000)
    ##       ) {
    ##           ## we try hard not to paginate between content and
    ##           ## data rows at the same nesting level
    ##           while(is_content_pos(rows[[p]]) && p > lastpag + 1) {
    ##               p = p - 1L
                  
    ##           }
              
          
    ##           reprows = newreprows
    ##           ## are we in a state we like?
    ##           if((p - lastpag) + length(reprows) <= perpage)
    ##               break
              
    ##           p = p - 1 #length(reprows)
    ##           cnt = cnt +1
    ##       }
    ## stopifnot(cnt < 1000)
    ## if(p <= lastpag)
    ##     stop("oopsie daisy")
    
    list(pos = p, reprows = reprows)
}


## Repeat rows only depend on the FIRST row in a table, not on where
## we think the next pagination might occur!!!
getreprows = function(lastpag, allrows, perpage, incl.crows = TRUE) {

    ## If we're on the first page, no repeated rows
    ## also if we already paginated the whole thing (this should never happen)
    if(lastpag == 0 || lastpag >= length(allrows))
        return(list())


    ## We only need to repeat things that won't be
    ## in the rows in our page, that translates to
    ## the things of a lower level than we start at
    ## at lastpag + 1
    curlev = tt_level(allrows[[lastpag + 1]]) - 1
    if(curlev < 0)
        return(list())
    
    ## we know lastpag is > 1 here because
    ## we would have hit the early escape if
    ## we were in the first page
    rows = allrows[1:lastpag]

    ret = list()
 
    rowlevs = sapply(rows, tt_level)
    labrows = sapply(rows, is, "TTLabelRow")
    labinds = which(labrows)
    
    repcontrows = numeric()

    for(l in seq(0, curlev)) {
        lablevpos = max(which(labrows & rowlevs == l))
        if(is.finite(lablevpos)) {
            contstart = lablevpos
            while(incl.crows &&
                  contstart > 1 &&
                  is_content_pos(rows[[contstart - 1]]) &&
                  tt_level(rows[[contstart - 1]]) == l) {
                      contstart = contstart -1
                  }
            
            ## the sequence includes the label row!!
            repcontrows = c(repcontrows, seq(contstart, lablevpos))

            
        }
    }
    ## this should be completely redundant right?
    repcontrows = unique(repcontrows[repcontrows <= lastpag])
    allrows[repcontrows]
}

