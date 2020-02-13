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






## paginate_ttree = function(ttree, perpage = 80,
##                           reprint.controws = TRUE) {


##     hdr = .make_s3_header(ttree)
##     ## +1 for the --------- divider line
##     ncolheader = length(hdr) + 1L

##     realppage = perpage - ncolheader
    
##     allrows = collect_leaves(ttree, TRUE, TRUE)

##     ##no pagination required
##     if(length(allrows) <= realppage)
##         return(to_s3compat(ttree))

##     levs = sapply(allrows, tt_level)
##     pos = realppage 
##     lastpag = 0
##     ret = list()
##     while(lastpag < length(allrows)) {

##         lst = .fixpaginatepos(pos, allrows, lastpag, perpage = realppage, incl.crows = reprint.controws )
##         pos = lst[["pos"]]
##         reprows = lst[["reprows"]]
##         rws = c(reprows, allrows[seq(lastpag + 1, pos)])
##         s3rtab = rtablel(header = hdr, lapply(rws, to_s3compat), format = obj_fmt(ttree))
##         ret = c(ret, list(s3rtab))
##         lastpag = pos
##         pos = min(length(allrows), pos + realppage)
##     }

##     ret
## }



## .fixpaginatepos = function(p, rows, lastpag = 0, perpage, incl.crows) {

##     if(lastpag >= length(rows)) {
##         warning("extra iteration, lastpag is already > length(rows) - perpage")
##         return(list(pos = p, reprows = list()))
##     }

##     ## reprows only depends on the FIRST row on a page!
##     reprows = getreprows(lastpag, rows, perpage, incl.crows)
    
##     stopifnot(p > lastpag || p == length(rows))

##     ## walk backwards until we are in an ok spot or there is nowhere else to look
##     while( p > lastpag &&
##            ## check total lines required
##            ((p - lastpag) + length(reprows) > perpage ||
##             ## check if we are at a content row
##             is_content_pos(rows[[p]]) ||
##             ## check if we're at a first and not only data row
##             (is_content_pos(rows[[p - 1]]) && !is_content_pos(rows[[p + 1]])))
##           ) {
##               p = p -1
##           }

##     if(p == lastpag) stop("Unable to find pagination position between rows", lastpag, " and ", lastpag + perpage)
    
##     list(pos = p, reprows = reprows)
## }


## ## Repeat rows only depend on the FIRST row in a table, not on where
## ## we think the next pagination might occur!!!
## getreprows = function(lastpag, allrows, perpage, incl.crows = TRUE) {

##     ## If we're on the first page, no repeated rows
##     ## also if we already paginated the whole thing (this should never happen)
##     if(lastpag == 0 || lastpag >= length(allrows))
##         return(list())


##     ## We only need to repeat things that won't be
##     ## in the rows in our page, that translates to
##     ## the things of a lower level than we start at
##     ## at lastpag + 1
##     curlev = tt_level(allrows[[lastpag + 1]]) - 1
##     if(curlev < 0)
##         return(list())
    
##     ## we know lastpag is > 1 here because
##     ## we would have hit the early escape if
##     ## we were in the first page
##     rows = allrows[1:lastpag]

##     ret = list()
 
##     rowlevs = sapply(rows, tt_level)
##     labrows = sapply(rows, is, "LabelRow")
##     labinds = which(labrows)
    
##     repcontrows = numeric()

##     for(l in seq(0, curlev)) {
##         lablevpos = max(which(labrows & rowlevs == l))
##         if(is.finite(lablevpos)) {
##             contstart = lablevpos
##             while(incl.crows &&
##                   contstart > 1 &&
##                   is_content_pos(rows[[contstart - 1]]) &&
##                   tt_level(rows[[contstart - 1]]) == l) {
##                       contstart = contstart -1
##                   }
            
##             ## the sequence includes the label row!!
##             repcontrows = c(repcontrows, seq(contstart, lablevpos))

            
##         }
##     }
##     ## this should be completely redundant right?
##     repcontrows = unique(repcontrows[repcontrows <= lastpag])
##     allrows[repcontrows]
## }

## this is where we will take wordwrapping
## into account when it is added
##
## ALL calculations of vertical space for pagination
## purposes must go through nlines and divider_height!!!!!!!!

## this will be customizable someday. I have foreseen it (spooky noises)
divider_height = function(cinfo) 1L

setGeneric("nlines",
           function(x, colwidths) standardGeneric("nlines"))

setMethod("nlines", "TableRow",
          function(x, colwidths) {
    1L
})

setMethod("nlines", "LabelRow",
          function(x, colwidths) {
    if(labrow_visible(x))
        1L
    else
        0L
})


setMethod("nlines", "VTableTree",
          function(x, colwidths) {
    length(collect_leaves(x, TRUE, TRUE))
})

setMethod("nlines", "InstantiatedColumnInfo",
          function(x, colwidths) {
    lfs = collect_leaves(coltree(x))
    depths = sapply(lfs, function(l) length(pos_splits(l)))
    max(depths) + divider_height(x)
    
})

pagdfrow = function(row,
                    nm = obj_name(row),
                    lab = obj_label(row),
                    rnum,
                    pth ,
                    sibpos = NA_integer_,
                    nsibs = NA_integer_,
                    extent = nlines(row, colwidths),
                    colwidths = NULL,
                    repext = 0L,
                    repind = integer()
                    ) {
     
    data.frame(label = lab,
               name = nm,
               abs_rownumber = rnum,
               path = I(list(pth)),
               pos_in_siblings = sibpos,
               n_siblings = nsibs,
               self_extent = extent,
               par_extent = repext,
               reprint_inds = I(list(unlist(repind))),
               row_class = class(row),
               
               stringsAsFactors = FALSE)
}

#' Make layout summary  data.frame for use during pagination
#' @inheritParams argument_conventions
#' @export
make_pagdf = function(tt, colwidths = NULL) {
    rownum = 0
    pag_df = function(tree, path, incontent = FALSE,
                      cwidths,
                      repr_ext = 0L,
                      repr_inds = integer()) {
        ret = list()
        if(labrow_visible(tree)) {
            lr = tt_labelrow(tree)
            rownum <<- rownum + 1L
            ret  =  c(ret,
                      list(pagdfrow(row = lr,
                                    rnum = rownum,
                                    nm = obj_name(tree),
                                    pth = path,
                                    colwidths = cwidths,
                                    repext = repr_ext,
                                    repind = list(repr_inds) )))
            repr_ext = repr_ext + 1L
            repr_inds = c(repr_inds, rownum)
        }
        if(is(tree, "TableTree") &&
           nrow(content_table(tree)) > 0) {
            ctab = content_table(tree)
            ## already put rownum in there if necessary
            rnbef = rownum + 1L 
            crows = pag_df(ctab,
                           path = path,
                           cwidths = cwidths,
                           repr_ext = repr_ext,
                           repr_inds = repr_inds)
            if(is(crows, "data.frame"))
                crows = list(crows)
            ret = c(ret, crows)
            repr_ext = repr_ext + nlines(ctab)
            repr_inds = c(repr_inds, rnbef:rownum)
        }
        kids = tree_children(tree)
        nk = length(kids)
        for(i in seq_along(kids)) {
            k = kids[[i]]
            if(is(k, "TableRow")) {
                rownum <<- rownum + 1
                ret = c(ret, list(pagdfrow(k, rnum = rownum,
                                         colwidths = cwidths,
                                         sibpos = i,
                                         nsibs = nk,
                                         pth = path,
                                         repext = repr_ext,
                                         repind = repr_inds)))
            } else {
                newrows = pag_df(k, path = c(path, obj_name(k)),
                                 cwidths = cwidths, repr_ext = repr_ext,
                                 repr_inds = repr_inds)
                if(is(newrows, "data.frame")) {
                    newrows = list(newrows)
                }
                ret = c(ret, newrows)
            }
        }
        ret
        
    }
    rws = pag_df(tt, path = "root", cwidths = colwidths)
    do.call(rbind.data.frame, rws)
}

    

valid_pag = function(pagdf,
                     guess,
                     min_sibs,
                     nosplit = NULL,
                     verbose = FALSE) {
    rw = pagdf[guess,]
   
    if(verbose)
        message("Checking pagination after row ", guess)
    if(rw[["row_class"]] %in% c("LabelRow", "ContentRow")) {
        if(verbose)
            message("\t....................... FAIL: last row is a label or content row")
        return(FALSE)
    }

    sibpos = rw[["pos_in_siblings"]]
    nsib = rw[["n_siblings"]]
    okpos = min(min_sibs + 1, rw[["n_siblings"]])
    if( sibpos != nsib){
        retfalse = FALSE
        if(sibpos < min_sibs + 1) {
            retfalse = TRUE
            if(verbose)
                message("\t....................... FAIL: last row had only ", sibpos - 1, "preceeding siblings, needed ", min_sibs)
        } else if (nsib - sibpos < min_sibs + 1) {
            retfalse = TRUE
            if(verbose)
                message("\t....................... FAIL: last row had only ", nsib - sibpos - 1, "following siblings, needed ", min_sibs)
        }
        if(retfalse)
            return(FALSE)
    }
    if(guess < nrow(pagdf)) {
        curpth = unlist(rw$path)
        nxtpth = unlist(pagdf$path[[guess+1]])
        inplay = nosplit[(nosplit %in% intersect(curpth, nxtpth))]    
        if(length(inplay) > 0) {
            curvals = curpth[match(inplay, curpth) + 1]
            nxtvals = nxtpth[match(inplay, nxtpth) + 1]
            if(identical(curvals, nxtvals)) {
                if(verbose)
                    message("\t....................... FAIL: values of unsplitable vars before [", curvals, "] and after [", nxtvals, "] match")
                return(FALSE)
            }
        }
    }
    if(verbose)
        message("\t....................... OK")
    TRUE
}


find_pag = function(pagdf,
                    start,
                    guess,
                    rlpp,
                    min_siblings,
                    nosplitin = character(),
                    verbose = FALSE) {
    origuess = guess
    while(guess >= start && !valid_pag(pagdf, guess, min_sibs = min_siblings, nosplit = nosplitin, verbose)) {
        guess = guess - 1
    }
    if(guess < start)
        stop("Unable to find any valid pagination between ", start, " and ", origuess)
    guess
}

#' Determine pagination of a TableTree
#' @inheritParams argument_conventions
#' @param lpp numeric. Maximum lines per page including (re)printed header and context rows
#' @param min_siblings. numeric. Minimum sibling rows which must appear on either side of pagination row for a mid-subtable split to be valid. Defaults to 2.
#' @export
pag_tt_indices = function(tt, lpp = 15,
                           min_siblings = 2,
                           nosplitin = character(),
                           colwidths = NULL,
                           verbose = FALSE) {

    
    hlines = nlines(col_info(tt))
    ## row lines per page
    rlpp = lpp - hlines
    pagdf = make_pagdf(tt, colwidths)
  
    
    start = 1
    nr = nrow(pagdf)
    ret = list()
    while(start < nr) {
        adjrlpp = rlpp - pagdf$par_extent[start]
        stopifnot(adjrlpp > 0)
        guess = min(nr, start + adjrlpp - 1)
        end = find_pag(pagdf, start, guess,
                       rlpp = adjrlpp,
                       min_siblings = min_siblings,
                       nosplitin = nosplitin,
                       verbose = verbose)
        ret = c(ret, list(c(pagdf$reprint_inds[[start]],
                            start:end)))
        start = end + 1
    }
    ret
}
