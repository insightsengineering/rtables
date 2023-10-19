## We actually make one so we can ensure the generated code itself has coverage...

test_sfunx <- function(x, .N_total) { # nocov start
  c(as.list(summary(x)), list(grp = list(
    range = range(x), n_unique = c(.N_total, length(unique(x)))
  )))
} # nocov end

test_afun <- make_afun(test_sfunx,
  .labels = c(
    "Min." = "Minimum",
    "1st Qu." = "1st Quartile",
    "3rd Qu." = "Third Quartile",
    "Max." = "Maximum"
  ),
  .formats = c(
    "Min." = "xx.x",
    "1st Qu." = "xx.xx",
    "3rd Qu." = "xx.xx",
    "Max." = "xx.x"
  )
)

test_afun_grp <- make_afun(test_afun,
  .formats = c(grp = "xx - xx"),
  .ungroup_stats = "grp"
)

test_sfundf <- function(df, .N_total) { # nocov start
  x <- df[[1]]
  c(
    as.list(summary(x)),
    list(grp = list(
      range = range(x),
      n_unique = c(.N_total, length(unique(x)))
    ))
  )
} # nocov end

test_afundf <- make_afun(test_sfundf,
  .labels = c(
    "Min." = "Minimum",
    "1st Qu." = "1st Quartile",
    "3rd Qu." = "Third Quartile",
    "Max." = "Maximum"
  ),
  .formats = c(
    "Min." = "xx.x",
    "1st Qu." = "xx.xx",
    "3rd Qu." = "xx.xx",
    "Max." = "xx.x"
  )
)
