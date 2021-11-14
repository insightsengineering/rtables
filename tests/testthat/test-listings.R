context("Listings machinery")


test_that("listings var labels don't get mucked up by topleft machinery #262", {
    anl <- ex_adsl
    anl <- anl[1:10, c("USUBJID", "ARM", "BMRKR1")]
    anl <- var_relabel(anl,
            USUBJID = "Unique\nSubject\nIdentifier",
            ARM = "Description\nOf\nPlanned Arm"
            )

    lsting <- as_listing(anl, key_cols = c("USUBJID")) %>%
        add_listing_col("ARM")
    expect_identical(var_labels(anl), var_labels(lsting))

    matform <- listing_matrix_form(lsting)
    expect_identical(matform$strings[1:3, 1, drop = TRUE],
                     c("Unique", "Subject", "Identifier"))
})
