.onLoad <- function(libname = find.package("RepDataPeerAssignment1"),
                    pkgname = "RepDataPeerAssignment1"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # sample file names from taxstats
      c("value", "Var1", "Var2",

        # we use the magrittr pipe
        ".",

        # to return
        "out",

        # generic.inflators
        "variable",

        # CGT inflator
        "marginal_rate_first",


        # Taxstats Table 1
        "Selected_items",
        "Sum",

        # dput(unique(c(names(grattan:::medicare_tbl), names(grattan:::sapto_tbl), names(grattan:::cgt_expenditures))))
        c("family_status", "lower_threshold", "URL", "Projected"),

        # lito_tbl
        "max_lito", "min_bracket", "lito_taper"
      )
    )
  invisible()
}
