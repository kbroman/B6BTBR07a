\name{bad.array}
\alias{bad.array}
\docType{data}
\title{List of mice with bad array data by tissue}
\description{
These mice should be dropped from tissue-specific calculations such as
genome scans. This needs to be incorporated into analysis (multraw) somehow.
}
\usage{data(bad.array)}
\example{
\dontrun{
bad.array <- list(
  adipose = c("Mouse3176","Mouse3521","Mouse3463","Mouse3243","Mouse3631"),
  liver = c("Mouse3535","Mouse3360","Mouse3248"),
  islet = c(),
  hypo = c("Mouse3464"),
  kidney = c("Mouse3435"),
  gastroc = c("Mouse3426","Mouse3437","Mouse3442","Mouse3443","Mouse3444","Mouse3489","Mouse3526","Mouse3538"))
package.dir <- "~/Rlib/B6BTBR07a/data"
save("bad.array",
  file = file.path(package.dir, "bad.array.RData"),
  compress = TRUE)
}
}
