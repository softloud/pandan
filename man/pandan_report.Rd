% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pandan_report.R
\name{pandan_report}
\alias{pandan_report}
\title{Report}
\usage{
pandan_report(project, gs_url = Sys.getenv("PANDAN_TRACKER"), update = FALSE)
}
\arguments{
\item{project}{Project tracking sheet. First column for
components, second to some number of columns for project level progress,
and any column name containing \code{read_edit} will be classified as reading over
and editing, not project progress.}

\item{gs_url}{Url for googlesheet. Defaults to system environment variable
\code{PANDAN}. \code{usethis::edit_r_environ()} to set this.}

\item{update}{Update progress log.}
}
\description{
Report
}
