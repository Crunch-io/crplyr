% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize.R
\name{summarize}
\alias{summarize}
\alias{summarise.CrunchDataset}
\title{Aggregate a Crunch dataset}
\usage{
\method{summarise}{CrunchDataset}(.data, ...)
}
\arguments{
\item{.data}{A \code{CrunchDataset}}

\item{...}{named aggregations to include in the resulting table.}
}
\value{
A \code{tbl_crunch_cube} or \code{cr_tibble} of results. This subclass
of \code{tibble} allows \code{ggplot2::autoplot} to work, but can get in the way
in some tidyverse operations. You may wish to convert to a tibble using
\code{as_tibble()}.
}
\description{
This is an alternate interface to \code{crunch::crtabs()} that, in addition to
being "tidy", makes it easier to query multiple measures at the same time.
}
\details{
Note that while \code{mutate()} is not generally supported in \code{crplyr}, you can
derive expressions on the fly in \code{summarize()}.
}
\examples{
\dontrun{
ds \%>\%
    filter(cyl == 6) \%>\%
    group_by(vs) \%>\%
    summarize(hp=mean(hp), sd_hp=sd(hp), count=n())
}
}
