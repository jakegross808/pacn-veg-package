% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EDA_understory.R
\name{UnderNativityCover.plot.nat_v_non}
\alias{UnderNativityCover.plot.nat_v_non}
\title{Native V. Non-native Cover Plot for Understory data}
\usage{
UnderNativityCover.plot.nat_v_non(
  combine_strata = FALSE,
  paired_change = FALSE,
  crosstalk = FALSE,
  crosstalk_group = "cover",
  interactive = FALSE,
  year_filter = TRUE,
  park,
  sample_frame,
  community,
  year,
  cycle,
  plot_type,
  paired_cycle,
  silent = FALSE,
  data_table
)
}
\arguments{
\item{combine_strata}{If \code{TRUE}, don't split data into high and low strata. Otherwise, keep as is.}

\item{paired_change}{If \code{TRUE}, calculate change in percent cover across two cycles. If \code{cycle} is not specified, it will default to the last cycle.}

\item{park}{Four letter unit code of park(s)}

\item{sample_frame}{Name of sample frame}

\item{community}{Name of plant community}

\item{year}{Monitoring year}

\item{cycle}{Monitoring cycle}

\item{plot_type}{Type of plot (fixed vs. rotational)}

\item{paired_cycle}{Only required if \code{paired_change == TRUE}. The cycle number to compare to when calculating paired change.}

\item{silent}{Suppress informational messages?}
}
\value{
Nat-v-Non Plot comparing Native and Non-native Cover (or Change in Cover)
}
\description{
Native V. Non-native Cover Plot for Understory data
}
\examples{
\dontrun{

Native_v_Nonnative_Plot <- UnderNativityCover.plot.nat_v_non(sample_frame = "Haleakala", sample_cycle = 2, paired_change = TRUE)
}
}
