% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EDA_understory.R
\name{understory_spp_trends_rank}
\alias{understory_spp_trends_rank}
\title{Species understory cover trends, ranked.}
\usage{
understory_spp_trends_rank(
  combine_strata = TRUE,
  plant_grouping = "Species",
  paired_change = TRUE,
  sample_frame = sample_frame,
  cycle = cycle,
  top_n = NULL,
  rank_by = "positive",
  remove_nativity = NULL,
  return_table = FALSE
)
}
\arguments{
\item{combine_strata}{If \code{TRUE}(default), combine high and low strata into one stratum.}

\item{plant_grouping}{set to "Species" (default), Cover values for individual plant species.
Note that the other options in summarize_understory() have not been tested yet for understory_spp_trends_rank():

"None" = No Groups, all vegetation (native & non-native) is lumped together,
"Nativity" = Cover values lumped by native and non-native vegetation.
"Life_Form" = Cover values grouped into native and non-native lifeforms (e.g. Trees, Shrubs, Ferns).}

\item{paired_change}{If \code{FALSE} (default) analysis utilizes both "Fixed" and "Rotational" plots in jitter plot and trend.
If \code{TRUE}, drops "Rotational" plots and uses only "Fixed" plots for analysis, in addition, the geom_jitter points are connected with lines to show trends for each plot across cycles.}

\item{sample_frame}{Name of sample frame}

\item{cycle}{Monitoring cycle}

\item{top_n}{default = NULL which will set a minimum cutoff of species with Cover > 1\% for paired_change == FALSE or Chg_Per_Year > 0.2\% (1\%/5 years) if paired_change == TRUE. Alternatively, a custom number of ranked species to include in output (e.g. for top 5 enter '5', etc.)}

\item{rank_by}{Used with paired_change = TRUE. Should the final graph be ranked by "positive" trend (default) or "negative" trend.}

\item{remove_nativity}{option to remove a Nativity category. For example remove_nativity = "Native" will only show non-native species in final graph.}

\item{return_table}{Default == FALSE. If TRUE, return table instead of graph.}
}
\value{
Figure showing species cover per plot as geom_jitter graph with mean trend over all cycles
}
\description{
Species understory cover trends, ranked.
}
\examples{
\dontrun{
understory_spp_trends_rank(sample_frame = "Puu Alii", top_n = 5, rank_by = "positive", remove_nativity = "Native", paired_change = FALSE)

}
}
