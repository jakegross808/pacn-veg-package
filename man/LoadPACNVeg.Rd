% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{LoadPACNVeg}
\alias{LoadPACNVeg}
\title{Load raw data into package environment}
\usage{
LoadPACNVeg(
  ftpc_params = "pacn",
  eips_paths,
  data_path,
  data_source = "db",
  cache = TRUE,
  expire_interval_days = 7,
  force_refresh = FALSE,
  TE_Species = TRUE
)
}
\arguments{
\item{ftpc_params}{Connection information for FTPC database. Either a DSN name, a path to a csv containing connection information, or a named list of connection arguments. The csv or list should contain the following parameters:
\itemize{
\item Driver
\item Server
\item Database
\item Trusted_Connection
}}

\item{eips_paths}{Character vector of paths to EIPS database(s).}

\item{data_path}{A path to either:
\itemize{
\item a folder containing the data in csv format
\item a .zip file containing the data in csv format
\item an .Rdata file
}}

\item{data_source}{Either "db" (fetch data from databases or cache) or "file" (fetch data from folder or zip archive of csv's).}

\item{cache}{Should the data be cached locally to avoid reading from the databases every time?}

\item{expire_interval_days}{Amount of time (in days) before the cache expires and has to be refreshed from the Access db. Defaults to 7 days. Ignored if \code{ftpc_conn} and \code{eips_paths} are \code{NULL}.}

\item{force_refresh}{Refresh the cache from the databases even if it's not expired? Ignored if \code{cache == FALSE}.}

\item{TE_Species}{default = TRUE. Include T&E species names in output.}
}
\value{
Invisibly return a list containing all raw data
}
\description{
Run this function before you do anything else.
}
\examples{
\dontrun{

eips <- c("path/to/database1_be.mdb", "path/to/database2_be.mdb")
ftpc <- "pacnveg"

# Read data from veg databases and cache it locally for a week (default)
LoadPACNVeg(ftpc, eips)

# Force refresh cached data that haven't expired
LoadPACNVeg(ftpc, eips, force_refresh = TRUE)

# Read data from a folder of csv files. This will not be cached.
path_to_csv <- "path/to/csv/folder"
LoadPACNVeg(data_path = path_to_csv)
}

}
