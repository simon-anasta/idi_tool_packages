% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_apply_confidential.R
\name{apply_graduated_random_rounding}
\alias{apply_graduated_random_rounding}
\title{Apply graduated random rounding to data frame}
\usage{
apply_graduated_random_rounding(df, GRR_columns, stable_across_cols = NULL)
}
\arguments{
\item{df}{the data.frame to apply graduated random rounding to.}

\item{GRR_columns}{a column or columns of \code{df} to be rounded.}

\item{stable_across_cols}{an optional array of column names. If provided,
these column names are used to generate seeds for the rounding. This
enables consistent or reproducible rounding.}
}
\value{
the data.frame with the columns in \code{GRR_columns} renamed with a
\verb{raw_*} prefix, and a matching column with a \verb{conf_*} prefix that contains
the rounded values.
}
\description{
Applies graduated random rounding (GRR) to specified columns.
Creates raw_ and conf_ columns so original values are preserved.
}
\details{
Thresholds for graduation are set by the Stats NZ microdata output guide.
Only apply one of GRR or RR3. Applyng both will cause errors.
}
