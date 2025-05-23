% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_hist}
\alias{plot_hist}
\title{Plot histogram of a variable in a gstlearn db}
\usage{
plot_hist(db, vname, color = "blue", nbin = 30, title = NULL, xlab = NULL)
}
\arguments{
\item{db}{Db object.}

\item{vname}{name of the variable to be represented}

\item{color}{Single color to use for all curves (default to blue)}

\item{nbin}{a single number giving the number of cells for the histogram}

\item{title}{Title of plot}

\item{xlab}{label of the abscissa}
}
\value{
A \pkg{ggplot2} object containing the plot. If stored in a variable, use the function \code{print} to display the plot.
}
\description{
Function to plot the histogram of a variable in a gstlearn db
}
