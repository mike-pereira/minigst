% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{dbplot_point}
\alias{dbplot_point}
\title{Plot variables in a Db}
\usage{
dbplot_point(
  db,
  color = NULL,
  cat_color = NULL,
  size = NULL,
  cmap = NULL,
  colValLimits = NULL,
  naColor = "transparent",
  pointColor = "black",
  sizeRange = c(0.5, 3),
  absSize = FALSE,
  hideLegend = FALSE,
  legendTitle = NA,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  add = FALSE
)
}
\arguments{
\item{db}{Db object.}

\item{color, cat_color}{Name of the variable defining the color of the points: use `color` for continuous variables and `cat_color` for categorical variables.}

\item{size}{Name of the variable defining the size of the points.}

\item{cmap}{Color map used to plot the points by color. Can be specified by a name (cf. \code{\link{printAllPalettes}} to display available choices), or by a vector of color names which will be interpolated to create a palette.}

\item{colValLimits}{Range of values for teh color mapping.}

\item{naColor}{Color used for NA values.}

\item{pointColor}{Default color of points when \code{color} is not specified.}

\item{sizeRange}{Vector specifying the range of sizes when plotting the points by size.}

\item{absSize}{Whether the absolute value of the variable should be used to define the size of the points.}

\item{hideLegend}{Whether to hide the legend.}

\item{legendTitle}{Title(s) of the legend: either a single string if just the color or the size if plotted, or a vector containing respectively the title of the color legende and the title of the size legend. Setting the legend title to `NA` means that the name of the variable is used as a title.}

\item{xlab}{Title of the x-axis.}

\item{ylab}{Title of the y-axis.}

\item{title}{Title of plot.}

\item{add}{Either a boolean or \pkg{ggplot2} object. If \code{FALSE} (Default), then a new plot is created. If \code{TRUE}, then the points are added to the last plot (see \pkg{ggplot2::last_plot()}) specifying if the points are to be added to the last plot or not}
}
\value{
A \pkg{ggplot2} object containing the plot. If stored in a variable, use the function \code{print} to display the plot.
}
\description{
Function to plot variables in a Db object as isolated points. The value of the variables are used to define
the color, size and/or labels of the points.
}
\details{
Note that you can specify only one of the arguments `color` and `cat_color`,but you can specify it jointly with the `size` argument.
}
\examples{
library(minigst)

# Load Data
data("Scotland")

# Create Db
db=dfToDb(df=Scotland, coordnames=c("Longitude", "Latitude"))

# Plot the variables Elevation (for the size of the points) and Longitude (for the color of the points)
dbplot_point(db=db,size="Elevation",color="Longitude")

# Store the plot, and then display it
plt=dbplot_point(db=db,size="Elevation",title="Printed plot")
print(plt)

}
