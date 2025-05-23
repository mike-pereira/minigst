% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Db.R
\name{setVar}
\alias{setVar}
\title{Set variables of interest in a Db}
\usage{
setVar(db, vname, mode = "Var")
}
\arguments{
\item{db}{Db object.}

\item{vname}{Name(s) of the selected variable(s), stored as a (vector of) string(s).}

\item{mode}{Role of the variable: either "Var" for variable of interest, or "Drift" for drift specification.}
}
\value{
The function updates \code{db} and returns nothing.
}
\description{
Function to select which variables in the Db are the variables of interest or drifts in the study.
}
\examples{
library(minigst)

# Load Data


# Create Db
data("Scotland")
db=dfToDb(df=Scotland, coordnames=c("Longitude", "Latitude"))
db$display()

# Select the elevation as the variable of interest
setVar(db=db, vname="Elevation")
db$display()

}
