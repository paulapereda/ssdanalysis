\name{reg_tables}

\alias{reg_tables}

\title{Analysis for South Sudan Impact Evaluation: Regression Tables}

\usage{reg_tables(data, sample, module, path = here::here())}

\arguments{
\item{data}{The panel \code{data.frame} containing all rounds collected and indicators constructed.}

\item{sample}{One of "FULL" (sample: high-frequency + endline) or "END" (sample: endline).}

\item{module}{Module of outcome to analyze. One of "food_security", "agriculture", "livestock", "business", "wage_labor", "coping_strategies", "revenue", "psychosocial", "assets", "consumption", "finance".}

\item{path}{Path to folder to save \code{.tex} regression tables. Default: \code{here()}.}
}

\description{
Purpose: create tables for target indicators of an impact evaluation in South Sudan.
The regression includes controls for outcomes at baseline and additional controls selected using Double Lasso Selection.
NOTE: in our impact evaluation, estimates were standardized relative to the mean and standard deviation values
for the control group.
}

\examples{
# Example of how to call the function for the 'food_security' module
reg_tables(data = your_data, sample = "FULL", module = "food_security")
}
