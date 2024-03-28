\name{coef_plot}

\alias{coef_plot}

\title{Analysis for South Sudan Impact Evaluation: Coefficient Plots}

\usage{coef_plot(data, sample, module, path = here::here())}

\arguments{
\item{data}{The panel \code{data.frame} containing all rounds collected and indicators constructed.}

\item{sample}{One of "FULL" (sample: high-frequency + endline) or "END" (sample: endline).}

\item{module}{Module of outcome to analyze. One of "food_security", "agriculture", "livestock", "business", "wage_labor", "psychosocial", "consumption".}

\item{path}{Path to folder to save \code{.png} coefficient plot. Default: \code{here()}.}
}

\description{
Purpose: create a plot to compare standardized estimates for target indicators of an impact evaluation in South Sudan.
The regression includes controls for outcomes at baseline and additional controls selected using Double Lasso Selection.
NOTE: in our impact evaluation, estimates were standardized relative to the mean and standard deviation values
for the control group.
}

\examples{
# Example of how to call the function for the 'food_security' module
coef_plot(data = your_data, sample = "FULL", module = "food_security")
}
