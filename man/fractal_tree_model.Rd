% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fractal_tree_model.R
\name{fractal_tree_model}
\alias{fractal_tree_model}
\title{Build a 2D L-System Tree from Pre-defined Tree Templates}
\usage{
fractal_tree_model(tree_model, n_iter)
}
\arguments{
\item{tree_model}{The integer or the name (character) that refers to the desired tree model. Check details.}

\item{n_iter}{Integer. The number of iterations to build the tree. Each tree model has its own default value.}
}
\description{
Build a 2D L-System Tree from Pre-defined Tree Templates
}
\details{
There are 10 pre-implemented tree templates in this function, as follows (default \code{n_iter} between parentheses):
\itemize{
\item 1 or "binary_tree" (6);
\item 2 or "alternate_tree" (5);
\item 3 or "arrow_weed" (5);
\item 4 or "twiggy_weed" (5);
\item 5 or "stochastic_fuzzy_weed" (4);
\item 6 or "crooked_binary_tree" (6);
\item 7 or "crooked_alternate_tree" (5);
\item 8 or "crooked_arrow_weed" (5);
\item 9 or "crooked_twiggy_weed" (5);
\item 10 or "crooked_stochastic_fuzzy_weed" (4).
}
}
