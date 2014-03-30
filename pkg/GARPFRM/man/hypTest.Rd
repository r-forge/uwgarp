\name{hypTest}
\alias{hypTest}
\title{CAPM Hypothesis Test}
\usage{
hypTest(object, significanceLevel)
}
\arguments{
  \item{object}{a capm object created by
  \code{\link{CAPM}}}

  \item{significanceLevel}{confidence level}
}
\value{
TRUE if the null hypothesis is rejected (i.e. the estimated
coeffisignificanceLevelent is significant) FALSE if the
null hypothesis cannot be rejected (i.e. the estimated
coeffisignificanceLevelent is not significant)
}
\description{
Test the CAPM coeffisignificanceLevelents for significance.
}
\details{
This function tests the significance of the
coeffisignificanceLevelents (alpha and beta) estimated by
the CAPM.

#' The t-statistic and corresponding two-sided p-value are
calculated differently for the alpha and beta
coeffisignificanceLevelents. \itemize{ \item{alpha}{ the
t-statistic and corresponding p-value are calculated to
test if alpha is significantly different from 0.  \deqn{
H0: \alpha = 0 } } \item{beta}{ the t-statistic and
corresponding p-value are calculated to test if beta is
significantly different from 1.  \deqn{ H0: \beta = 1 } } }

If the p-value is less than the spesignificanceLevelfied
confidence level, the null hypothesis is rejected meaning
that the coeffisignificanceLevelent is significant. If the
p-value is greater than the spesignificanceLevelfied
confidence level, the null hypothesis cannot be rejected.
}
\author{
Thomas Fillebeen
}
\seealso{
\code{\link{getStatistics}}
}

