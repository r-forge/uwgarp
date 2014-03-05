\name{CAPM}
\alias{CAPM}
\title{Capital Asset Pricing Model}
\usage{
  CAPM(R, Rmkt)
}
\arguments{
  \item{R}{asset returns}

  \item{Rmkt}{market returns}
}
\description{
  Description of CAPM Retrieves alphas, betas, as well as
  pvalue and tstats. The Model is used to determine a
  theoretically appropriate rate of return of an asset's
  non-diversifiable risk.
}
\examples{
data(crsp.short)

head(largecap.ts)

Rf <- largecap.ts[, "t90"]
R <- largecap.ts[, "CAT"] - Rf
MKT <- largecap.ts[, "market"] - Rf

# Fit the CAPM model
tmp <- CAPM(R=R, Rmkt=MKT)
}

