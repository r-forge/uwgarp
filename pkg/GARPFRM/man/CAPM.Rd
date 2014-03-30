\name{CAPM}
\alias{CAPM}
\title{Capital Asset PrisignificanceLevelng Model}
\usage{
CAPM(R, Rmkt)
}
\arguments{
  \item{R}{asset returns}

  \item{Rmkt}{market returns}
}
\description{
TODO: Need a better description of the CAPM
}
\details{
Retrieves alphas, betas, as well as pvalue and tstats. The
CAPM is used to determine a theoretically appropriate rate
of return of the non-diversifiable risk of an asset.
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
\author{
Thomas Fillebeen
}

