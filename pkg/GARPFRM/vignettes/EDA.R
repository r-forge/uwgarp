\documentclass[a4paper]{article}
\usepackage[OT1]{fontenc}
\usepackage{Sweave}
\usepackage{Rd}
\usepackage{amsmath}
\usepackage{hyperref}
\usepackage{url}
\usepackage[round]{natbib}
\usepackage{bm}
\usepackage{verbatim}
\usepackage[latin1]{inputenc}
\bibliographystyle{abbrvnat}

\let\proglang=\textsf
%\newcommand{\pkg}[1]{{\fontseries{b}\selectfont #1}}
%\newcommand{\R}[1]{{\fontseries{b}\selectfont #1}}
%\newcommand{\email}[1]{\href{mailto:#1}{\normalfont\texttt{#1}}}
%\newcommand{\E}{\mathsf{E}}
%\newcommand{\VAR}{\mathsf{VAR}}
%\newcommand{\COV}{\mathsf{COV}}
%\newcommand{\Prob}{\mathsf{P}}

\renewcommand{\topfraction}{0.85}
\renewcommand{\textfraction}{0.1}
\renewcommand{\baselinestretch}{1.5}
\setlength{\textwidth}{15cm} \setlength{\textheight}{22cm} \topmargin-1cm \evensidemargin0.5cm \oddsidemargin0.5cm

\usepackage[latin1]{inputenc}
% or whatever

\usepackage{lmodern}
\usepackage[T1]{fontenc}
% Or whatever. Note that the encoding and the font should match. If T1
% does not look nice, try deleting the line with the fontenc.

\begin{document}

\title{Exploratory Data Analysis, basic probability and statistics}
\author{Ross Bennett}

\maketitle

\begin{abstract}
The goal of this vignette is to demonstrate key concepts in Financial Risk Manager (FRM (R)) Part 1: Quantitative Analysis using R and the GARPFRM package. This vignette will cover exploratory data analysis, basic probability and statistics, and linear regression.
\end{abstract}

\tableofcontents

\section{Exploratory Data Analysis}

Load the GARPFRM package and the \verb"returns" dataset. The \verb"returns" dataset includes weekly returns for SPY, AAPL, XOM, GOOG, MSFT, and GE from 2005-01-14 to 2013-11-22.
<<>>=
library(GARPFRM)
data(returns)
@

The exploratory data analysis, basic probability and statistics will use the SPY weekly returns.
<<>>=
SPY.ret <- returns[, "SPY"]
@

Plot of the SPY weekly returns. 
<<>>=
plot(SPY.ret, main="SPY Weekly Returns")
@

The density of the SPY weekly returns is plotted to better understand its distribution. A normal density is overlayed on the plot with standard estimates of the sample mean and standard deviation. Another normal density is overlayed using robust estimates. It is clear from the chart that the robust estimates provide a better fit than the standard estimates of the sample mean and sample standard deviation, but it is not clear if the SPY returns are normally distributed.
<<>>=
# Plot the density of SPY Weekly Returns
plot(density(SPY.ret), main="Density of SPY Weekly Returns")
rug(SPY.ret)
# sample estimates
curve(dnorm(x, mean=mean(SPY.ret), sd=sd(SPY.ret)), 
      add=TRUE, col="red", lty=2, lwd=2)
# robust estimates
curve(dnorm(x, mean=median(SPY.ret), sd=mad(SPY.ret)), 
      add=TRUE, col="blue", lty=2, lwd=2)
legend("topleft", legend=c("estimated density", "normal density", "robust normal density"), 
       col=c("black", "red", "blue"), lty=c(1, 2, 2), bty="n", cex=0.8)
@

Quantile-Quantile plot of SPY weekly returns. It can be seen from the Normal Q-Q plot that the SPY returns have "fat tails".
<<>>=
qqnorm(SPY.ret)
qqline(SPY.ret)
@

We can test if the SPY weekly returns came from a normal distribution using the Shapiro-Wilk test of normality. The null hypothesis is that the data came from a normal distribution. The p-value is very small and we can reject the null hypothesis.
<<>>=
shapiro.test(coredata(SPY.ret))
@

\subsection{Basic Statistics}
Here calculate some basic statisitics on the SPY weekly returns.
<<>>=
# Sample mean of SPY return
mean(SPY.ret)

# Sample Variance of SPY returns
var(SPY.ret)

# Sample standard deviation of SPY returns
sd(SPY.ret)

# Standard error of SPY returns
sd(SPY.ret) / sqrt(nrow(SPY.ret))

# Sample skewness of SPY returns
# See ?skewness for additional methods for calculating skewness
skewness(SPY.ret, method="sample")

# Sample kurtosis of SPY returns
# See ?kurtosis for additional methods for calculating kurtosis
kurtosis(SPY.ret, method="sample")

# Summary statistics of SPY returns
summary(SPY.ret)

# Sample quantiles of SPY returns
quantile(SPY.ret, probs=c(0, 0.25, 0.5, 0.75, 1))
@


\end{document}