\name{rrat}
\alias{rrat}
\alias{rrat.default}
\alias{print.rrat}
\alias{predict.rrat}
%- Also NEED an '\alias' for EACH other topic documented here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\title{
Robust Regression with Asymmetric Heavy-Tail Noise Distributions
%%  ~~function to do ... ~~
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\description{
  \code{rrat} implements Robust Regression approach tailored to deal with Asymmetric Tail noise distribution. 
The main idea is to learn most of the parameters of the model using conditional quantile estimators 
(which are biased but robust estimators of the regression) 
and then to learn a few remaining parameters to combine and correct these estimators, 
in order to minimize the average squared error in an unbiased way in the end.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usage{
rrat(x, y, taus = 0.5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	a data frame of predictors in training data. Similar as design matrix style but without intercept.
%%     ~~Describe \code{x} here~~
}
  \item{y}{
	a numeric vector of response.
%%     ~~Describe \code{y} here~~
}
  \item{taus}{
	candidate quantiles of underlying robust regression.
%%     ~~Describe \code{taus} here~~
}
  \item{\dots}{
	optional parameters to be passed to the low level function \code{rq}.
%%     ~~Describe \code{\dots} here~~
}
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\details{
Moreover than the original RRAT paper, our implementation is extended to be able to conduct feature regularization of LASSO by adding parameter \code{method} as \code{lasso}, 
as well as tuning the value of the additional penalty parameter \code{lambda} in the function.

Such feature regularization is employed in all the underlying base quantile models simultaneously. The architecture and combination of these underlying base quantile models should be designed according to the asymmetric tail pattern of the noise distribution. 

}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\value{
  An object of class \code{rrat}, which is a list with the
  following components:

  \item{taus}{candidate quantiles of underlying robust regression.}
  \item{rrat_coefficients}{a named vector of coefficients}
  \item{rq_coefficients}{a named matrix of coefficients from 
	underlying quantile regressions bases}
  \item{bias_correction}{a number to correct the combinations of underlying 
	quantile regressions bases}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\references{
Takeuchi, Ichiro & Bengio, Y. & Kanamori, Takafumi. (2002). ``Robust Regression with Asymmetric Heavy-Tail Noise Distributions''. 
\emph{Neural computation}. 14. 2469-96. 10.1162/08997660260293300. 
%% ~put references to the literature/web site here ~
}
\author{
Yi He and Yuelin Zhao
%%  ~~who you are~~
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

rrat_1 <- rrat(x=iris[,2:4], y=iris[,1], taus = 0.5)

}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression} % use one of  RShowDoc("KEYWORDS")
\keyword{robust}
