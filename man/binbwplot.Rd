\name{binbwplot}
\alias{binbwplot}
\title{binbwplot}
\description{
Produce binbw plot(s) of the given data.
}
\usage{
binbwplot(inData, col = "grey", ylim = NULL, breaks = NULL, main = "BinBwPlot", xlab = "", ylab = "Data_Range", notch = FALSE)
}
\arguments{
  \item{inData}{data frame having two columns}
  \item{col}{Graphical parameters(color of the plot) for the binbwplot }
  \item{ylim}{y limits}
  \item{breaks}{bins of the histogram}
  \item{main}{The main title (on top) of the plot}
  \item{xlab}{X axis label}
  \item{ylab}{Y axis label}
  \item{notch}{Notch in the boxplot}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

exampleData1<-read.csv("example.csv")

\dontrun{binbwplot(exampleData1,
				col="grey",
				ylim=c(0,1),
				breaks=10,
				xlab="X-axis---Characters",
				ylab="Y-axis---Decimal_Values",
				main="Header of the binbwplot")}

exampleData2<-read.csv("random.csv")

\dontrun{binbwplot(exampleData2,
				col="pink",
				ylim=c(1,100),
				breaks=10,
				xlab="X-axis---Characters",
				ylab="Y-axis---Positive_integer_Values",
				main="Header of the binbwplot")}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ package }
