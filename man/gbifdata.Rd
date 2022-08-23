\name{gbif}
\alias{gbif}

\title{
Loads a dataset that was used in this packages
}
\description{
It produces a dataset conatining the lakes from Uganda as obatined from the Global Biodiversity Information Facility (GBIF) as of 20th/May /2020.
}
\usage{
gbifdata(x)
}

\arguments{
  \item{catergory}{defines the nature of the lakes depending on size (i.e., lakes with >200km2 is large and <200km2 as  a small lakes)}
\item{waterbody}{the name of the water identified in the dataset}
\item{Area}{the surface of the waterbody in question}
\item{species}{the species name (both genus and species names) for a particular species}
\item{IUCN}{the conservation status of the species as defined by the International Union for Conservation of Nature redlist}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{comp1 }{a dataframe with waterbody category, waterbody, species, species conservation status and surface area of the lakes }

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Anthony Basooma, Ecological Modeller (bas4ster@gmail.com)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{conwatplot}},
 \code{\link{conwat}}
}
\examples{
data(gbif)
\donttest{conwatplot(gbif, "waterbody", "Area")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
