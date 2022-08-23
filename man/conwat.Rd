\name{conswat}
\alias{conswat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conservation priority index for a waterbody
}
\description{
The index aids prioritising the selection of a waterbody for conservation and management. the higher the index value, the higher the priority for the waterbody. However, also waterbodies with index values should not be discarded but its applicable in case the resources are limiting.
}
\usage{
conswat(data, cols, weights, area, spp, params, n=7....)
}

\arguments{
  \item{data}{a dataset that will be evaluted to determine waterbody conservation priority index}
  \item{cols}{the columns to be considered: atleast four column names must be selected, namely the waterbodies, surface area of the waterbody, species names column, species IUCN caonservation status, }
  \item{area}{the surface area of each particular waterbody in the dataset. If the surface area of the waterbody is missing, it will be automatically filtered out}
  \item{spp}{species column name to ably group the species according to their number of occurence in the whole dataset }
  \item{iucnwt}{the species weight based on the species IUCN status}
  \item{params}{}
  \item{iucn}{a default value of 7 indicating the IUCN conservation categories}
}
\details{
The priority index is a novel conservation metric which weights the waterbody depending on the species richness and rareness among the water bodies considered. In addition, the species International Union Conservation for Nature status is weighted where a critically endangered is weighted 5, endangered = 4, vulnerable = 3, near threatened = 2, least concern = 1. Based on the assumption by IUCN that both data deficiecnt and not evaluted should considered threatened until thier status is established, all their weights was 5. The surface area of the waterbody is considerd in the computation.
}
\value{
}
\references{
Basooma et al. 2020. Prioritising the selection of a waterbody for conservation based on the fish species diversity, composition, and conservation in Uganda.
}
\author{
Anthony Basooma; Ecological Modelling Expert (bas4ster@gmail.com)
}
\note{

}


\seealso{
}
\examples{
CPI<- conswat(GBIF, cols=c(category,waterbody,species, IUCN, Area), spp=species,
iucn=IUCN, area=Area, params=c(waterbody, Area))
  }
