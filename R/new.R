#' @name conswat
#'
#' @title Prioritising water bodies for site-based conservation
#' @aliases conswat
#'
#'
#' @description Basooma et al. 2020. Prioritising the selection of a waterbody
#' for conservation based on the fish species diversity, composition, and
#' conservation in Uganda.
#'
#' @param vector
#'
#' @return vector
#'
#' @examples
#'
#' @details The priority index is a novel conservation metric which weights
#' the waterbody depending on the species richness and rareness among the
#' water bodies considered. In addition, the species International Union
#' Conservation for Nature status is weighted where a critically endangered
#' is weighted 5, endangered = 4, vulnerable = 3, near threatened = 2,
#' least concern = 1. Based on the assumption by IUCN that both data deficiecnt
#' and not evaluted should considered threatened until thier status is
#' established, all their weights was 5. The surface area of the waterbody
#'  considerd in the computation.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr group_by
#' @importFrom rlang warn
#' @importFrom rlang abort
#' @importFrom dplyr summarise
#' @importFrom dplyr case_when
#' @importFrom magrittr "%>%"
#'
#' @export
conswat <-function(data, cols, weights, area, spp, params, n=7){
  spp      <- enquo(spp)
  area     <- enquo(area)
  weights  <- enquo(weights)

  if(n>7){
    rlang::abort("The argument 'n' is IUCN status which  must not be greater than 7 or zero",
                 .subclass = "out_of_range",
                 n=n)
  }else if(n==0){
    rlang::abort("The argument 'n' is IUCN status which  must not be greater than 7 or zero",
                 .subclass="out_of_range",
                 n=n)
  } else if(n<7){
    rlang::warn("The argument 'n'  must be equal to IUCN status considered.
                The default value is 7")
  }
  if(n !=round(n)){
    rlang::abort("The argument 'n' must be an integer of 1 to 7",
                 .class = "non_integer",
                 n=n)
  }

  df<- data%>%

    dplyr::mutate(counts=1)%>%

    dplyr::group_by(across({{cols}}))%>%

    dplyr::summarise(len=sum(counts), .groups="drop")%>%

    dplyr::group_by(!!spp) %>%

    dplyr::mutate(cts=1, sumfreq= sum(cts))%>%

    dplyr::filter(!is.na(!!area), !is.na(!!spp))%>%

    dplyr::mutate(wts = case_when(!!weights=="CE"~5,
                                  !!weights=="critically endangered"~5,
                                  !!weights=="CR"~5,
                                  !!weights=="Critically endangered"~5,
                                  !!weights=="EN"~4,
                                  !!weights=="endangered"~4,
                                  !!weights=="Endangered"~4,
                                  !!weights=="ED"~4,
                                  !!weights=="VU"~3,
                                  !!weights=="Vulnerable"~3,
                                  !!weights=="Near threatened"~2,
                                  !!weights=="Near Threatened"~2,
                                  !!weights=="near threatened"~2,
                                  !!weights=="NT"~2,
                                  !!weights=="LC"~1,
                                  !!weights=="Least Concern"~1,
                                  !!weights=="least concern"~1,
                                  !!weights=="lc"~1,
                                  !!weights=="DD"~5,
                                  !!weights=="Data Deficient"~5,
                                  !!weights=="data deficient"~5,
                                  !!weights=="dd"~5,
                                  !!weights=="NE"~5,
                                  !!weights=="Not Evaluated"~5,
                                  !!weights=="UE"~5,
                                  !!weights=="not evaluated"~5,
                                  !!weights=="Not evaluated"~5),
                  rare= cut(sumfreq,
                            breaks=c(1, 1.9, 3.1, 5.1, 10.1, 44),
                            labels=c(5,4, 3, 2, 1),
                            right=FALSE),
                  rare= as.numeric(as.character(rare)))

  df2<- dplyr::pull(df, wts)

  df3 <- sapply(df2, function(df2)all(is.na(df2)))

  if(any(df3)){
    rlang::abort("The IUCN status names should be well written in the main data")
  }
  else{
    df%>%
      dplyr::mutate(PIs= cts*wts*rare)%>%

      dplyr::group_by(across({{params}}))%>%

      dplyr::summarise(CPIs = sum(PIs), .groups="drop")%>%

      dplyr::mutate(PIndex = CPIs/(!!area*n))%>%

      dplyr::select(-c(CPIs))

  }
}

#' @keywords conservation priority index
#'
#' @references Basooma et al. 2020. Using the novel priority index in prioritising
#' the selection of inland water bodies for site-based fish conservation.
#' Conservation Biology, 1, (2), 123-134.
NULL
