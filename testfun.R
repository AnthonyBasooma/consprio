library(consprio)
GBIF<- GBIF
testhab<- read.csv(file = "habitat.csv", header = T, strip.white = T)
testdata<- read.csv(file = "tests.csv", header = T, strip.white = T)
testfn <-function(data, cols, weights, area, spp, params, n=7){
  spp      <- enquo(spp)
  area     <- enquo(area)
  weights  <- enquo(weights)

  if(n>7){
  stop("The argument 'n' is IUCN status which  must not be greater than 7 or zero")
  }else if(n==0){
  stop("The argument 'n' is IUCN status which  must not be greater than 7 or zero")
  } else if(n<7){
  warning("The argument 'n'  must be equal to IUCN status considered. The default value is 7")
  }
  if(n !=round(n)){
    rlang::abort("The argument 'n' must be an integer of 1 to 7",
                 .class = "testfun_error",
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
    stop("check the conservation priotity names")
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


CPIf2 <- conswat(GBIF, cols = c(category, IUCN, sa, species, wb), weights = IUCN,
                 area = sa, spp = species, params = c(category, wb, sa), n=7)


testFive_7 <- conswat(testdata , cols = c(iucn, area, spp, wb), weights = iucn,
                   area = area, spp = spp)

dataf<- GBIF
dataf<- conwat(GBIF, cols = c(category, IUCN, sa, species, wb), weights = IUCN,
       area = sa, spp = species, params = c(category, wb, sa), n=7)


"The IUCN status names should be well written in the main data"

