\name{Sequences}
\alias{Sequences}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create sequences for NONMEM dataset
}
\description{
Functions for making sequences usefull for NONMEM datasets
}
\usage{
nmdataset$SEQ_IDS <- seq_per_id(ds$ID)
nmdataset$SEQ_OCC <- seq_per_occ(ds$ID, ds$OCC)
}

\arguments{
  \item{id_col}{
    Values from the (patient) ID column
}
\item{occ_col}{
    Values from the occasion (OCC) column
}
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
nmdataset$SEQ_IDS <- seq_per_id(ds$ID)
nmdataset$SEQ_OCC <- seq_per_occ(ds$ID, ds$OCC)
}
