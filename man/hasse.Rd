\name{hasse}
\alias{hasse}
\title{Draw Hasse diagram}
\usage{
hasse(data, labels = c(), parameters = list())
}
\arguments{
  \item{data}{\emph{n} x \emph{n} matrix, which represents
  partial order of \emph{n} elements in set. Each cell
  \code{[i, j]} has value \code{TRUE} iff \emph{i}-th
  element precedes \emph{j}-th element.}

  \item{labels}{Vector containing labels of elements. If
  \code{NULL} names of rows of parameter \code{data} will
  be used or labels will be generated ('a' + element index)
  in case \code{rownames(data)} is \code{NULL}.}

  \item{parameters}{List with named elements: \itemize{
  \item \code{arrow} -- direction of arrows:
  \code{"forward"}, \code{"backward"} or \code{"both"}
  (default \code{"forward"}), \item \code{cluster} --
  whether to cluster elements which have same parents and
  children and are connected all to all (see first
  commented example) (default \code{TRUE}), \item
  \code{newpage} -- whether to call \code{grid.newpage()}
  before drawing (default \code{TRUE}), \item \code{shape}
  -- shape of diagram nodes: \code{"roundrect"} or
  \code{"rect"} (default \code{"roundrect"}), \item
  \code{transitiveReduction} -- whether to perform
  transitive reduction (default \code{TRUE}). }}
}
\description{
This function draws Hasse diagram -- visualization of
transitive reduction of a finite partially ordered set.
}
\examples{
randomData <- generateRandomData(15, 2, 0.5)
hasse(randomData)

# Clustering example
data <- matrix(data = FALSE, ncol = 4, nrow = 4)
data[1, 2] = data[1, 3] = data[2, 4] = data[3, 4] = TRUE
data[2, 3] = data[3, 2] = TRUE
hasse(data, c(), list(cluster = TRUE))
hasse(data, c(), list(cluster = FALSE))

# Hasse to pdf example
# randomData <- generateRandomData(15, 2, 0.5)
# pdf("path-for-diagram.pdf")
# hasse(randomData, NULL, list(newpage = FALSE))
# dev.off()
}

