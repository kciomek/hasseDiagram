#' Draw Hasse diagram
#'
#' This function draws Hasse diagram -- visualization of transitive reduction
#' of a finite partially ordered set.
#' 
#' @param data \emph{n} x \emph{n} matrix, which represents partial order of \emph{n}
#' elements in set. Each cell \code{[i, j]} has value \code{TRUE}
#' iff \emph{i}-th element precedes \emph{j}-th element.
#' @param labels Vector containing labels of elements. If missing or \code{NULL} then
#' \code{data} row names will be used as labels. If \code{rownames(data)} are not present,
#' the labels will be generated as ('a' + element index).
#' @param parameters List with named elements:
#' \itemize{
#' \item \code{arrow} -- direction of arrows: \code{"forward"}, \code{"backward"},
#' \code{"both"} or \code{"none"} (default \code{"forward"}),
#' \item \code{cluster} -- whether to cluster elements which have the same parents
#' and children and are connected all to all (see first commented example) (default \code{TRUE}),
#' \item \code{clusterMerge} -- merge clustered nodes
#' within single node frame (default \code{FALSE}),
#' \item \code{clusterNonAdjacent} -- to allow clustering elements 
#' that are not mutually adjacent (default \code{FALSE}),
#' \item \code{edgeColor} -- edge color, from colors() (default \code{"black"}),
#' \item \code{newpage} -- whether to call \code{grid.newpage()} before drawing
#' (default \code{TRUE}),
#' \item \code{nodeColor} -- node frame color, from colors() (default \code{"black"}),
#' \item \code{margin} -- node margins, a list with 4 numerical items: 
#' \code{"tb"} for top-bottom margin, \code{"rl"} for right-left margin,
#' \code{"otb"} and \code{"orl"} for outer margin when multiple labels are present,
#' \item \code{shape} -- shape of diagram nodes: \code{"roundrect"}, \code{"rect"}
#' or \code{"none"} (default \code{"roundrect"}),
#' \item \code{transitiveReduction} -- whether to perform transitive reduction
#' (default \code{TRUE}).
#' }
#' @examples
#' randomData <- generateRandomData(15, 2, 0.5)
#' hasse(randomData)
#' 
#' # Clustering example
#' data <- matrix(data = FALSE, ncol = 4, nrow = 4)
#' data[1, 2] = data[1, 3] = data[2, 4] = data[3, 4] = TRUE
#' data[2, 3] = data[3, 2] = TRUE
#' hasse(data, c(), list(cluster = TRUE))
#' hasse(data, c(), list(cluster = FALSE))
#'
#' # Hasse to pdf example
#' # randomData <- generateRandomData(15, 2, 0.5)
#' # pdf("path-for-diagram.pdf")
#' # hasse(randomData, NULL, list(newpage = FALSE))
#' # dev.off()
#' @importFrom Rgraphviz agopen
#' @importFrom graph graphAM
#' @importFrom graph subGraph
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom methods as
#' @importFrom grDevices colors
#' @export
hasse <- function(data, labels = c(), parameters = list()) {
  stopifnot(is.matrix(data))
  stopifnot(nrow(data) > 0)
  stopifnot(nrow(data) == ncol(data))
  stopifnot(is.null(labels) || length(labels) == nrow(data))
  stopifnot(is.list(parameters))
  
  # Set defaults
  if (is.null(parameters$newpage)) {
    parameters$newpage <- TRUE
  } else {
  	stopifnot(is.logical(parameters$newpage))
  }
  if (is.null(parameters[["cluster"]])) {
    parameters$cluster <- TRUE
  } else {
	stopifnot(is.logical(parameters$cluster))
  }
  if (is.null(parameters$clusterMerge)) {
    parameters$clusterMerge <- FALSE
  } else {
	stopifnot(is.logical(parameters$clusterMerge))
  }
  if (is.null(parameters$clusterNonAdjacent)) {
    parameters$clusterNonAdjacent <- FALSE
  } else {
	stopifnot(is.logical(parameters$clusterNonAdjacent))
  }
  if (is.null(parameters$transitiveReduction))
    parameters$transitiveReduction <- TRUE
  if (!is.character(parameters$shape))
    parameters$shape <- "roundrect"
  if (is.character(parameters$arrow)) {
	stopifnot(parameters$arrow %in% c("forward", "backward", "both", "none"))
  } else {
    parameters$arrow = "forward"
  }
  if (is.null(parameters$margin)) {
    parameters$margin <- list()
    parameters$margin$rl <- parameters$margin$tb <- 0.125
    parameters$margin$orl <- parameters$margin$otb <- 0.08
  }
  if (is.character(parameters$edgeColor)) {
	stopifnot(parameters$edgeColor %in% colors())
  } else {
  	parameters$edgeColor <- "black"
  }
  if (is.character(parameters$nodeColor)) {
	stopifnot(parameters$nodeColor %in% colors())
  } else {
  	parameters$nodeColor <- "black"
  }
  
  nrNodes <- nrow(data)
  
  # Prepare node identifiers
  if (is.null(rownames(data))) {
    colnames(data) <- rownames(data) <- paste("a", seq_len(nrNodes), sep = "")
  }
  
  # Setup labels if missing
  if (is.null(labels)) {
    labels <- rownames(data)
  }
  
  # Convert labels to list with named elements
  labels <- as.list(labels)
  names(labels) <- rownames(data)
  
  # Remove self-loops 
  for (i in seq_len(nrNodes)) {
    data [i, i] <- FALSE
  }
  
  # Cluster
  groups <- extractGroups(data, parameters$clusterNonAdjacent)
  toRemove <- c()
  
  for (group in groups) {
    for (i in group) {
      for (j in group) {
        data[i, j] <- FALSE
      }
    }
    
    if (parameters$cluster) {
      first <- group[1]
      rest <- group[-1]
      
      rownames(data)[first] <-
        colnames(data)[first] <-
        names(labels)[first] <- paste(rownames(data)[group], collapse = "")
      
      toRemove <- c(toRemove, rest)
      labels[[first]] <- c(unlist(labels[group]))
    }
  }
  
  if (!is.null(toRemove)) {
    data <- data[-toRemove, -toRemove]
    labels <- labels[-toRemove]
  }
  
  nrNodes <- nrow(data)
  
  # Detect cycles
  tmpData <- data
  toVisit <- which(sapply(1:nrow(data), function(x) {length(which(tmpData[, x])) == 0}) == TRUE)
  
  while (length(toVisit) > 0) {
    n <- toVisit[1]
    toVisit <- toVisit[-1]
    
    for (m in which(tmpData[n, ] == TRUE)) {
      tmpData[n, m] <- FALSE
      
      if (length(which(tmpData[, m])) == 0) {
        toVisit <- c(toVisit, m)
      }
    }
  }
  
  notRemovedEdges <- which (tmpData == TRUE, arr.ind = TRUE)
  
  if (nrow(notRemovedEdges) > 0) {
    stop(paste("Cycle detected. Check edges: ",
         paste(sapply(seq_len(nrow(notRemovedEdges)),
                      function(x) { paste(rownames(notRemovedEdges)[notRemovedEdges[x, ]], collapse = "-")} ),
               collapse = ", "),
         sep = ""))
  }
  
  # Perform transitive reduction
  if (parameters$transitiveReduction) {
    for (source in seq_len(nrNodes)) {
      stack <- which(data[source, ])
      visited <- rep(F, nrNodes)
      visited[stack] <- T
      
      while (length(stack) > 0) {
        element <- stack[1]
        stack <- stack[-1]
        
        children <- which(data[element, ])
        for (child in children) {
          data[source, child] = FALSE
          if (!visited[child]) {
            stack <- c(child, stack)
          }
        }
      }
    }
  }
  
  # Calculate node levels
  ranks <- rep(1, nrNodes)
  queue <- which(sapply(1:nrow(data), function(x) {length(which(data[, x])) == 0}) == TRUE)
  distances <- rep(1, length(queue))
  
  while (length(queue) > 0) {
    element <- queue[1]
    queue <- queue[-1]
    dist <- distances[1]
    distances <- distances[-1]
    children <- which(data[element, ])
    
    for (i in seq_len(length(children))) {
      idx <- which(queue == children[i])
      
      if (length(idx) == 0) {
        ranks[children[i]] <- dist + 1
        queue <- c(queue, children[i])
        distances <- c(distances, dist + 1)
      } else {
        distances[idx] <- max(distances[idx], dist + 1)
        ranks[children[i]] <- max(ranks[children[i]], dist + 1)
      }
    }
  }
  
  # Build graph
  graph <- as(graphAM(data, "directed"), "graphNEL")
  
  nAttrs <- list()
  nAttrs$width <- sapply(labels, function(x) { nWi(x, parameters) })
  nAttrs$height <- sapply(labels, function(x) { nHi(x, parameters) })
  nAttrs$fixedsize <- rep(TRUE, nrNodes)
  nAttrs <- lapply(nAttrs, function(x) { names(x) <- rownames(data); x})
  
  subGList <- list()
  
  for (i in seq_len(max(ranks))) {
    subGList[[length(subGList) + 1]] <- list(graph = subGraph(rownames(data)[which(ranks == i)], graph),
                                             cluster = FALSE)
  }
  
  ragraph <- agopen(graph,
                    name = "graph",
                    subGList = subGList,
                    attrs = list(node = list(shape = "box"),
                                 graph = list(rank = "same", rankdir = "TB")),
                    nodeAttrs = nAttrs)
  # Draw graph
  if (parameters$newpage) {
    grid.newpage()
  }
  hGrob <- hasseGrob(ragraph, labels, parameters)
  grid.draw(hGrob)
  #return (hGrob)
}

isGroup <- function(data, i, j, groupNonAdjacent) {
  if ((data[i, j] == TRUE && data[j, i] == TRUE) || groupNonAdjacent == TRUE) {
    iParents <- data[, i]
    jParents <- data[, j]
    iChildren <- data[i, ]
    jChildren <- data[j, ]
    
    iParents[j] <- FALSE
    jParents[i] <- FALSE
    iChildren[j] <- FALSE
    jChildren[i] <- FALSE
    
    if (all(iParents == jParents) && all(iChildren == jChildren)) {
      return (TRUE)
    }
  }
  
  return (FALSE)
}

extractGroups <- function(data, groupNonAdjacent) {
  result <- list()
  itemGroup <- seq_len(nrow(data))
  
  for (i in seq_len(nrow(data))) {
    for (j in seq_len(nrow(data))) {
      if (isGroup(data, i, j, groupNonAdjacent)) {
        iGroup <- which(itemGroup == itemGroup[i])
        mergable <- TRUE
        
        for (k in iGroup) {
          if (k != i) {
            if (!isGroup(data, j, k, groupNonAdjacent)) {
              mergable <- FALSE
              break
            }
          }
        }
        
        if (mergable) {
          itemGroup[j] <- itemGroup[i]
        }
      }
    }
  }
  
  for (g in unique(itemGroup)) {
    items <- which(itemGroup == g)
    if (length(items) > 1) {
      result[[length(result) + 1]] <- items
    }
  }
  
  return (result)
}

# Node height by labels (in inches)
#' @importFrom grid convertY
#' @importFrom grid unit
nHi <- function(labels, parameters) {
  result <- unit(1, "lines") + unit(parameters$margin$tb * 2, "inch")
  if (length(labels) > 1 && parameters$clusterMerge == FALSE)
    result <- result + unit(parameters$margin$otb * 2, "inch")
  
  return (convertY(result, "inches", TRUE))
}

# Node width by labels (in inches)
#' @importFrom grid convertX
#' @importFrom grid unit
#' @importFrom grid stringWidth
nWi <- function(labels, parameters) {
  result <- unit(0, "inch")
  for (label in labels)
    result <- result + stringWidth(label) + unit(parameters$margin$rl * 2, "inch")
  if (length(labels) > 1 && parameters$clusterMerge == FALSE)
    result <- result + (length(labels) + 1) * unit(parameters$margin$orl, "inch")
  
  return (convertX(result, "inches", TRUE))
}

#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom grid grid.rect
#' @importFrom grid grid.roundrect
#' @importFrom grid grid.clip
#' @importFrom grid convertWidth
#' @importFrom grid stringWidth
#' @importFrom grid unit
#' @importFrom grid convertHeight
#' @importFrom grid grid.text
#' @importFrom grid gpar
#' @importFrom grid popViewport
drawNode <- function(x, y, width, height, labels, parameters, isInner=FALSE) {
  vp <- viewport(x,
                 y,
                 width,
                 height,
                 xscale = c(0, nWi(labels, parameters)),
                 yscale = c(0, nHi(labels, parameters)))
  pushViewport(vp)
  
  if (parameters$shape != "none" && (isInner == FALSE || parameters$clusterMerge == FALSE)) {
    gp <- gpar(col = parameters$nodeColor)
  
    if (parameters$shape == "rect")
      grid.rect(gp = gp)
    else if (parameters$shape == "roundrect")
      grid.roundrect(gp = gp)
    else
      stop(paste("Unsupported node shape '", parameters$shape, "'.", sep = ""))
  }
  
  grid.clip()
  
  if (length(labels) == 1) {
    cex <- min(1.0 / (convertWidth(stringWidth(labels) + unit(parameters$margin$rl * 2, "inch"), "npc", TRUE)),
               1.0 / (convertHeight(unit(1, "lines") + unit(parameters$margin$tb * 2, "inch"), "npc", TRUE)))
    
    grid.text(labels[1], gp = gpar(cex = cex))
  }
  else {
    xCenter <- unit(ifelse(parameters$clusterMerge == FALSE, parameters$margin$orl, 0.0), "native")
    yCenter <- unit(0.5, "npc")
    
    for (label in labels) {
      drawNode(xCenter + unit(nWi(label, parameters), "native") * 0.5,
               yCenter,
               unit(nWi(label, parameters), "native"),
               unit(nHi(label, parameters), "native"),
               label,
               parameters,
			   TRUE)
      xCenter <- xCenter + unit(nWi(label, parameters), "native") 
	  if (parameters$clusterMerge == FALSE)
	  	xCenter <- xCenter + unit(parameters$margin$orl, "native")
    }
  }
  
  popViewport()
}


#' @importFrom grid grob
hasseGrob <- function(graph, labels, parameters) {
  grob(graph = graph, labels = labels, parameters = parameters, cl = "hasseGrob")
}

#' @importFrom grid arrow
#' @importFrom grid drawDetails
#' @importFrom grid viewport
#' @importFrom grid unit
#' @importFrom grid pushViewport
#' @importFrom grid grid.lines
#' @importFrom grid popViewport
#' @importFrom Rgraphviz upRight
#' @importFrom Rgraphviz boundBox
#' @importFrom Rgraphviz botLeft
#' @importFrom Rgraphviz getX
#' @importFrom Rgraphviz getY
#' @importFrom Rgraphviz AgNode
#' @importFrom Rgraphviz getNodeCenter
#' @importFrom Rgraphviz getNodeRW
#' @importFrom Rgraphviz getNodeLW
#' @importFrom Rgraphviz getNodeHeight
#' @importFrom Rgraphviz AgEdge
#' @importFrom Rgraphviz bezierPoints
#' @export
drawDetails.hasseGrob <- function(x, ...) {
  g <- x$graph
  ur <- upRight(boundBox(g))
  bl <- botLeft(boundBox(g))
  
  vp <- viewport(width = unit(0.96, "npc"),
                 height = unit(0.96, "npc"),
                 xscale = c(getX(bl), getX(ur)),
                 yscale = c(getY(bl), getY(ur)))
  
  pushViewport(vp)
  
  # Draw nodes
  for (agNode in AgNode(g)) {
    center <- getNodeCenter(agNode)
    centerX <- unit(getX(center), "native")
    centerY <- unit(getY(center), "native")
    width <- unit(getNodeRW(agNode) + getNodeLW(agNode), "native")
    height <- unit(getNodeHeight(agNode), "native")
    
    drawNode(centerX, centerY, width, height, unlist(x$labels[agNode@name]), x$parameters)
  }
  
  # Draw edges
  dir <- x$parameters$arrow
  gp <- gpar(col = x$parameters$edgeColor)
  
  for (edge in AgEdge(g)) {
    nrLines <- length(edge@splines)
	
    for (i in seq_len(nrLines)) {
      arrow <- NULL
      arrowEnds <- NULL
      
      if (dir == "forward" && i == nrLines) {
        arrowEnds = "last"
      }
      else if (dir == "backward" && i == 1) {
        arrowEnds = "first"
      }
      else if (dir == "both") {
        if (nrLines == 1)
          arrowEnds = "both"
        else if (i == 1)
          arrowEnds = "first"
        else if (i == nrLines)
          arrowEnds = "last"
      }
      
      if (!is.null(arrowEnds)) {
        arrow <- arrow(angle = 30,
                       length = min(unit(4, "mm"), unit(0.02, "npc")),
                       ends = arrowEnds,
                       type = "open")
      }
	  
      bp <- bezierPoints(edge@splines[[i]])
      grid.lines(bp[, 1], bp[, 2], default.units = "native", arrow = arrow, gp = gp)
    }
  }
  
  popViewport()
}

#### Random data

#' Generate random data for hasse function
#'
#' This function generates random data for \code{\link{hasse}} function.
#' 
#' @param nrNodes Numer of nodes (\code{0 < nrNodes}).
#' @param minGraphs Minimal number of graphs to generate (\code{0 < minGraphs <= nrNodes}).
#' @param density Value which determines number of edges and shape of graphs (\code{density in [0.0; 1.0]}).
#' @return \code{nrNodes} x \code{nrNodes} matrix.
#' @examples
#' data0_0 <- generateRandomData(15, 2, 0.0)
#' data0_5 <- generateRandomData(15, 2, 0.5)
#' data1_0 <- generateRandomData(15, 2, 1.0)
#' 
#' hasse(data0_0)
#' hasse(data0_5)
#' hasse(data1_0)
#' @export
generateRandomData <- function(nrNodes, minGraphs = 1, density = 0.5) {
  stopifnot(nrNodes > 0)
  stopifnot(minGraphs <= nrNodes)
  stopifnot(density >= 0.0)
  stopifnot(density <= 1.0)
  
  result <- matrix(ncol = 0, nrow = 0)
  p <- nrNodes %/% minGraphs
  
  for (i in seq_len(minGraphs)) {
    size <- p
    if (i == minGraphs)
      size <- nrNodes - (minGraphs - 1) * p
    graph <- generateRandomGraph(size, density)
    
    top <- cbind(result, matrix(data = FALSE, nrow = nrow(result), ncol = ncol(graph)))
    bottom <- cbind(matrix(data = FALSE, nrow = nrow(graph), ncol = ncol(result)), graph)
    result <- rbind(top, bottom)
  }
  
  colnames(result) <- rownames(result) <- paste("a", 1:nrNodes, sep = "")
  
  return (result)
}


#' @importFrom stats runif
generateRandomGraph <- function(nrNodes, density = 0.2) {
  result <- matrix(data = FALSE, nrow = nrNodes, ncol = nrNodes)
  
  for (i in seq_len(nrNodes - 1))
    for (j in (i + 1):nrNodes)
      if (runif(1, 0.0, 1.0) < density)
        result[j, i] <- TRUE
  
  return (result)
}

