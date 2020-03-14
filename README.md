[![CRAN Version](https://www.r-pkg.org/badges/version/hasseDiagram)](https://cran.r-project.org/package=hasseDiagram) 
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/hasseDiagram)](https://cran.r-project.org/package=hasseDiagram) 
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

HasseDiagram
============

This project is an [R](http://www.r-project.org "R project") package
providing method for drawing Hasse diagram - visualization of transitive
reduction of a finite partially ordered set.

Installation in R
-----------------

Install [Rgraphviz](https://bioconductor.org/packages/release/bioc/html/Rgraphviz.html "Rgraphviz") (R version 3.6+):

    install.packages("BiocManager")
    BiocManager::install("Rgraphviz")

Or for older versions of R:
    
    source("https://bioconductor.org/biocLite.R")
    biocLite("Rgraphviz")

Then get the latest version of *hasseDiagram* directly from CRAN:

    install.packages("hasseDiagram")
    

Tutorial
--------

Input:  *n* x *n* matrix  that represents partial order of *n* elements, where in *i*-th row and *j*-th column (*[i, j]*) is *TRUE* iff *i*-th element precedes *j*-th one in the ordering.

Exemplary matrix of 8 nodes (elements) with 13 edges (relations):

    data <- matrix(data = FALSE, ncol = 8, nrow = 8)
    data[1, 2] = data[1, 3] = data[1, 4] = data[1, 5] = TRUE
    data[6, 3] = data[6, 4] = data[6, 5] = TRUE
    data[2, 4] = data[2, 5] = TRUE
    data[3, 4] = data[3, 5] = TRUE
    data[4, 5] = TRUE
    data[7, 8] = TRUE
    

To draw Hasse diagram call:

    hasse(data, labels=paste("Node", 1:8, sep="-"))

It results in the following diagram:

![Example](doc/example-1.png?raw=true)


Transitive reduction has been applied by default, but it can be controlled by a parameter. The same data visualized without applying transitive reduction:

    hasse(data, labels=paste("Node", 1:8, sep="-"), parameters=list(transitiveReduction=FALSE))
    

![Example](doc/example-2.png?raw=true)


Nodes with the same parents, same children, and adjacent to each other are clustered by default into groups:

    data <- matrix(data = FALSE, ncol = 4, nrow = 4)
    data[1, 2] = data[1, 3] = data[1, 4] = TRUE
    data[2, 3] = data[3, 2] = TRUE # cluster
    data[2, 4] = TRUE
    data[3, 4] = TRUE

    hasse(data, labels=paste("Node", 1:4, sep="-"))

![Example](doc/example-3.png?raw=true)

This behavior can be switched off:

    hasse(data, labels=paste("Node", 1:4, sep="-"), parameters=list(cluster=FALSE))

![Example](doc/example-4.png?raw=true)

It is also possible to cluster nodes that have common parents and children but are not adjacent to each other:

    data <- matrix(data = FALSE, ncol = 4, nrow = 4)
    data[1, 2] = data[1, 3] = data[1, 4] = TRUE
    data[2, 4] = TRUE
    data[3, 4] = TRUE
    hasse(data, labels=paste("Node", 1:4, sep="-"), parameters=list(clusterNonAdjacent=TRUE))

![Example](doc/example-5.png?raw=true)

The package also allows for controlling shape and labels of nodes, direction of edges, and colors of nodes and edges. See [documentation](https://cran.r-project.org/web/packages/hasseDiagram/hasseDiagram.pdf) for more information.

Credits
-------

The package has been improved thanks to [guido-s](https://github.com/guido-s) and [vsklad](https://github.com/vsklad).

License
-------

The MIT License (MIT)

Copyright (c) 2014-2020 Krzysztof Ciomek

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.