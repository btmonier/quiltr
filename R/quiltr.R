#' @title Pattern generator
#'
#' @description This function generates a pattern of isoceles right triangles
#'    of random colors. Patterns of initial triangle "seed" are then cloned
#'    and rotated based on symmetrical patterns.
#'
#' @param x The number of triangles for the seed pattern in the x direction.
#' @param y The number of triangles for the seed pattern in the y direction.
#' @param col A vector of colors for the pattern.
#' @param ... Additional parameters to be passed. For example, probability
#'    weights can be applied here in vector form.
#'
#' @return A list of polygon metadata
#'
#' @examples
#' # Return a quiltr object
#' qds <- quiltr()
#'
#' # Plot the object
#' qds <- quiltr()
#' plot(qds)
#'
#' # Plot with unequal color probabilities
#' qds <- quiltr(
#'     x    = 10,
#'     y    = 10,
#'     col  = c("#DCD0C0", "#C0B283", "#F4F4F4"),
#'     prob = c(0.2, 0.7, 0.1)
#' )
#' plot(qds, sym = "reflect")
#'
#' @export
quiltr <- function(x = 3,
                   y = 3,
                   col = c("#AACBFF", "#E0E0E0"),
                   ...) {
    ls_tri <- lapply(seq(0, y - 1), function(j) {
        lapply(seq(0, x - 1), function(i) {
            tri1 = list(
                x = c(0, 0, 1) + i,
                y = c(0, 1, 1) + j,
                color = sample(
                    x = col,
                    size = 1,
                    replace = FALSE,
                    ...
                )
            )
            tri2 = list(
                x = c(0, 1, 1) + i,
                y = c(0, 0, 1) + j,
                color = sample(
                    x = col,
                    size = 1,
                    replace = FALSE,
                    ...
                )
            )
            return(list(tri1 = tri1, tri2 = tri2))
        })
    })
    ls_tri <- unlist(ls_tri, recursive = FALSE)

    structure(
        ls_tri,
        class = "quiltr",
        x = x,
        y = y,
        col = col
    )
}


#' @param sym Symmetry type. Choose from \code{rotate} or \code{reflect}.
#'
#' @importFrom graphics par plot.new plot.window polygon
#' @importFrom grDevices rgb
#'
#' @export
plot.quiltr <- function(x, sym = c("rotate", "reflect"), ...) {
    sym <- match.arg(sym)
    tri <- x
    x_dim   <- attr(x, "x")
    y_dim   <- attr(x, "y")

    old <- graphics::par(pty = "m", mar = c(3, 1, 3, 1))
    on.exit(par(old))

    graphics::par(mar = c(0, 0, 0, 0), pty = "s", bg = NA)
    graphics::plot.new()
    graphics::plot.window(
        xlim = c(-x_dim, x_dim),
        ylim = c(-y_dim, y_dim),
        xaxs = "i",
        yaxs = "i",
        bg = "white"
    )

    switch(
        EXPR = sym,
        rotate = for (i in seq_len(x_dim * y_dim)) {
            for (j in seq_len(2)) {
                graphics::polygon(
                    x = tri[[i]][[j]]$x,
                    y = tri[[i]][[j]]$y,
                    border = NA,
                    col = tri[[i]][[j]]$color
                )
                graphics::polygon(
                    x = tri[[i]][[j]]$y,
                    y = -tri[[i]][[j]]$x,
                    col = tri[[i]][[j]]$color,
                    border = NA
                )
                graphics::polygon(
                    x = -tri[[i]][[j]]$x,
                    y = -tri[[i]][[j]]$y,
                    col = tri[[i]][[j]]$color,
                    border = NA
                )
                graphics::polygon(
                    x = -tri[[i]][[j]]$y,
                    y = tri[[i]][[j]]$x,
                    col = tri[[i]][[j]]$color,
                    border = NA
                )
            }
        },
        reflect = for (i in seq_len(x_dim * y_dim)) {
            for (j in seq_len(2)) {
                graphics::polygon(
                    x = tri[[i]][[j]]$x,
                    y = tri[[i]][[j]]$y,
                    border = NA,
                    col = tri[[i]][[j]]$color
                )
                graphics::polygon(
                    x = tri[[i]][[j]]$x,
                    y = -tri[[i]][[j]]$y,
                    col = tri[[i]][[j]]$color,
                    border = NA
                )
                graphics::polygon(
                    x = -tri[[i]][[j]]$x,
                    y = -tri[[i]][[j]]$y,
                    col = tri[[i]][[j]]$color,
                    border = NA
                )
                graphics::polygon(
                    x = -tri[[i]][[j]]$x,
                    y = tri[[i]][[j]]$y,
                    col = tri[[i]][[j]]$color,
                    border = NA
                )
            }
        }
    )
}


#' @export
summary.quiltr <- function(object, ...) {
    cat("A quiltr data set:\n")
    cat("  Class.....", class(object),        "\n")
    cat("  Dim (x)...", attr(object, "x"),    "\n")
    cat("  Dim (y)...", attr(object, "y"),    "\n")
}
