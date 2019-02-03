# Title New polygon generation scheme (WIP)


# === Functions (WIP) ===============================================

## Random color tesselation object - returned as list
quiltr <- function(x, y, ...) {
    ls_tri <- lapply(seq(0, y - 1), function(j) {
        lapply(seq(0, x - 1), function(i) {
            tri1 = list(
                x = c(0, 0, 1) + i,
                y = c(0, 1, 1) + j,
                color = sample(
                    x = c(col1, col2),
                    size = 1,
                    prob = c(0.3, 0.7)
                )
            )
            tri2 = list(
                x = c(0, 1, 1) + i,
                y = c(0, 0, 1) + j,
                color = sample(
                    x = c(col1, col2),
                    size = 1,
                    prob = c(0.7, 0.3)
                )
            )
            return(list(tri1 = tri1, tri2 = tri2))
        })
    })
    ls_tri <- unlist(ls_tri, recursive = FALSE)
    return(ls_tri)
}

col1 <- "lightblue"
col2 <- "lightyellow"



# === Workflow ======================================================

## Parameters
x   <- 5
y   <- 5
tri <- tri_prime(x, y)

## Graphics
par(mar = c(0, 0, 0, 0), pty = "s", bg = NA)
plot.new()
plot.window(
    xlim = c(-x, x),
    ylim = c(-y, y),
    xaxs = "i",
    yaxs = "i",
    bg = "white"
)

for (i in seq_len(x * y)) {
    for (j in seq_len(2)) {
        polygon(
            x = tri[[i]][[j]]$x,
            y = tri[[i]][[j]]$y,
            border = NA,
            col = tri[[i]][[j]]$color
        )
        polygon(
            x = tri[[i]][[j]]$y,
            y = -tri[[i]][[j]]$x,
            col = tri[[i]][[j]]$color,
            border = NA
        )
        polygon(
            x = -tri[[i]][[j]]$x,
            y = -tri[[i]][[j]]$y,
            col = tri[[i]][[j]]$color,
            border = NA
        )
        polygon(
            x = -tri[[i]][[j]]$y,
            y = tri[[i]][[j]]$x,
            col = tri[[i]][[j]]$color,
            border = NA
        )
    }
}




# for (i in seq_len(x * y)) {
#     polygon(
#         x = tri1[[i]]$x,
#         y = tri1[[i]]$y,
#         col = as.vector(t(col1))[i],
#         border = NA
#     )
#     polygon(
#         x = tri1[[i]]$x,
#         y = -tri1[[i]]$y,
#         col = as.vector(t(col1))[i],
#         border = NA
#     )
#     polygon(
#         x = -tri1[[i]]$x,
#         y = -tri1[[i]]$y,
#         col = as.vector(t(col1))[i],
#         border = NA
#     )
#     polygon(
#         x = -tri1[[i]]$x,
#         y = tri1[[i]]$y,
#         col = as.vector(t(col1))[i],
#         border = NA
#     )
#     polygon(
#         x = tri2[[i]]$x,
#         y = tri2[[i]]$y,
#         col = as.vector(t(col2))[i],
#         border = NA
#     )
#     polygon(
#         x = tri2[[i]]$x,
#         y = -tri2[[i]]$y,
#         col = as.vector(t(col2))[i],
#         border = NA
#     )
#     polygon(
#         x = -tri2[[i]]$x,
#         y = -tri2[[i]]$y,
#         col = as.vector(t(col2))[i],
#         border = NA
#     )
#     polygon(
#         x = -tri2[[i]]$x,
#         y = tri2[[i]]$y,
#         col = as.vector(t(col2))[i],
#         border = NA
#     )
# }


















