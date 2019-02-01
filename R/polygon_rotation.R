# Title New polygon generation scheme (WIP)


# === Functions (WIP) ===============================================
# dev.off()
# par(pty = "s", mar = c(0.5, 0.5, 0.5, 0.5))
# plot(
#     x = NA,
#     y = NA,
#     xlim = c(-1.5, 1.5),
#     ylim = c(-1.5, 1.5),
#     xlab = "",
#     ylab = "",
#     pch = 16,
#     bg = NA,
#     fg = NA,
#     axes = FALSE
# )
# abline(h = 0, v = 0, col = "gray60")
#
# tri <- matrix(c(0, 1, 1, 0, 0, 1), nrow = 2, byrow = TRUE)
# polygon(x = tri[1, ], y = tri[2, ])
#
#
# tri <- data.frame(
#     x = c(0, 1, 1),
#     y = c(0, 0, 1)
# )

## Rotater function
r <- function(x, y) {
    x_pr <- -y
    y_pr <- x
    return(list(x = x_pr, y = y_pr))
}

## Random color tesselation object - returned as list
ngon_prime2 <- function(x = 2, y = 2, col1 = "#4d4d4d", col2 = "#add8e6") {
    df_tri1 <- lapply(seq(0, y - 1), function(j) {
        lapply(seq(0, x - 1), function(i) {
            list(
                x = c(0, 0, 1) + i,
                y = c(0, 1, 1) + j
            )
        })
    })
    df_tri1 <- unlist(df_tri1, recursive = FALSE)

    df_tri2 <- lapply(seq(0, y - 1), function(j) {
        lapply(seq(0, x - 1), function(i) {
            list(
                x = c(0, 1, 1) + i,
                y = c(0, 0, 1) + j
            )
        })
    })
    df_tri2 <- unlist(df_tri2, recursive = FALSE)

    col1 <- sample(c(col1, col2), x * y, replace = TRUE)
    col2 <- sample(c(col1, col2), x * y, replace = TRUE)

    df_ls <- list(df_tri1, df_tri2, col1, col2, x = x, y = y)

    return(df_ls)
}

rotate <- function(x) t(apply(x, 2, rev))



# === Workflow ======================================================

## Parameters
tmp  <- ngon_prime2(x = 5, y = 5)
x    <- tmp$x
y    <- tmp$y
tri1 <- tmp[[1]]
tri2 <- tmp[[2]]
col1 <- tmp[[3]]
col2 <- tmp[[4]]
rot  <- -90 * (pi / 180)

## Make color matrix
col1 <- matrix(data = col1, nrow = y, ncol = x)
col2 <- matrix(data = col2, nrow = y, ncol = x)

## Graphics
par(mar = c(0, 0, 0, 0), pty = "s", bg = NA)
plot.new()
plot.window(
    xlim = c(-x, x),
    ylim = c(-y, y),
    xaxs = "i",
    yaxs = "i"
)



for (i in seq_len(x * y)) {
    polygon(
        x = tri1[[i]]$x,
        y = tri1[[i]]$y,
        col = as.vector(col1)[i],
        border = NA
    )
    polygon(
        x = tri2[[i]]$x,
        y = tri2[[i]]$y,
        col = as.vector(col2)[i],
        border = NA
    )
    polygon(
        x = tri1[[i]]$x * cos(rot) - tri1[[i]]$y * sin(rot),
        y = tri1[[i]]$x * sin(rot) - tri1[[i]]$y * cos(rot),
        col = as.vector(rotate(col1))[i],
        border = NA
    )
    polygon(
        x = tri2[[i]]$x * cos(rot) - tri2[[i]]$y * sin(rot),
        y = tri2[[i]]$x * sin(rot) - tri2[[i]]$y * cos(rot),
        col = as.vector(rotate(rotate(rotate(col2))))[i],
        border = NA
    )
}






















