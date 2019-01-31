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
    df.tri1 <- lapply(seq(0, y - 1), function(j) {
        lapply(seq(0, x - 1), function(i) {
            tmp1 <- data.frame(
                x = c(0, 0, 1) + i,
                y = c(0, 1, 1) + j
            )
            list(tmp1)
        })
    })
    df.tri1 <- unlist(df.tri1, recursive = FALSE)
    df.tri1 <- unlist(df.tri1, recursive = FALSE)

    df.tri2 <- lapply(seq(0, y - 1), function(j) {
        lapply(seq(0, x - 1), function(i) {
            tmp1 <- data.frame(
                x = c(0, 1, 1) + i,
                y = c(0, 0, 1) + j
            )
            list(tmp1)
        })
    })
    df.tri2 <- unlist(df.tri2, recursive = FALSE)
    df.tri2 <- unlist(df.tri2, recursive = FALSE)

    col1 <- sample(c(col1, col2), x * y, replace = TRUE)
    col2 <- sample(c(col1, col2), x * y, replace = TRUE)

    df.ls <- list(df.tri1, df.tri2, col1, col2, x = x, y = y)

    return(df.ls)
}



# === Workflow ======================================================

## Parameters
tmp  <- ngon_prime2()
x    <- tmp$y
y    <- tmp$y
tri1 <- tmp[[1]]
tri2 <- tmp[[2]]
col1 <- tmp[[3]]
col2 <- tmp[[4]]

## Graphics
par(mar = c(0, 0, 0, 0), pty = "s", bg = NA)
plot.new()
plot.window(
    xlim = c(0, x),
    ylim = c(0, y),
    xaxs = "i",
    yaxs = "i"
)

polygon(
    x = do.call("rbind", tri1)$x,
    y = do.call("rbind", tri1)$y,
    col = col1,
    border = NA
)

# for (i in seq_len(4)) {
#     polygon(
#         x = tri1[[i]]$x,
#         y = tri1[[i]]$y,
#         col = col1[i],
#         border = NA
#     )
# }






















