## Proportional text bar - not exported
makeTextBar <- function(use,
                        total,
                        charUse = "=",
                        charRem = "-",
                        charLen = 25,
                        charEnd = c("[", "]")) {
    # Description:
    #   Generates a text-based proportion bar for downstream applications
    #
    # Args:
    #   use:     Number of units used in totalal size.
    #   total:   Total number units in totalal size.
    #   charUse: Text character to display used portion of bar. Default is
    #            "#".
    #   charRem: Text character to display unused portion of bar. Default is
    #            " ".
    #   charLen: Total number of characters used for bar. Default is 25 units.
    #   charEnd: "Fancy" end pieces if your want them. Defaults to
    #            c("[", "]").
    #
    # Returns:
    #   A vector containing strings.

    propUse <- use / total
    propRem <- 1 - propUse

    numUse <- round(propUse * charLen, 0)

    if (use == 0) {
        numUse <- 0
    } else if (numUse == 0) {
        numUse <- 1
    } else if (numUse == charLen) {
        numUse <- charLen - 1
    } else {
        numUse
    }

    numRem <- charLen - numUse

    text.bar <- paste0(
        charEnd[1],
        paste(rep(charUse, numUse), collapse = ""),
        paste(rep(charRem, numRem), collapse = ""),
        charEnd[2]
    )

    return(text.bar)
}


## Get color stats
colorStats <- function(x) {
    x <- unlist(x)
    x <- x[grep(pattern = "color", x = names(x))]
    return(as.list(table(x)))
}


## Make text bars for color stats
colorBars <- function(x) {
    x <- colorStats(x)
    cat("Color usage:\n")
    col_n <- names(x)
    col_unl <- unlist(x) * 4

    for (i in seq_along(x)) {
        cat(
            "  ",
            sprintf(paste0("%0-", max(nchar(col_n)), "s"), col_n[i]),
            makeTextBar(
                use = col_unl[i],
                total = sum(col_unl)
            ),
            paste0(
                "(",
                sprintf(
                    paste0("%0", max(nchar(col_unl)), "s"), col_unl[i]
                ),
                ")"
            ),
            "\n"
        )
    }
    cat("---\n")
    cat("Total units:", sum(col_unl))
}







