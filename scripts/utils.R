vector_paste <- function(..., 
                         sep = " ",
                         collapse = NULL,
                         recycle0 = FALSE) {

    # Make all potential combinations of the vectors
    combos <- expand.grid(
        ...,
        stringsAsFactors = FALSE
    ) 

    # Loop over them and paste them together by row according to specifications
    return(
        sapply(
            1:nrow(combos),
            \(i) combos[i, ] |>
                unlist() |>
                paste(
                    sep = sep,
                    collapse = collapse,
                    recycle0 = recycle0
                )
        )
    )
} 