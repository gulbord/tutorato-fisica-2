get_scores <- function(file) {
    # read data into data.frame and name columns
    grades <- read.table(
        file,
        header = FALSE,
        dec = ",",
        na.strings = "SV",
        col.names = c("matricola", sprintf("es%0.2d", 1:11))
    )

    # order by student id
    grades <- grades[order(grades$matricola), ]

    # get weights = mean of each exercise (between scores > 0)
    weights <- apply(grades[-1], 2, \(x) mean(x[x > 0], na.rm = TRUE))

    # calculate weighted mean for each student
    grades$media <- apply(
        grades[-1], 1, \(x) weighted.mean(x, ifelse(is.na(x), 0, weights))
    )

    # round to 0, 0.5, 1, 1.5, 2, 2.5, 3 to get the final grade
    grades$voto <- round(grades$media) / 2

    # reorder columns
    last <- ncol(grades)
    grades <- grades[, c(1, last - 1, last, 2:(last - 2))]

    list(grades = grades, weights = weights)
}

df_to_md <- function(df, file) {
    cat(paste(names(df), collapse = "|"), file = file)
    cat("\n", file = file, append = TRUE)
    cat(
        paste(rep("-", ncol(df)), collapse = "|"),
        file = file, append = TRUE
    )
    cat("\n", file = file, append = TRUE)

    for (i in seq_len(nrow(df))) {
        cat(paste(df[i, ], collapse = "|"), file = file, append = TRUE)
        cat("\n", file = file, append = TRUE)
    }
}

print_scores <- function(data, file) {
    data$grades$media <- format(data$grades$media, digits = 2)
    data$grades[is.na(data$grades)] <- "SV"

    df_to_md(data$grades, file)
}

# chimica
get_scores("voti-chi.txt") |> print_scores("voti-chi.md")
# chimica industriale
get_scores("voti-chi-ind.txt") |> print_scores("voti-chi-ind.md")
