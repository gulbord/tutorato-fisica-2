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

    list(grades = grades, weights = weights)
}

chim <- get_scores("voti-chi.txt")
chim_ind <- get_scores("voti-chi-ind.txt")

# format for printing
chim$grades$media <- format(chim$grades$media, digits = 2)
chim$grades[is.na(chim$grades)] <- "SV"
chim_ind$grades$media <- format(chim_ind$grades$media, digits = 2)
chim_ind$grades[is.na(chim_ind$grades)] <- "SV"

write.table(
    chim$grades, "voti-chi.csv",
    quote = FALSE, row.names = FALSE
)
write.table(
    format(chim$weights, digits = 3), "pesi-chi.csv",
    quote = FALSE, col.names = "media"
)
write.table(
    chim_ind$grades, "voti-chi-ind.csv",
    quote = FALSE, row.names = FALSE
)
write.table(
    format(chim_ind$weights, digits = 3), "pesi-chi-ind.csv",
    quote = FALSE, col.names = "media"
)
