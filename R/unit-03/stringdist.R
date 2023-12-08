library(stringdist)
library(dplyr)
library(fuzzyjoin)

stringdist("Mark Mckerrow", "Mckerrow Mark", method = "jaccard")
stringdist("Mark Mckerrow", "Thomas McManus", method = "jaccard")

fn_ln <- tibble(
    id = 1:3,
    name = c("Mark Mckerrow", "Alex Rand", "Thomas McManus")
)

ln_fn <- tibble(
    id = 1:3,
    name = c("Mckerrow Mark", "Rand Alex", "McManus Thomas")
)


stringdist_join(fn_ln, ln_fn, by = "name", max_dist = 0.1, method = "jaccard")

jumble_name <- function(s) {
    v <- unlist(strsplit(s, ""))
    x <- paste0(sample(v, length(v)), collapse = "")
    x
}

jumbled <- ln_fn |>
    mutate(name = purrr::map_chr(name, jumble_name))

stringdist_join(jumbled, ln_fn, by = "name", max_dist = 0.1, method = "jaccard")



sep_names <- ln_fn |>
    mutate(
        ln = stringr::str_extract(name, "^.+")
    )
