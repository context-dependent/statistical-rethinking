data(penguins, package = "palmerpenguins")

d <- penguins[complete.cases(penguins), ] |>
    dplyr::mutate(
        bill_length_z = (bill_length_mm - mean(bill_length_mm)) /
            sd(bill_length_mm),
        bill_depth_z = (bill_depth_mm - mean(bill_depth_mm)) /
            sd(bill_depth_mm)
    )

m <- lm(bill_length_z ~ 0 + bill_depth_z, d)

d |> readr::write_csv("R/unit-05/penguins.csv")
