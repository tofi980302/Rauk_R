#' ---
#' title: "Live code from RaukR"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' Use `rmarkdown::render()` on this `.R` file or, in RStudio, click on the
#' "Compile Report" spiral-notebook icon.
#'
#' ## Where to find this document
#'
#' Shortlink humans can type:
#'
#'   * [pos.it/jenny-live-code](https://pos.it/jenny-live-code) *new, but I
#'     think/hope it's working well*
#'   * [rstd.io/jenny-live-code](https://rstd.io/jenny-live-code) *old, but
#'     seems to still work*
#'
#' ## How this all works
#'
#' Horrible link that the shortlink actually points to:
#'
#'   * <https://dl.dropboxusercontent.com/scl/fi/rvn9czis34gnygo32spe7/jenny-live-code.R?rlkey=aeeipr96ovoqiroo6iicfhkay&dl=0>
#'
#' Uses the `raw=1` query trick for rendering a DropBox-hosted file in the
#' browser:
#'
#'   * <https://help.dropbox.com/share/force-download>
#'   * 2023/2024 changes in the structure of DropBox links have changed things,
#'     but the `dl.dropboxusercontent.com` link seems to have the desired
#'     behaviour *even though it lacks the actual `raw=1` query parameter*. I
#'     learned about this URL style from
#'     [a DropBox forum post](https://www.dropboxforum.com/t5/View-download-and-export/Shared-Link-quot-scl-quot-to-quot-s-quot/m-p/695266/highlight/true#M49151)
#'   * I got this whole idea years ago from [Michael Levy](https://twitter.com/LevyEclectic)
#'
#' Live coding workflow:
#'
#'   * I work in this R script locally. I save often.
#'   * This file lives in a directory synced to DropBox.
#'   * You open the DropBox file at
#'     [pos.it/jenny-live-code](https://pos.it/jenny-live-code) and refresh as
#'     needed.
#'   * Should allow you to see, copy, paste everything I've typed and save the
#'     entire transcript at the end. This file is highly perishable, so save
#'     your own copy if you want it.
#'   * Every now and then the refresh won't work. Just re-open from from the
#'     pos.it link: [pos.it/jenny-live-code](https://pos.it/jenny-live-code)
#'
#+ setup, include = FALSE
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)

#+ live-code

library(tidyverse)
library(repurrrsive)
library(gapminder)

africa <- gapminder[gapminder$continent == "Africa", ]
africa_mm <- max(africa$lifeExp) - min(africa$lifeExp)

americas <- gapminder[gapminder$continent == "Americas", ]
americas_mm <- max(americas$lifeExp) - min(americas$lifeExp)

asia <- gapminder[gapminder$continent == "Asia", ]
asia_mm <- max(asia$lifeExp) - min(africa$lifeExp)

europe <- gapminder[gapminder$continent == "Europe", ]
europe_mm <- max(europe$lifeExp) - min(europe$lifeExp)

oceania <- gapminder[gapminder$continent == "Oceania", ]
oceania_mm <- max(europe$lifeExp) - min(oceania$lifeExp)

cbind(
  continent = c("Africa", "Asias", "Europe", "Oceania"),
  max_minus_min = c(africa_mm, americas_mm, asia_mm,
                    europe_mm, oceania_mm)
)

gapminder %>%
  group_by(continent) %>%
  summarize(max_minus_min = max(lifeExp) - min(lifeExp))

got_chars
View(got_chars)

str(got_chars)
str(got_chars, list.len = 4, max.level = 1)

got_chars[9]
got_chars[[9]]

str(got_chars[9], list.len = 4)
str(got_chars[[9]], list.len = 4)

# how many aliases does each got char have?
daenerys <- got_chars[[9]]
daenerys[["aliases"]]
length(daenerys[["aliases"]])

asha <- got_chars[[13]]
asha[["aliases"]]
length(asha[["aliases"]])

map(got_chars, \(x) length(x[["aliases"]]))
map(got_chars, \(x) length(x[["titles"]]))
map(got_chars, \(x) length(x[["allegiances"]]))
map(got_chars, \(x) length(x[["books"]]))
map(got_chars, \(x) length(x[["povBooks"]]))
map(got_chars, \(x) length(x[["tvSeries"]]))

View(sw_films)
map(sw_films, \(x) length(x[["planets"]]))
map(sw_starships, \(x) length(x[["films"]]))

# What's each character's name?
map_chr(got_chars,\(x) x[["name"]])
map_chr(sw_people, \(x) x[["name"]])

# What color is each SW character's hair?
map_chr(sw_people, \(x) x[["hair_color"]])

# Is the GoT character alive?
map_lgl(got_chars, \(x) x[["alive"]])

# Is the SW character female?
map_lgl(sw_people, \(x) x[["gender"]] == "female")

# How heavy is each SW character?
map_chr(sw_people, \(x) x[["mass"]])
sw_people |>
  map_chr(\(x) x[["mass"]]) |>
  parse_number(na = "unknown")


map_chr(got_chars, "name")
map_chr(got_chars, 3)

map_chr(got_chars, "titles")

View(sw_vehicles)

sw_vehicles |>
  map_chr(list("pilots", 1), .default = NA)

map_chr(got_chars, "name")

got_chars_named <- set_names(got_chars, map_chr(got_chars, "name"))

got_chars_named |>
  map_lgl("alive")

allegiances <- map(got_chars_named, "allegiances")
tibble::enframe(allegiances, value = "allegiances")

got_chars |>
  transpose() |>
  enframe() |>
  rowwise() |>
  mutate(
    lengths = list(lengths(value)),
    min_len = min(lengths),
    max_len = max(lengths)
  ) |>
  select(name, ends_with("_len")) |>
  filter(min_len != max_len)

# Good candidates for exploration!
#
#   name        min_len max_len
#   <chr>         <int>   <int>
# 1 titles            1       5
# 2 aliases           1      16
# 3 allegiances       0       3
# 4 books             0       5
# 5 povBooks          1       5
# 6 tvSeries          1       6
# 7 playedBy          1       3

got_chars_named |>
  map_chr(list("povBooks", 1))

got_chars_named |>
  map_chr(list("allegiances", 1), .default = "none")

# Which SW film has the most characters?
sw_films |>
  set_names(map_chr(sw_films, "title")) |>
  map("characters") |>
  map_int(length) |>
  sort(decreasing = TRUE) |>
  _[1]

# Which SW species has the most possible eye colors?
View(sw_species)
sw_species |>
  set_names(map_chr(sw_species, "name")) |>
  map("eye_colors") |>
  discard(\(x) x %in% c("unknown", "n/a")) |>
  map(\(x) str_split_1(x, ",\\s*")) |>
  map_int(length) |>
  sort(decreasing = TRUE) |>
  _[1]

# Which GoT character has the most allegiances? Aliases? Titles?
# I'm skipping this one!

# Which GoT character has been played by multiple actors?
got_chars_named |>
  map("playedBy") |>
  keep(\(x) length(x) > 1)

# Smush together all of the books/povBooks for each character
got_chars_named |>
  map("titles") |>
  map(\(x) str_flatten_comma(x)) |>
  map_chr(\(x) if (x == "") "-none-" else x) |>
  enframe()
