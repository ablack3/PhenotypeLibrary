library(dplyr)

# get phenotype ids
dirs <- list.files("inst") %>%
  stringr::str_subset("\\d+")

# pull phenotype info into a dataframe
tidyPhenotype <- function(id){
  dfPhe <- readr::read_csv(glue::glue("inst/{id}/phenotypeDescription.csv"), col_types = "dcdccc")
  dfCohort <- readr::read_csv(glue::glue("inst/{id}/cohortDescription.csv"), col_types = "ddccd")

  dfPhe %>%
    left_join(dfCohort, by = "phenotypeId") %>%
    mutate(cohortJson = purrr::map(cohortId, ~jsonlite::read_json(glue::glue("inst/{id}/{.}.json")))) %>%
    mutate(cohortSql = purrr::map(cohortId, ~readr::read_file(glue::glue("inst/{id}/{.}.sql"))))
}

# Put all phenotypes into a single dataframe
phelib <- purrr::map_dfr(dirs, tidyPhenotype)

usethis::use_data(phelib)



# verify that all text is valid utf-8
library(dplyr)
phenotypeIds <- list.files("inst") %>%
  stringr::str_subset("\\d+")

jsonFiles <- purrr::map(phenotypeIds, ~paste0(glue::glue("inst/{.}/") , list.files(glue::glue("inst/{.}")))) %>%
  unlist() %>%
  stringr::str_subset("\\.json")

# convert all json files to utf8, only do this once!
# for(f in jsonFiles){
#   readr::read_lines_raw(f) %>%
#     purrr::map(stringi::stri_encode, from = "cp1252", to = "UTF8") %>%
#     unlist %>%
#     readr::write_lines(f)
# }

df <- tibble::tibble(jsonFiles) %>%
  mutate(isutf8 = purrr::map(jsonFiles, ~stringi::stri_enc_isutf8(readr::read_lines_raw(.)))) %>%
  mutate(bad_lines = purrr::map_chr(isutf8, ~paste(which(!.), collapse = ","))) %>%
  filter(bad_lines != "") %>%
  mutate(notutf8 = paste("file:", jsonFiles, "lines:", bad_lines)) %>%
  select(notutf8)

if(nrow(df) != 0){
  message(paste0(c("The following lines contain non-utf8 characters", df$notutf8), collapse = "\n"))
}

# sql
sqlFiles <- purrr::map(phenotypeIds, ~paste0(glue::glue("inst/{.}/") , list.files(glue::glue("inst/{.}")))) %>%
  unlist() %>%
  stringr::str_subset("\\.sql")

df <- tibble::tibble(sqlFiles) %>%
  mutate(isutf8 = purrr::map(sqlFiles, ~stringi::stri_enc_isutf8(readr::read_lines_raw(.)))) %>%
  mutate(bad_lines = purrr::map_chr(isutf8, ~paste(which(!.), collapse = ","))) %>%
  filter(bad_lines != "") %>%
  mutate(notutf8 = paste("file:", sqlFiles, "lines:", bad_lines)) %>%
  select(notutf8)

readr::read_lines_raw('inst/254443000/254443003.json') %>%
  purrr::map(stringi::stri_encode, from = "cp1252", to = "UTF8") %>%
  unlist %>%
  readr::write_lines("work/test.json")

file.edit("work/test.json")

stringi::stri_enc_isutf8(readr::read_lines_raw(glue::glue("inst/{.}/")))

stringi::stri_enc_list(T) %>%
  # stringr::str_subset("ANSI")
  stringr::str_subset("1252")


phelib

file.edit("inst/254443000/254443003.json")

# > The following lines contain non-utf8 characters
# > file: inst/254443000/254443001.json lines: 24,30
# > file: inst/254443000/254443002.json lines: 24,30
# > file: inst/254443000/254443003.json lines: 30
# > file: inst/254443000/254443004.json lines: 64
# > file: inst/378419000/378419003.json lines: 966,1227
# > file: inst/4098597000/4098597001.json lines: 24,30
# > file: inst/4098597000/4098597002.json lines: 24,30
# > file: inst/4101602000/4101602001.json lines: 24,30
# > file: inst/4101602000/4101602002.json lines: 24,30
# > file: inst/4101602000/4101602003.json lines: 23,29
# > file: inst/4101602000/4101602004.json lines: 73
# > file: inst/4137275000/4137275003.json lines: 23
# > file: inst/4164770000/4164770001.json lines: 24,30
# > file: inst/4164770000/4164770002.json lines: 24,30
# > file: inst/4164770000/4164770003.json lines: 63
# > file: inst/4266367000/4266367003.json lines: 173,929
# > file: inst/436642000/436642004.json lines: 81
# > file: inst/437663000/437663003.json lines: 129,885




