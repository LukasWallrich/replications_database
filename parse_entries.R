library(stringr)
library(purrr)
library(glue)
library(dplyr)
library(tibble)
library(rcrossref)
source("reference_helpers.R")

md <- readr::read_file("Reversals_completed.md")
str_squish_mild <- function(string) {
  str_replace_all(string, "(\\s)(\\s)(\\1\\2)+", "\\1\\2") %>%
  str_replace_all("(\\s)\\1+", "\\1") %>% str_trim() %>% str_replace_all("\r\n", "\n")
}

# Remove header
md <- md %>% str_remove(regex(".*\\[TOC\\]", dotall = TRUE))
# Remove further literature
md <- md %>% str_remove(regex("\n## Further literature.*$", dotall = TRUE))

# Split into categories
md_split <- md %>% str_split("\n### ") %>% unlist()


# Remove blank lines and empty categories
md_split <- purrr::map_chr(md_split, str_squish_mild)

md_split <- md_split[str_length(md_split) > 50]

md_split_named <- map(md_split, \(x) {
  name <- name <- str_extract(x, ".*?(?=\\s+---)")
  entries <- x %>% str_remove(".*\\s+---")
  list(entries) %>% set_names(name)
}) %>% flatten() %>% map(~str_split(.x, "\n####") %>% map(str_squish_mild)) %>% flatten() %>% map(\(x) {
  x[str_lengths(x) > 0]
})


effect_to_tibble <- function(md) {
  title <- md %>% str_extract(".*?(?=\\s*\\*)")
  message("Processing ", title)
  md <- md %>% str_remove(".*?\\s*(?=\\*)")
  title2 <- str_extract(md, "(?<=\\*\\s?\n\\*\\*).*?(?=\\*\\*)")
  description <- str_extract(md, "(?<=[[:alpha:])]{1}\\*\\*).*?(?=\n)") %>% str_trim() %>% str_remove_all("(^[:punct:]+\\s*)|[:punct:]+$")

  categories <- c("Status", "Original paper", "Critiques?", "Original effect size", "Replication effect size")

  entry <- md %>% str_remove(regex(".*(?=\n \\* Status:)", dotall = TRUE))

  details <- list()

  for (i in seq_along(categories)) {
    if (i == length(categories)) {
      end <- "$"
    } else {
      end <- "(?=\\* {categories[i+1]})"
    }
    details[categories[i]] <- entry %>% str_extract(regex(as.character(glue(glue("(?<={categories[i]}:).*{end}"))), dotall = TRUE)) %>% str_trim()
  }

#
#   details <- map(entry, \(x) {
#     name <- str_extract(x, ".*?(?=:)")
#     value <- str_remove(x, ".*?: ")
#     list(value) %>% set_names(name)
#   }) %>% flatten() %>% .[str_lengths(.)>0]

tibble::tibble(title, title2, description, !!!details) %>% rename(Critiques = `Critiques?`)
}

effects_df <- md_split_named %>% map_dfr(.id = "category", ~map_dfr(.x, effect_to_tibble)) %>%
  rowid_to_column("effect_id")

str_extract_effect <- function(effect) {
  #Accept if effect is entered directly
  if (str_trim(effect) %in% c("r", "d", "OR")) return(effect %>% str_trim())
  effect_type <- str_extract_all(effect, "( Detection rate )|( PD )|( OR )|(Cohen’s f)|(_\\s?b\\s?_)|(Cramer’s _V_)|(_\\s?d\\s?_)|(_\\s?g\\s?_)|(_\\s?r\\s?_)|(_?\\s?η_?2)|(_?\\s?η<sup>2</sup>\\s?_?)|(_d $)|( d )|( g )|( d$)|( r )|( r$)") %>%
    unlist() %>% str_remove_all("_") %>% str_trim()
  if(length(effect_type) > 1) {
    warning(effect, " cannot be uniquely identified")
    return(NA)
  } else if (length(effect_type) == 0) {
    return(NA)
  }
  effect_type
}

str_extract_effect_size <- function(effect) {
  #ToDos: distinguish eta-sq and partial eta-sq
  # - Decide whether PD is a relevant effect size

  # Remove confidence intervals
  effect <- str_remove(effect, "\\[-?–?−?\\d*\\.\\d+\\s?[:punct:]?\\s?-?–?−?\\d*\\.\\d+\\]") %>%
    str_remove("9\\d%\\s?-?–?−?CIs?\\s*")

    # Remove links
  effect <- str_remove(effect,  "\\[.+\\]\\(.+\\)")

  # Remove ± values
  effect <- str_remove(effect, "±\\s?-?–?−?\\d?.\\d+")

  # Remove years (i.e. length 4 numbers not preceded by .)
  effect <- str_remove_all(effect, "([^\\.]\\d{4})|(^\\d{4})")

  # Remove p-values
  effect <- str_remove_all(effect, "[_ ]{1,2}p[_ ]{1,2}.*?\\.\\d+")


  #Check that only single number remains
  effect_size <- str_extract_all(effect, "-?–?−?\\d*\\.\\d+") %>% unlist()

  if (length(effect_size) > 1) {
    warning("'", effect, "' cannot be parsed. Need to provide a single number.")
    return(NA)
  }
  #Replace longer dashes
  effect_size %>% str_replace("[–−]", "-") %>% as.numeric()
}

parse_effect <- function(entry) {
  map_dfr(entry, function(free_text) {
    if (is.na(free_text)) return(tibble(parsed = FALSE))
  label <- str_extract(free_text, ".*?(?=:)")
  count <- str_count(free_text, "=")
  if (count == 1) {
    effect <- str_split(free_text, "=") %>% flatten()
    effect_type <- str_extract_effect(effect[1])
    effect_size <- str_extract_effect_size(effect[2])
        if (is.na(effect_type)) {
      warning("Effect type not recognised: ", free_text)
      out <- tibble()
    } else {
    out <- tibble(metric_reported = effect_type, size_reported = effect_size)
    }
  } else if (count == 2 & str_detect(free_text, "converted")) {
    effect1 <- str_extract(free_text, ".*?\\.\\d+")
    effect2 <- str_remove(free_text, ".*?\\.\\d+")

    effect1_s <- str_split(effect1, "=") %>% flatten()
    effect_type <- str_extract_effect(effect1_s[1])
    effect_size <- str_extract_effect_size(effect1_s[2])
    if (is.na(effect_type)) {
      warning("Effect type not recognised: ", effect1)
      out <- tibble(metric_reported = NA, size_reported = NA)
    } else {
      out <- tibble(metric_reported = effect_type, size_reported = effect_size)
    }
    effect2_s <- str_split(effect2, "=") %>% flatten()
    effect_type <- str_extract_effect(effect2_s[1])
    effect_size <- str_extract_effect_size(effect2_s[2])
    if (is.na(effect_type)) {
      warning("Effect type not recognised: ", effect2)
    } else {
      out <- cbind(out, tibble(metric_converted = effect_type, size_converted = effect_size))
    }
  } else if (str_detect(free_text, "(not reported)|(N/A)") && !str_detect(free_text, "\\d")) {
    label <- "not reported"
    out <- tibble()
  } else {
    out <- tibble()
  }
  if (nrow(out) == 0) {
    out <- tibble(parsed = FALSE)
  } else {
    out$parsed <- TRUE
  }
  out$label <- label %>% str_trim()
  out
  })
}

original_effects <- effects_df %>% select(effect_id, entered_text = `Original effect size`) %>%
  rowwise() %>%
  # Separate multiple studies/effects (if they are separated by ;)
  summarise(effect_id = effect_id, entered_text = entered_text %>% str_replace_all("&lt;", "&lt") %>% str_split(";") %>% unlist()) %>%
  ungroup() %>%
  mutate(type = "original", parse_effect(entered_text))

replication_effects <- effects_df %>% select(effect_id, entered_text = `Replication effect size`) %>%
  rowwise() %>%
  # Separate multiple studies/effects (if they are separated by ;)
  summarise(effect_id = effect_id, entered_text = entered_text %>% str_replace_all("&lt;", "&lt") %>% str_split(";") %>% unlist()) %>%
  ungroup() %>%
  mutate(type = "replication", parse_effect(entered_text))

safe_cr_cn <- safely(cr_cn)

extract_paper <- function(entries) {
  map_dfr(entries, function (entry) {
  if (str_count(entry, "http") > 1) {
    warning("Two links found - cannot parse more than one: ", entry)
    return(tibble(parsed = FALSE))
  }
  citations <- str_extract(entry, "(?<=\\[)citations.*?(?=\\]\\.?$)")
  entry <- str_remove(entry, "\\[citations.*?\\]\\.?$")
  if (str_detect(entry, "doi")) {
    if (str_detect(entry, "citeseerx.ist.psu.edu")) {
      warning("citeseerx URLs contain a doi parameter, but no actual doi - not extracted")
      return(tibble(parsed = FALSE))
    }
    doi <- str_extract(entry, "(?<=(doi/)|(doi\\.org/)).*?(?=[? )])") %>% str_remove("full/")
  } else {
    return(tibble(parsed = FALSE))
  }
  message("Requesting ", doi)

  rq_time <- Sys.time()

  ref <- safe_cr_cn(doi, format = "citeproc-json-ish")

  #if (difftime(Sys.time(), rq_time, units = "secs") > 10) browser()

  if (!is.null(ref$error)) {
    warning("Crossref error")
    return(tibble(parsed = FALSE))
  }
  if (is.null(ref$result)) {
    warning("Crossref returned nothing - likely timeout")
    return(tibble(parsed = FALSE))
  }

  ref <- ref$result

  message(ref$published$`date-parts`[1])

  notes <- entry %>% str_remove(".*?[( ]\\d{4}[^\\d]")

  authors_string <- paste(ref$author$family, ref$author$given, sep = ", ", collapse = " and ")

  #Is subtitle needed? Was on first example. Can there be a better separator?
  tibble(title = paste(ref$title, ref$subtitle, sep = "."), year = ref$published$`date-parts`[1],
         authors = list(ref$author), authors_string = authors_string, doi = doi, url = ref$URL, publication = ref$`container-title`,
         pages = ref$page, volume = ref$volume, issue = ref$issue,
         citation_count_CR = ref$`is-referenced-by-count`,
         citation_count_entered = citations, description = notes,
         citation = generate_apa_citation(authors_string, year),
         reference = generate_apa_reference(authors_string, year, title, publication, volume, issue, doi, url))
  })
  }

original_papers <- effects_df %>% select(effect_id, original_paper = `Original paper`) %>%
  mutate(extract_paper(original_paper))



critique_papers <-




