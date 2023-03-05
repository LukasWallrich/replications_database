# Taken from Lukas W's current work on CiteSource
# https://github.com/ESHackathon/CiteSource/blob/main/R/tables.R


generate_apa_reference <- function(authors, year, title, source, volume, issue, doi, weblink,
                                   return_html = FALSE, format_journal_case = TRUE) {
    id <- seq_along(authors)

  # Extract last names and initials
  citations <- tibble::tibble(id, authors, year, title, source, volume, issue, doi, weblink) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), .fns = ~dplyr::na_if(.x, ""))) %>%
    mutate(last_names = stringr::str_replace_all(authors, "(,.*? and)", "__") %>% stringr::str_remove(",.*$") %>% stringr::str_split("__ "),
           initials = stringr::str_replace_all(authors, "( and.*?, )", "__") %>% stringr::str_remove("^.*?,") %>%
             stringr::str_split("__") %>% purrr::map(~.x %>% stringr::str_remove_all("\\.") %>%
                                                       stringr::str_trim() %>%
                                                       stringr::str_split(pattern = " ") %>%
                                                       purrr::map(stringr::str_trunc, 1, ellipsis = "") %>%
                                                       purrr::map(stringr::str_c, collapse=". ") %>%
                                                       purrr::flatten_chr() %>%
                                                       paste0(".")))
  # Merge initials to names
  citations$initialed_names <-   citations %>% dplyr::select(last_names, initials) %>% as.list() %>%
    purrr::transpose() %>% purrr::map(~paste(.x[[1]], .x[[2]], sep = ", "))

  if (format_journal_case) {
    citations <- citations %>% dplyr::mutate(source = stringr::str_to_title(source))
  }

  unlist_paste <- function(x) {
    x[map_lgl(x, is.null)] <- NA
    map_chr(x, ~paste(.x, collapse = ", "))
  }

  # Flatten remaining lists
  citations <- citations %>% mutate(across(c(where(is.list), -authors), ~.x %>% unlist_paste()))

  # Helper function to deal with missing values
  nNA <- function (x, ..., alt = "", pre = "") {
    ifelse(is.na(x), alt, paste0(pre, x, ...))
  }

  # Compose references

  citations <- citations %>% dplyr::mutate(doi = dplyr::if_else(stringr::str_detect(doi, "http"), doi, paste0("https://doi.org/", doi)),
                                           link = coalesce(doi, weblink))

  if (return_html) {
    citations %>%
      dplyr::rowwise() %>% dplyr::mutate(
        reference = glue::glue("
                              {glue::glue_collapse(initialed_names, ', ', last = ' & ')} ({year}). {title}. {nNA(source, pre = '<i>', '</i>')}{nNA(volume, pre = '<i>, ', '</i>')}{nNA(issue, pre = '(', ')')}. {nNA(link, pre = '<a href = \"', '\">')}{nNA(link, '</a>')}
                                                     ")) %>% dplyr::pull(reference)
  } else {
    citations %>%
      dplyr::rowwise() %>% dplyr::mutate(
        reference = glue::glue("
                              {glue::glue_collapse(initialed_names, ', ', last = ' & ')} ({year}). {title}. {nNA(source)}{nNA(volume, pre = ', ')}{nNA(issue, pre = '(', ')')}. {nNA(link)}
                                                     ")) %>% dplyr::pull(reference)
  }

}


generate_apa_citation <- function(authors, year) {

  id <- seq_along(authors)
  # Extract last names and initials
  processed_names <- tibble::tibble(id = id, authors = authors, year = year) %>%
    mutate(last_names = stringr::str_replace_all(authors, "(,.*? and)", "__") %>% stringr::str_remove(",.*$") %>% stringr::str_split("__ "),
           initials = stringr::str_replace_all(authors, "( and.*?, )", "__") %>% stringr::str_remove("^.*?,") %>%
             stringr::str_split("__") %>% purrr::map(~.x %>% stringr::str_remove_all("\\.") %>%
                                                       stringr::str_trim() %>%
                                                       stringr::str_split(pattern = " ") %>%
                                                       purrr::map(stringr::str_trunc, 1, ellipsis = "") %>%
                                                       purrr::map(stringr::str_c, collapse=". ") %>%
                                                       purrr::flatten_chr() %>%
                                                       paste0(".")))

  # If last name does not uniquely describe authors, first author should be disambiguated in APA style
  # Here implemented by comparing initials -
  # False positives where some sources contain two names (or initials) while others include 1
  # False negative where same initial refers to different names
  # Appears to be best balance for now - further options and instructions could be provided

  last_name <- processed_names %>% dplyr::pull(last_names) %>% unlist()
  initialed_name_list <- processed_names %>% dplyr::select(last_names, initials) %>% as.list() %>%
    purrr::transpose() %>% purrr::map(~paste(.x[[2]], .x[[1]]))

  initialed_name <- initialed_name_list %>% unlist()

  authors <- tibble::tibble(last_name, initialed_name)
  duplicated_last_names <- last_name[duplicated(last_name)]
  # Identify which last names appear with different initials
  to_disambiguate <- authors %>% dplyr::filter(.data$last_name %in% duplicated_last_names) %>%
    dplyr::group_by(.data$last_name) %>%
    dplyr::summarise(disambiguate = dplyr::n_distinct(.data$initialed_name) > 1) %>%
    dplyr::filter(disambiguate == TRUE) %>% dplyr::pull(.data$last_name)

  # Replace those last names with initials
  for (i in seq_along(processed_names$last_names)) {
    if (processed_names$last_names[[i]][[1]] %in% to_disambiguate) {
      processed_names$last_names[[i]][[1]] <- initialed_name_list[[i]][[1]]
    }
  }

  # Create simple citations
  citations <- processed_names %>% dplyr::rowwise() %>%
    dplyr::mutate(N_authors = length(.data$last_names),
                  year = as.character(.data$year),
                  citation = dplyr::case_when(
                    .data$N_authors == 1 ~ glue::glue("{last_names[1]} ({year})"),
                    .data$N_authors == 2 ~ glue::glue("{last_names[1]} & {last_names[2]} ({year})"),
                    .data$N_authors > 2 ~ glue::glue("{last_names[1]} et al. ({year})")
                  )) %>%
    dplyr::ungroup()

  # Disambiguate
  citations_unambiguous <- citations %>%
    dplyr::filter(!(duplicated(citation) | (duplicated(citation, fromLast = TRUE))))

  citations_ambiguous <- citations %>%
    dplyr::filter((duplicated(citation) | (duplicated(citation, fromLast = TRUE))))

  if (nrow(citations_ambiguous) > 0) {

    citations_ambiguous <- purrr::map_dfr(unique(citations_ambiguous$citation), function (current_citation) {
      group <- citations_ambiguous %>% dplyr::filter(.data$citation == current_citation) %>%
        dplyr::mutate(author_names = purrr::map(.data$last_names, stringr::str_c())) %>%
        dplyr::arrange(dplyr::desc(.data$author_names))

      # Case 1: multiple publications by same author(s) in same year - add letters
      if (dplyr::n_distinct(group$author_names) == 1) {
        group %>% dplyr::mutate(year = paste0(year, letters[1:dplyr::n()])) %>% dplyr::rowwise() %>%
          dplyr::mutate(N_authors = length(last_names),
                        citation = dplyr::case_when(
                          .data$N_authors == 1 ~ glue::glue("{last_names[1]} ({year})"),
                          .data$N_authors == 2 ~ glue::glue("{last_names[1]} & {last_names[2]} ({year})"),
                          .data$N_authors > 2 ~ glue::glue("{last_names[1]} et al. ({year})")
                        )) %>%
          dplyr::ungroup()
      } else {
        # Case 2: distinct authors
        #Find maximum number of common authors
        pairs <- combn(group$last_names, 2)
        common <- numeric()
        for (i in seq_len(ncol(pairs))) {
          first <- pairs[[1,i]]
          second <- pairs[[2,i]]
          len <- min(length(first), length(second))
          comparison <- suppressWarnings(first[1:len] == second[1:len])
          #Common authors: either those before first divergence, or all
          common <- c(common, dplyr::coalesce(which(comparison == FALSE)[1]-1, length(comparison)))
        }
        common <- max(common)

        group %>% dplyr::rowwise() %>%
          dplyr::mutate(citation = dplyr::case_when(
            (common < 5 | .data$N_authors < 5) & N_authors < common + 3 ~ glue::glue("{glue::glue_collapse(last_names, ', ', last = ' & ')} ({year})"),
            (common < 5 | .data$N_authors < 5) ~ glue::glue("{glue::glue_collapse(last_names[1:(common+1)], ', ')} et al. ({year})"),
            common >= 5 & .data$N_authors < common + 2  ~ glue::glue("{last_names[1]} ... {last_names[common]}  & {last_names[common+1]} ({year})"),
            common >= 5 & .data$N_authors < common + 3  ~ glue::glue("{last_names[1]} ... {last_names[common+1]}  & {last_names[common+2]} ({year})"),
            common >= 5  ~ glue::glue("{last_names[1]} ... {last_names[common+1]} et al. ({year})")
          )) %>% dplyr::ungroup()
      }
    })

    citations_still_ambiguous <- citations_ambiguous  %>%
      dplyr::filter((duplicated(citation) | (duplicated(citation, fromLast = TRUE))))

    citations_unambiguous <- dplyr::bind_rows(citations_unambiguous, citations_ambiguous  %>%
                                                dplyr::filter(!(duplicated(citation) | (duplicated(citation, fromLast = TRUE)))))

    # If some of Case 2 were in fact Case 1s (e.g., more than 2 authors with same names), they need to be further disambiguated
    citations_still_ambiguous <- citations_still_ambiguous %>% dplyr::group_by(citation) %>%
      dplyr::mutate(letter = letters[1:dplyr::n()]) %>% dplyr::rowwise() %>%
      dplyr::mutate(citation = stringr::str_replace(citation, "([:digit:])\\)", glue::glue("\\1{letter})"))) %>%
      dplyr::ungroup()

    citations_unambiguous <- dplyr::bind_rows(citations_unambiguous, citations_still_ambiguous)

  }
  citations_unambiguous %>%
    dplyr::left_join(tibble::tibble(id), ., by = "id") %>% dplyr::pull(citation)

}
