## Open research links for the papers in _data/papers.bib: where the code is,
## the preprint, published peer review and a registered protocol. Each is
## read from public metadata and the full text of the paper, so they are
## suggestions for whoever reviews the weekly pull request, not settled facts.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

preprint_servers <- c(
  "medRxiv", "bioRxiv (Cold Spring Harbor Laboratory)", "bioRxiv", "arXiv",
  "arXiv (Cornell University)", "OSF Preprints (OSF Preprints)",
  "Research Square (Research Square)"
)

## Journals that publish the reviews of every paper
open_review_journals <- c(
  "eLife", "Wellcome Open Research", "F1000Research", "Gates Open Research",
  "The Journal of Open Source Software", "Journal of Open Source Software"
)

## JATS sub-articles that hold published reviews
review_subarticles <- c(
  "aggregated-review-documents", "decision-letter", "referee-report",
  "reviewer-report", "editor-report"
)

## The APIs drop the odd request in a run over many papers
retry <- function(f) {
  for (i in 1:4) {
    out <- tryCatch(f(), error = function(e) NULL)
    if (!is.null(out)) return(out)
    Sys.sleep(2 * i)
  }
  NULL
}

get_json <- function(u) {
  retry(function() jsonlite::fromJSON(u, simplifyVector = FALSE))
}

get_text <- function(u) {
  retry(function() {
    con <- url(u, headers = c("User-Agent" = "epiforecasts-website"))
    on.exit(close(con))
    paste(readLines(con, warn = FALSE), collapse = "\n")
  })
}

is_rxiv <- function(doi) grepl("^10\\.(1101|64898)/", doi)

## Full text as JATS XML, where a public source has it: Europe PMC for papers
## in PubMed Central, the bioRxiv and medRxiv API for preprints there, and the
## eLife API for eLife
full_text <- function(doi) {
  hits <- get_json(sprintf(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=DOI:%%22%s%%22&format=json&resultType=lite",
    utils::URLencode(doi, reserved = TRUE)
  ))
  pmcid <- NULL
  for (r in hits$resultList$result) {
    if (!is.null(r$pmcid)) {
      pmcid <- r$pmcid
      break
    }
  }
  if (!is.null(pmcid)) {
    x <- get_text(sprintf(
      "https://www.ebi.ac.uk/europepmc/webservices/rest/%s/fullTextXML", pmcid
    ))
    if (!is.null(x)) return(x)
  }
  if (is_rxiv(doi)) {
    for (server in c("medrxiv", "biorxiv")) {
      d <- get_json(paste0("https://api.biorxiv.org/details/", server, "/", doi))
      n <- length(d$collection)
      if (n > 0) return(get_text(d$collection[[n]]$jatsxml))
    }
  }
  if (grepl("^10\\.7554/elife\\.", doi)) {
    id <- sub("^10\\.7554/elife\\.([0-9]+).*", "\\1", doi)
    return(get_text(paste0("https://api.elifesciences.org/articles/", id)))
  }
  NULL
}

plain_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", x))

## Papers say where their code is in an availability statement, so links
## there come before links anywhere else in the text
availability_text <- function(x) {
  plain <- plain_text(x)
  hits <- gregexpr(
    "(?i)(code|data|software) (and (code|data) )?availability|availability of (data|code)|data and code|underlying data|software availability",
    plain,
    perl = TRUE
  )[[1]]
  if (hits[1] == -1) return("")
  paste(vapply(hits, function(h) substr(plain, h, h + 600), ""), collapse = " ")
}

code_link <- function(x) {
  if (is.null(x)) return(NA_character_)
  pick <- function(txt) {
    gh <- regmatches(
      txt,
      gregexpr("https?://(www\\.)?github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", txt)
    )[[1]]
    gh <- sub("[.]$", "", gh)
    gh <- gh[!grepl("github\\.com/(apps|orgs|sponsors)/", gh)]
    if (length(gh) > 0) return(sub("^http://", "https://", gh[1]))
    zenodo <- regmatches(
      txt,
      gregexpr("https?://(dx\\.)?doi\\.org/10\\.5281/zenodo\\.[0-9]+|https?://zenodo\\.org/records?/[0-9]+", txt)
    )[[1]]
    if (length(zenodo) > 0) return(zenodo[1])
    NA_character_
  }
  found <- pick(availability_text(x))
  if (is.na(found)) found <- pick(x)
  found
}

## A protocol on OSF that the text around the link calls registered
registered_link <- function(x) {
  if (is.null(x)) return(NA_character_)
  plain <- plain_text(x)
  starts <- gregexpr("https?://osf\\.io/[a-z0-9]{5}", plain)[[1]]
  if (starts[1] == -1) return(NA_character_)
  for (s in starts) {
    context <- substr(plain, max(1, s - 300), s + 40)
    if (grepl("(?i)protocol|pre-?regist|registered|registry", context, perl = TRUE)) {
      rest <- substr(plain, s, s + 40)
      return(regmatches(rest, regexpr("https?://osf\\.io/[a-z0-9]{5}", rest)))
    }
  }
  NA_character_
}

## Suggested links for one paper, as the fields of its entry in
## _data/paper-open.yml
suggest_paper_meta <- function(doi) {
  doi <- tolower(doi)
  work <- get_json(paste0("https://api.openalex.org/works/doi:", doi))
  journal <- work$primary_location$source$display_name %||% ""
  is_preprint <- identical(work$type, "preprint") || is_rxiv(doi) ||
    grepl("^10\\.48550/", doi)
  preprint <- NULL
  if (!is_preprint) {
    for (l in work$locations) {
      if ((l$source$display_name %||% "") %in% preprint_servers &&
            !is.null(l$landing_page_url)) {
        preprint <- sub("^https?://(dx\\.)?doi\\.org/", "https://doi.org/", l$landing_page_url)
        break
      }
    }
  }
  x <- full_text(doi)
  sub_types <- if (is.null(x)) character(0) else
    sub('article-type="(.*)"', "\\1", regmatches(x, gregexpr('article-type="[a-z-]+"', x))[[1]])
  open_review <- !is_preprint && (journal %in% open_review_journals ||
    any(sub_types %in% review_subarticles) ||
    (!is.null(x) && grepl("(?i)peer review file", x, perl = TRUE)))
  code <- code_link(x)
  registered <- registered_link(x)
  meta <- list(
    code = if (!is.na(code)) code,
    preprint = preprint,
    open_review = if (open_review) TRUE,
    registered = if (!is.na(registered)) registered
  )
  Filter(Negate(is.null), meta)
}

## The published version of a preprint, from Crossref's is-preprint-of
## relation, the published DOI medRxiv and bioRxiv record, or OpenAlex, which
## merges a preprint into its published version and so often knows first.
## Matching on these links rather than on titles catches papers whose title
## changed.
published_version <- function(doi) {
  doi <- tolower(doi)
  work <- get_json(paste0("https://api.openalex.org/works/doi:", doi))
  merged <- tolower(sub("^https://doi.org/", "", work$doi %||% ""))
  if (nzchar(merged) && merged != doi && !identical(work$type, "preprint")) {
    return(merged)
  }
  cr <- get_json(paste0("https://api.crossref.org/works/", doi))
  rel <- cr$message$relation$`is-preprint-of`
  for (r in rel) {
    if (identical(r$`id-type`, "doi")) return(tolower(r$id))
  }
  if (is_rxiv(doi)) {
    for (server in c("medrxiv", "biorxiv")) {
      d <- get_json(paste0("https://api.biorxiv.org/pubs/", server, "/", doi))
      for (p in d$collection) {
        if (!is.null(p$published_doi) && nzchar(p$published_doi)) {
          return(tolower(p$published_doi))
        }
      }
    }
  }
  NA_character_
}

read_paper_meta <- function(path = "_data/paper-open.yml") {
  if (!file.exists(path)) return(list())
  yaml::read_yaml(path) %||% list()
}

## Entries are written one per DOI, in the order given, below the comment
## block that opens the file
write_paper_meta <- function(meta, path = "_data/paper-open.yml") {
  lines <- readLines(path)
  header <- lines[seq_len(match(TRUE, startsWith(lines, '"'), nomatch = length(lines) + 1) - 1)]
  body <- unlist(lapply(names(meta), function(doi) {
    fields <- meta[[doi]]
    if (length(fields) == 0) return(sprintf('"%s": {}', doi))
    c(
      sprintf('"%s":', doi),
      vapply(names(fields), function(f) {
        v <- fields[[f]]
        sprintf("  %s: %s", f, if (isTRUE(v)) "true" else v)
      }, "")
    )
  }))
  writeLines(c(header, body), path)
}
