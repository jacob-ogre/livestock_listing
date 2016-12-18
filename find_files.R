# BSD_2_clause
#
# A script to find (possible) matches to ESA listing rules that discuss livestock

library(dplyr)
library(ecosscraper)
library(parallel)
library(pdftools)
library(readr)
library(stringr)
# library(tidyverse)
library(tidyr)

BASED <- "/datadrive/data/ESAdocs"
FRDIR <- file.path(BASED, "federal_register")

find_matches <- function(f) {
  live <- "livestock"
  catt <- "cattle"
  shee <- "sheep"
  tram <- "trampl*"
  graz <- "graz*"
  sedi <- "sediment*"
  qual <- "water quality"
  doc <- pdf_text(f)
  live_hit <- sum(unlist(lapply(doc, grep, pattern = live, ignore.case = TRUE)))
  catt_hit <- sum(unlist(lapply(doc, grep, pattern = catt, ignore.case = TRUE)))
  shee_hit <- sum(unlist(lapply(doc, grep, pattern = shee, ignore.case = TRUE)))
  tram_hit <- sum(unlist(lapply(doc, grep, pattern = tram, ignore.case = TRUE)))
  graz_hit <- sum(unlist(lapply(doc, grep, pattern = graz, ignore.case = TRUE)))
  sedi_hit <- sum(unlist(lapply(doc, grep, pattern = sedi, ignore.case = TRUE)))
  qual_hit <- sum(unlist(lapply(doc, grep, pattern = qual, ignore.case = TRUE)))
  npage <- length(doc)
  res <- data_frame(file = f,
                    n_pages_livestock = live_hit,
                    n_pages_sheep = shee_hit,
                    n_pages_cattle = catt_hit,
                    n_pages_trampl = tram_hit,
                    n_pages_graz = graz_hit,
                    n_pages_sedi = sedi_hit,
                    n_pages_qual = qual_hit,
                    n_hits = sum(live_hit, catt_hit, shee_hit, tram_hit,
                                 graz_hit, sedi_hit, qual_hit),
                    n_pages = npage)
  return(res)
}

# fils <- list.files(FRDIR, full.names = TRUE)
# hits <- mclapply(fils, find_matches,
#                  mc.cores = detectCores() - 2,
#                  mc.preschedule = FALSE)
# livestock_hits <- bind_rows(hits)
# write_tsv(livestock_hits, path = "~/livestock_hits.tsv")
# save(livestock_hits, file = "~/livestock_hits.rda")

# At this point, moved to local machine to make joins, etc, easier
load("livestock_hits.rda")
load("ECOS_species_tables_2016-12-17.rda")
fedreg_table$`Document Type` <- NA
fedreg_table <- fedreg_table[, c(1:3, 6, 4:5)]
fr_combo <- rbind(fedreg_table, adddoc_table)

livestock_hits$basename <- basename(livestock_hits$file)
fr_combo$basename <- basename(fr_combo$Doc_Link)
fr_combo$basename <- ifelse(is.na(fr_combo$basename) |
                                  grepl(fr_combo$basename,
                                        pattern = "pdf$|PDF$"),
                                fr_combo$basename,
                                paste0(fr_combo$basename, ".pdf"))
fr_combo$basename <- gsub(fr_combo$basename,
                              pattern = "&",
                              replacement = "and")
fr_combo$basename <- gsub(fr_combo$basename,
                              pattern = " ",
                              replacement = "_")

setdiff(livestock_hits$basename, fr_combo$basename)
setdiff(fr_combo$basename, livestock_hits$basename)

test <- left_join(livestock_hits, fr_combo, by = "basename")
load("TECP_table_2016-12-17.rda")
TE_dom <- filter_domestic(TECP_table) %>% filter_listed()

test_filt <- filter(test, test$Species %in% unique(TE_dom$Scientific_Name))
length(unique(test_filt$Species))

spp_list <- aggregate(Species ~ Doc_Link, data = test_filt, FUN = unique)
live_graze_int <- left_join(spp_list, test_filt, by = "Doc_Link")
live_graze <- distinct(live_graze_int, Doc_Link, .keep_all = TRUE)
live_graze <- select(live_graze, -Species.y, -basename)
names(live_graze)[2] <- "Species"
live_graze$file <- gsub(live_graze$file,
                        pattern = "/datadrive/data/",
                        replacement = "https://defend-esc-dev.org/")
names(live_graze)
livestock_2 <- live_graze[, c(15, 3, 13:14, 12, 11, 4:10, 2, 1, 16)]
names(livestock_2) <- c("Title", "Link", "Date", "FR_Cit",
                       "n_pg", "n_hit", "n_lives*", "n_sheep", "n_cattle",
                       "n_trampl*", "n_graz*", "n_sediment*", "n_water_qual",
                       "Species", "Orig_URL", "Doc_Type")


tmp1 <- filter(livestock_2, !grepl(livestock_2$Title, pattern = "Propos|Notice of Review"))
tmp_fil <- filter(tmp1, grepl(tmp1$Title,
                              pattern = "[Ee]ndangered|[Tt]hreaten|[Ee]nd.|[Tt]hr"))
length(unique(unlist(tmp_fil$Species)))
live_graze <- tmp_fil

missing_spp <- setdiff(TE_dom$Scientific_Name,
                       unique(unlist(tmp_fil$Species)))
TE_tab_miss <- filter(TE_dom, TE_dom$Scientific_Name %in% missing_spp)
TE_miss <- paste0("<a href='", TE_tab_miss$Species_Page, "' target='_blank'>",
                  TE_tab_miss$Scientific_Name, "</a>")
missing_spp <- c(TE_miss, rep(NA, 2))
missing_spp <- matrix(missing_spp, nrow = 5, ncol = 3)

save(live_graze, file = "live_graze.rda")
save(missing_spp, file = "missing_spp.rda")
