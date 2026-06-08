# Document analysis for green steel procurement paper
# ------------------------------------------------------------------------------
# Purpose: Quantitative and qualitative analysis of EU and national policy documents.
# Author: Natalie Gravett-Foyn
# Project: Green steel public procurement / lead markets
#
# Notes for reuse:
# - Run from the project root directory (for example, with an RStudio Project).
# - Expected folders include data/, data_intermediate/, outputs/ and figures/.
# - The script does not set a working directory, to keep it portable for GitHub.
# ------------------------------------------------------------------------------

required_packages <- c(
  "quanteda",
  "quanteda.textstats",
  "readtext",
  "pdftools",
  "stringr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "ggwordcloud",
  "viridis",
  "fmsb"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Policy dictionaries
# ------------------------------------------------------------------------------
# Five categories: macro_outcomes, production_pathways, circularity_materials, efficiency, procurement_mechanisms
policy_dictionary <- list(
  macro_outcomes = c(
    "decarbon*", "carbon intens*", "climate neutral*", "net zero", "carbon neutral*",
    "emission reduction*", "greenhouse gas", "ghg reduction*", "co2 reduction*",
    "climate mitigation", "climate resilien*", "strategic autonom*", "resilien*",
    "competitiveness", "industrial competit*", "energy security", "security",
    "supply security", "strategic dependenc*", "strategic value"
  ),

  production_pathways = c(
    "hydrogen", "green hydrogen", "renewable hydrogen",
    "electrolysis", "direct reduced iron", "direct reduction",
    "dri", "h2 dri", "electric arc furnace", "eaf",
    "renewable electricity", "renewable energy",
    "fossil free steel", "low carbon steel", "green steel",
    "near zero steel", "clean steel", "sustainable steel",
    "industrial electrification", "carbon capture", "ccs", "ccu", "zero-carbon steel"
  ),

  circularity_materials = c(
    "circular*", "recycl*", "scrap", "secondary steel",
    "material efficiency", "resource efficiency",
    "life cycle", "lifecycle", "life cycle assessment", "lca",
    "embodied carbon", "embodied emissions",
    "product carbon footprint", "pcf",
    "environmental product declaration", "epd",
    "durabil*", "reuse", "remanufact*"
  ),

  efficiency = c(
    "energy efficiency", "energy efficient",
    "process efficiency",
    "efficiency improvement*", "energy intensity",
    "best available technique*", "bat"
  ),

  procurement_mechanisms = c(
    "public procurement", "green public procurement", "gpp",
    "strategic procurement", "sustainable procurement",
    "most economically advantageous tender", "meat",
    "award criteria", "award criterion", "tender",
    "contracting authorit*",
    "life cycle costing", "lcc",
    "total cost of ownership", "tco",
    "technical specification*", "minimum requirement*",
    "contract performance clause*",
    "environmental criteria", "lead market*", "procurement criteria", "procurement" ,"joint procurement"
  )
)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 2. Optional pilot: three EU documents
# ------------------------------------------------------------------------------
# Load packages

# Read PDFs
docs <- readtext("data/eu_policy_test/*")
head(docs)

# Check PDFs were read properly
nchar(docs$text)

# Minimal cleaning for pdf reading [getting rid of white space, nuls etc]
# Remove NUL characters safely first
clean_text <- function(x) {
  # Convert to raw
  raw_vec <- charToRaw(x)

  # Remove NUL bytes (0x00)
  raw_vec <- raw_vec[raw_vec != as.raw(0)]

  # Convert back to character
  x_clean <- rawToChar(raw_vec)

  # Normalise encoding
  x_clean <- iconv(x_clean, from = "UTF-8", to = "UTF-8", sub = "")

  # Collapse whitespace
  x_clean <- gsub("\\s+", " ", x_clean)

  return(x_clean)
}

#checking clean worked
corp <- corpus(docs, text_field = "text")
toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE)

docs$text <- sapply(docs$text, clean_text)
class(docs$text)

# Create corpus
corp <- corpus(docs, text_field = "text")

toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE
)

#creating compounds within my dictionary so multiword phrases are treated as one
#compounding: circularity
toks <- tokens_compound(
  toks,
  pattern = phrase(c(
    "secondary steel",
    "material efficiency",
    "resource efficiency",
    "life cycle",
    "life cycle assessment",
    "embodied carbon",
    "embodied emissions",
    "product carbon footprint",
    "environmental product declaration"
  ))
)

#compounding: procurement
toks2 <- tokens_compound(
  toks,
  pattern = phrase(c(
    "public procurement",
    "green public procurement",
    "strategic procurement",
    "sustainable procurement",
    "most economically advantageous tender",
    "award criteria",
    "award criterion",
    "contracting authorit*",
    "life cycle costing",
    "total cost of ownership",
    "technical specification*",
    "minimum requirement*",
    "contract performance clause*",
    "environmental criteria",
    "lead market*",
    "procurement criteria",
    "joint procurement"
  ))
)

#compounding: macro
toks2 <- tokens_compound(
  toks,
  pattern = phrase(c(
    "carbon intens*",
    "climate neutral*",
    "net zero",
    "carbon neutral*",
    "emission reduction*",
    "greenhouse gas",
    "ghg reduction*",
    "co2 reduction*",
    "climate mitigation",
    "climate resilien*",
    "strategic autonom*",
    "industrial competit*",
    "energy security",
    "supply security",
    "strategic dependenc*",
    "strategic value"
  ))
)

#compounding: production
toks2 <- tokens_compound(
  toks,
  pattern = phrase(c(
    "green hydrogen",
    "renewable hydrogen",
    "direct reduced iron",
    "direct reduction",
    "h2 dri",
    "electric arc furnace",
    "renewable electricity",
    "renewable energy",
    "fossil free steel",
    "low carbon steel",
    "green steel",
    "sustainable steel",
    "near zero steel",
    "clean steel",
    "industrial electrification",
    "carbon capture",
    "zero-carbon steel"
  ))
)

#compounding: efficiency
toks2 <- tokens_compound(
  toks,
  pattern = phrase(c(
    "energy efficiency",
    "energy efficient",
    "process efficiency",
    "efficiency improvement*",
    "energy intensity",
    "best available technique*"
  ))
)

dfm_all <- dfm(toks2)
dfm_dict <- dfm_lookup(dfm_all, dictionary = dictionary(policy_dictionary), valuetype = "glob")


# Create DFM
dfm_all <- dfm(toks2)

# Term frequencies
#apply dictionary with the dfm lookup
#result -> count of references to my documents
dict <- dictionary(policy_dictionary)

dfm_dict <- dfm_lookup(
  dfm_all,
  dictionary = dict,
  valuetype = "glob"   # important because we used *
)

dfm_dict

#normalising this per 1000-words
counts <- convert(dfm_dict, to = "data.frame")

counts$total_words <- ntoken(dfm_all)

counts_norm <- counts |>
  mutate(
    across(
      names(policy_dictionary),
      ~ (.x / total_words) * 1000
    )
  )

counts_norm

#check¨
colSums(dfm_dict)

#sensitivity test - that procurement isnt inflated by generic matches ("award", "tender", "contract")
topfeatures(dfm_select(dfm_all, pattern = policy_dictionary$procurement_mechanisms, valuetype = "glob"), 20)

## VISUALISATION OF WORD COUNTS
# creating term level dfm
# loading libraries

# Flatten your dictionary into one vector
all_terms <- unlist(policy_dictionary)

# Select matching features from dfm
dfm_terms <- dfm_select(
  dfm_all,
  pattern = all_terms,
  valuetype = "glob"
)

dfm_terms

#convert to tidy format and normalise
term_counts <- convert(dfm_terms, to = "data.frame")

term_counts$total_words <- ntoken(dfm_all)

term_long <- term_counts |>
  pivot_longer(
    cols = -c(doc_id, total_words),
    names_to = "term",
    values_to = "count"
  ) |>
  mutate(
    freq_per_1000 = (count / total_words) * 1000
  )

#remove ultra rare terms
term_long_filtered <- term_long |>
  group_by(term) |>
  filter(sum(count) > 2) |>   # adjust threshold
  ungroup()

#create the heatmap
ggplot(term_long_filtered,
       aes(x = doc_id, y = term, fill = freq_per_1000)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", name = "Freq per 1k words") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Dictionary Term Frequency by EU Policy Document",
    x = "Document",
    y = "Term"
  )

#iterating heatmap - grouping the heatmap by category

# --- 1. Select individual dictionary terms (not aggregated) ---

all_terms <- unlist(policy_dictionary)

dfm_terms <- dfm_select(
  dfm_all,
  pattern = all_terms,
  valuetype = "glob"
)

# --- 2. Convert to tidy format ---

term_counts <- convert(dfm_terms, to = "data.frame")
term_counts$total_words <- ntoken(dfm_all)

term_long <- term_counts |>
  pivot_longer(
    cols = -c(doc_id, total_words),
    names_to = "term",
    values_to = "count"
  ) |>
  mutate(
    freq_per_1000 = (count / total_words) * 1000
  )

# --- 3. Remove ultra-rare terms (optional but recommended) ---

term_long_filtered <- term_long |>
  group_by(term) |>
  filter(sum(count) > 2) |>   # adjust threshold if needed
  ungroup()

# --- 4. Create term-to-category mapping ---

term_category_map <- stack(policy_dictionary)
colnames(term_category_map) <- c("term", "category")

term_long_filtered <- term_long_filtered |>
  left_join(term_category_map, by = "term")

# --- 5. Order terms by category, then by overall frequency ---

term_order <- term_long_filtered |>
  group_by(term, category) |>
  summarise(total_freq = sum(freq_per_1000), .groups = "drop") |>
  arrange(category, desc(total_freq)) |>
  pull(term)

term_long_filtered$term <- factor(term_long_filtered$term, levels = unique(term_order))

# --- 6. Plot heatmap ---

ggplot(term_long_filtered,
       aes(x = doc_id, y = term, fill = freq_per_1000)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Freq per 1k words") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Dictionary Term Frequency by EU Policy Document",
    subtitle = "Terms ordered by analytical category",
    x = "Document",
    y = "Term"
  )

#iterating heatmap - renaming the pdfs from IDs to names

# -----------------------------
# 0) Rename documents (edit this mapping)
# -----------------------------
doc_name_map <- c(
  "12-SMAP.pdf" = "SMAP",
  "20-NZIA.pdf" = "NZIA",
  "3-CID.pdf"   = "CID"
)

# -----------------------------
# 1) Term-level dfm for dictionary terms
# -----------------------------
all_terms <- unlist(policy_dictionary)

dfm_terms <- dfm_select(
  dfm_all,
  pattern = all_terms,
  valuetype = "glob"
)

# -----------------------------
# 2) Convert to tidy + normalise per 1k words
# -----------------------------
term_counts <- convert(dfm_terms, to = "data.frame")
term_counts$total_words <- ntoken(dfm_all)

term_long <- term_counts |>
  mutate(
    doc_label = dplyr::recode(doc_id, !!!doc_name_map, .default = doc_id)
  ) |>
  pivot_longer(
    cols = -c(doc_id, doc_label, total_words),
    names_to = "term",
    values_to = "count"
  ) |>
  mutate(freq_per_1000 = (count / total_words) * 1000)

# -----------------------------
# 3) Filter ultra-rare terms (optional)
# -----------------------------
term_long_filtered <- term_long |>
  group_by(term) |>
  filter(sum(count) > 2) |>   # adjust threshold if needed
  ungroup()

# -----------------------------
# 4) Add category labels
# -----------------------------
term_category_map <- stack(policy_dictionary)
colnames(term_category_map) <- c("term", "category")

term_long_filtered <- term_long_filtered |>
  left_join(term_category_map, by = "term")

# -----------------------------
# 5) Order terms by category, then by overall frequency
# -----------------------------
term_order <- term_long_filtered |>
  group_by(term, category) |>
  summarise(total_freq = sum(freq_per_1000), .groups = "drop") |>
  arrange(category, desc(total_freq)) |>
  pull(term) |>
  unique()

term_long_filtered$term <- factor(term_long_filtered$term, levels = term_order)

# Optional: order documents by your preferred order
doc_order <- c("SMAP", "CID", "NZIA")
term_long_filtered$doc_label <- factor(term_long_filtered$doc_label, levels = doc_order)

# -----------------------------
# 6) Plot heatmap
# -----------------------------
ggplot(term_long_filtered,
       aes(x = doc_label, y = term, fill = freq_per_1000)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Freq per 1k words") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Dictionary Term Frequency by EU Policy Document",
    subtitle = "Terms ordered by analytical category",
    x = "Document",
    y = "Term"
  )

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


#co-occurrence test - what does the procurement language co-occur with?
#first checking what compounded PP tokens exist to test
feat <- featnames(dfm_all)

feat[grepl("procure", feat)]
feat[grepl("public", feat)]

#then checking the co-occurrence tokens exist
c("public_procurement", "hydrogen", "embodied_carbon", "life_cycle_assessment") %in% feat

#safe co-occurrence query - which of the following exist
targets <- c("hydrogen", "embodied_carbon", "life_cycle_assessment")
key <- "public_procurement"

present <- intersect(c(key, targets), feat)
present

#then runs a co-occurrence test
fcm_mat <- fcm(dfm_all)

fcm_mat[key, setdiff(present, key), drop = FALSE]


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
##RELATIVE SALIENCE (term)- what is disproportionately emphasised in one document compared to the others ##
#prepare data
#full dfm
dfm_terms

#compute keyness
#result -> feature (term) chi2/G2 statistic, direction (over or underused)

key_smap <- textstat_keyness(
  dfm_terms,
  target = "12-SMAP.pdf",   # use your actual doc_id
  measure = "lr"            # log-likelihood (recommended)
)

head(key_smap, 15)

#add direction clarity
key_smap <- key_smap |>
  mutate(
    direction = ifelse(G2 > 0, "Overused in SMAP", "Underused in SMAP")
  )

# Visualise

top_smap <- key_smap |>
  arrange(desc(G2)) |>
  slice_head(n = 15)

ggplot(top_smap,
       aes(x = reorder(feature, G2), y = G2)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Relative Salience (Keyness) — SMAP",
    subtitle = "Terms disproportionately emphasised relative to other EU documents",
    x = "Term",
    y = "Log-Likelihood (G²)"
  )

#repeat for NZIA and CID
key_nzia <- textstat_keyness(dfm_terms, target = "20-NZIA.pdf", measure = "lr")
key_cid  <- textstat_keyness(dfm_terms, target = "3-CID.pdf", measure = "lr")

##RELATIVE SALIENCE (term category)- what is disproportionately emphasised in one document compared to the others ##

key_smap_cat <- textstat_keyness(
  dfm_dict,
  target = "12-SMAP.pdf",
  measure = "lr"   # log-likelihood
)

key_smap_cat

key_nzia_cat <- textstat_keyness(dfm_dict, target = "20-NZIA.pdf", measure = "lr")
key_cid_cat  <- textstat_keyness(dfm_dict, target = "3-CID.pdf", measure = "lr")

format_keyness <- function(key_df, doc_label) {
  key_df |>
    mutate(
      document = doc_label,
      direction = ifelse(G2 > 0, "Over-emphasised", "Under-emphasised")
    )
}

key_smap_cat <- format_keyness(key_smap_cat, "SMAP")
key_nzia_cat <- format_keyness(key_nzia_cat, "NZIA")
key_cid_cat  <- format_keyness(key_cid_cat,  "CID")

key_all_cat <- bind_rows(key_smap_cat, key_nzia_cat, key_cid_cat)

# Visualise category level salience
#result -> Positive bars → disproportionately emphasised in that document
#result -> Negative bars → comparatively downplayed
ggplot(key_all_cat,
       aes(x = reorder(feature, G2), y = G2, fill = direction)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ document) +
  scale_fill_manual(values = c(
    "Over-emphasised" = "#2c7fb8",
    "Under-emphasised" = "#f03b20"
  )) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(
    title = "Category-Level Relative Salience Across EU Policy Documents",
    subtitle = "Log-likelihood keyness comparing each document to the others",
    x = "Analytical Category",
    y = "Log-Likelihood (G²)"
  )


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## CATEGORY-GROUPED WORD CLOUDS ##
# install.packages(c("quanteda", "dplyr", "tidyr", "ggplot2", "ggwordcloud"))

#build term -> category map from dictionary
term_category_map <- stack(policy_dictionary)
colnames(term_category_map) <- c("term", "category")

#create a tidy term-freq table (per 1k words)
term_df <- convert(dfm_terms, to = "data.frame") |>
  mutate(total_words = ntoken(dfm_all)) |>
  pivot_longer(
    cols = -c(doc_id, total_words),
    names_to = "term",
    values_to = "count"
  ) |>
  mutate(freq_per_1000 = (count / total_words) * 1000) |>
  left_join(term_category_map, by = "term") |>
  filter(!is.na(category), count > 0)

#select a document for processing
doc_name_map <- c(
  "12-SMAP.pdf" = "SMAP",
  "20-NZIA.pdf" = "NZIA",
  "3-CID.pdf"   = "CID"
)

doc_to_plot <- "12-SMAP.pdf"  # change this

term_doc <- term_df |>
  filter(doc_id == doc_to_plot) |>
  mutate(doc_label = dplyr::recode(doc_id, !!!doc_name_map, .default = doc_id))

#optional code - only taking top N terms per category
term_doc_top <- term_doc |>
  group_by(category) |>
  slice_max(order_by = freq_per_1000, n = 20, with_ties = FALSE) |>
  ungroup()

term_doc_top$term <- gsub("_", " ", term_doc_top$term)

#faceted word cloud (one per category)
ggplot(term_doc_top, aes(label = term, size = freq_per_1000)) +
  geom_text_wordcloud_area() +
  facet_wrap(~ category, scales = "free") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(
    title = paste0("Term Frequency Word Clouds by Category — ", unique(term_doc_top$doc_label)),
    subtitle = "Font size proportional to frequency per 1,000 words (top terms per category)"
  )

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## COMBINED WORD CLOUD - all articles, all words, all word categories ##

# ---- 1) term -> category map
term_category_map <- stack(policy_dictionary)
colnames(term_category_map) <- c("term", "category")

# ---- 2) Build tidy term table from dfm_terms
# Assumes dfm_terms contains individual dictionary terms
# and dfm_all is your full dfm for word totals

term_df <- convert(dfm_terms, to = "data.frame") |>
  mutate(total_words = ntoken(dfm_all)) |>
  pivot_longer(
    cols = -c(doc_id, total_words),
    names_to = "term",
    values_to = "count"
  ) |>
  mutate(freq_per_1000 = (count / total_words) * 1000) |>
  left_join(term_category_map, by = "term") |>
  filter(!is.na(category), count > 0)

# ---- 3) Restrict to your three EU test docs (edit as needed)
docs_keep <- c("12-SMAP.pdf", "20-NZIA.pdf", "3-CID.pdf")

term_3docs <- term_df |>
  filter(doc_id %in% docs_keep)

# ---- 4) Aggregate across the 3 documents
# (sum of per-1k frequencies; alternative: mean)
term_agg <- term_3docs |>
  group_by(term, category) |>
  summarise(freq = sum(freq_per_1000), .groups = "drop")

# Optional: make multiword terms readable (underscores -> spaces)
term_agg$term_label <- gsub("_", " ", term_agg$term)

# Optional: keep the cloud readable (top N overall)
term_agg <- term_agg |>
  slice_max(order_by = freq, n = 80, with_ties = FALSE)

# ---- 5) Plot single word cloud coloured by category
set.seed(123)

ggplot(term_agg, aes(label = term_label, size = freq, colour = category)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 18) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "EU Policy Corpus Snapshot (SMAP, NZIA, CID)",
    subtitle = "Single word cloud: size = aggregated frequency per 1,000 words; colour = analytical category",
    size = "Freq (per 1k)",
    colour = "Category"
  )

#variant plot: a tiny 3×category heatmap showing which categories are strongest in which document (so the word cloud has context)

# -----------------------------
# 0) Rename documents (edit mapping)
# -----------------------------
doc_name_map <- c(
  "12-SMAP.pdf" = "SMAP",
  "20-NZIA.pdf" = "NZIA",
  "3-CID.pdf"   = "CID"
)

docs_keep <- c("12-SMAP.pdf", "20-NZIA.pdf", "3-CID.pdf")

# -----------------------------
# 1) Convert category dfm to tidy + normalise per 1k words
# -----------------------------
cat_df <- convert(dfm_dict, to = "data.frame") |>
  filter(doc_id %in% docs_keep) |>
  mutate(
    total_words = ntoken(dfm_all)[match(doc_id, docnames(dfm_all))],
    doc_label   = dplyr::recode(doc_id, !!!doc_name_map, .default = doc_id)
  )

cat_long <- cat_df |>
  pivot_longer(
    cols = -c(doc_id, doc_label, total_words),
    names_to = "category",
    values_to = "count"
  ) |>
  mutate(freq_per_1000 = (count / total_words) * 1000)

# Optional: order categories in your preferred conceptual order
category_order <- c(
  "macro_outcomes",
  "production_pathways",
  "circularity_materials",
  "efficiency",
  "procurement_mechanisms"
)
cat_long$category <- factor(cat_long$category, levels = category_order)

# Optional: order documents
doc_order <- c("SMAP", "CID", "NZIA")
cat_long$doc_label <- factor(cat_long$doc_label, levels = doc_order)

# -----------------------------
# 2) Plot tiny heatmap
# -----------------------------
ggplot(cat_long, aes(x = doc_label, y = category, fill = freq_per_1000)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", freq_per_1000)), size = 3) +
  scale_fill_viridis_c(option = "magma", name = "Freq per 1k") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  ) +
  labs(
    title = "Category Strength by Document (EU Test Set)",
    subtitle = "Dictionary hits per 1,000 words (numbers shown in tiles)"
  )

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#-> START HERE
# =========================================================
# EU POLICY CORPUS — FULL RUN (PDFs in data/eu/)
# Dictionary-based NLP with quanteda
# Outputs: doc-level table, term heatmap, doc×category heatmap, corpus wordcloud
# =========================================================

# ---- Packages ----

# ---- 0) Policy dictionary ----
policy_dictionary <- list(
  macro_outcomes = c(
    "decarbon*", "carbon intens*", "climate neutral*", "net zero", "carbon neutral*",
    "emission reduction*", "greenhouse gas", "ghg reduction*", "co2 reduction*",
    "climate mitigation", "climate resilien*", "strategic autonom*", "resilien*",
    "competitiveness", "industrial competit*", "energy security", "security",
    "supply security", "strategic dependenc*", "strategic value"
  ),

  production_pathways = c(
    "hydrogen", "green hydrogen", "renewable hydrogen",
    "electrolysis", "direct reduced iron", "direct reduction",
    "dri", "h2 dri", "electric arc furnace", "eaf",
    "renewable electricity", "renewable energy",
    "fossil free steel", "low carbon steel", "green steel",
    "near zero steel", "clean steel", "sustainable steel",
    "industrial electrification", "carbon capture", "ccs", "ccu", "zero-carbon steel"
  ),

  circularity_materials = c(
    "circular*", "recycl*", "scrap", "secondary steel",
    "material efficiency", "resource efficiency",
    "life cycle", "lifecycle", "life cycle assessment", "lca",
    "embodied carbon", "embodied emissions",
    "product carbon footprint", "pcf",
    "environmental product declaration", "epd",
    "durabil*", "reuse", "remanufact*"
  ),

  efficiency = c(
    "energy efficiency", "energy efficient",
    "process efficiency",
    "efficiency improvement*", "energy intensity",
    "best available technique*", "bat"
  ),

  procurement_mechanisms = c(
    "public procurement", "green public procurement", "gpp",
    "strategic procurement", "sustainable procurement",
    "most economically advantageous tender", "meat",
    "award criteria", "award criterion", "tender",
    "contracting authorit*",
    "life cycle costing", "lcc",
    "total cost of ownership", "tco",
    "technical specification*", "minimum requirement*",
    "contract performance clause*",
    "environmental criteria", "lead market*", "procurement criteria", "procurement" ,"joint procurement"
  )
)

dict <- dictionary(policy_dictionary)

# ---- 1) Read PDFs from data/eu/ ----
pdf_dir <- "data/eu"
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

stopifnot(length(pdf_files) > 0)

docs <- data.frame(
  doc_id = basename(pdf_files),
  text = vapply(pdf_files, function(f) paste(pdf_text(f), collapse = "\n"), character(1)),
  stringsAsFactors = FALSE
)

# ---- 2) Robust cleaning (NUL-safe, element-by-element) ----
clean_text <- function(x) {
  # raw strip NUL bytes, then normalise encoding + whitespace
  r <- charToRaw(x)
  r <- r[r != as.raw(0)]
  y <- rawToChar(r)
  y <- iconv(y, from = "UTF-8", to = "UTF-8", sub = "")
  y <- gsub("\\s+", " ", y)
  y
}

docs$text <- vapply(docs$text, clean_text, character(1))

# Quick check: extremely short docs likely failed extraction
docs$n_chars <- nchar(docs$text)
print(summary(docs$n_chars))
# Optional: inspect failures
# docs |> arrange(n_chars) |> head(10)

# ---- 3) Build corpus + tokens ----
corp <- corpus(docs, text_field = "text")

toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE
)

# ---- 4) Compound multiword phrases (single place, no duplicates) ----
compound_phrases <- phrase(c(
  # procurement / mechanisms
  "public procurement",
  "green public procurement",
  "strategic procurement",
  "sustainable procurement",
  "most economically advantageous tender",
  "award criteria",
  "award criterion",
  "contracting authorit*",
  "life cycle costing",
  "total cost of ownership",
  "technical specification*",
  "minimum requirement*",
  "contract performance clause*",
  "environmental criteria",
  "lead market*",
  "procurement criteria",
  "joint procurement",

  # circularity
  "secondary steel",
  "material efficiency",
  "resource efficiency",
  "life cycle",
  "life cycle assessment",
  "embodied carbon",
  "embodied emissions",
  "product carbon footprint",
  "environmental product declaration",

  # production
  "green hydrogen",
  "renewable hydrogen",
  "direct reduced iron",
  "direct reduction",
  "h2 dri",
  "electric arc furnace",
  "renewable electricity",
  "renewable energy",
  "fossil free steel",
  "low carbon steel",
  "green steel",
  "sustainable steel",
  "near zero steel",
  "clean steel",
  "industrial electrification",
  "carbon capture",
  "zero-carbon steel",

  # macro
  "carbon intens*",
  "climate neutral*",
  "net zero",
  "carbon neutral*",
  "emission reduction*",
  "greenhouse gas",
  "ghg reduction*",
  "co2 reduction*",
  "climate mitigation",
  "climate resilien*",
  "strategic autonom*",
  "industrial competit*",
  "energy security",
  "supply security",
  "strategic dependenc*",
  "strategic value",

  # efficiency
  "energy efficiency",
  "energy efficient",
  "process efficiency",
  "efficiency improvement*",
  "energy intensity",
  "best available technique*"
))

toks <- tokens_compound(toks, pattern = compound_phrases)

# ---- 5) DFM + dictionary lookup (category counts) ----
dfm_all <- dfm(toks)
dfm_dict <- dfm_lookup(dfm_all, dictionary = dict, valuetype = "glob")

# Category frequencies per 1k words
cat_counts <- convert(dfm_dict, to = "data.frame")
cat_counts$total_words <- ntoken(dfm_all)

cat_counts_norm <- cat_counts |>
  mutate(across(names(policy_dictionary), ~ (.x / total_words) * 1000))

# Save doc-level category table
dir.create("outputs", showWarnings = FALSE)
write.csv(cat_counts_norm, "outputs/eu_category_counts_per_1k.csv", row.names = FALSE)

# ---- 6) Tiny document × category heatmap (full EU corpus) ----
cat_long <- cat_counts_norm |>
  select(doc_id, all_of(names(policy_dictionary))) |>
  pivot_longer(
    cols = -doc_id,
    names_to = "category",
    values_to = "freq_per_1000"
  )

category_order <- c(
  "macro_outcomes",
  "production_pathways",
  "circularity_materials",
  "efficiency",
  "procurement_mechanisms"
)
cat_long$category <- factor(cat_long$category, levels = category_order)

p_cat_heat <- ggplot(cat_long, aes(x = doc_id, y = category, fill = freq_per_1000)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_viridis_c(option = "magma", name = "Per 1k words") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    title = "EU policy corpus: category frequencies by document",
    subtitle = "Dictionary hits per 1,000 words"
  )

ggsave("outputs/eu_doc_by_category_heatmap.png", p_cat_heat, width = 12, height = 3.5, dpi = 300)

# ---- 7) Term-level heatmap ordered by category ----
all_terms <- unlist(policy_dictionary)

dfm_terms <- dfm_select(dfm_all, pattern = all_terms, valuetype = "glob")

term_counts <- convert(dfm_terms, to = "data.frame")
term_counts$total_words <- ntoken(dfm_all)

term_long <- term_counts |>
  pivot_longer(
    cols = -c(doc_id, total_words),
    names_to = "term",
    values_to = "count"
  ) |>
  mutate(freq_per_1000 = (count / total_words) * 1000)

# Filter ultra-rare terms (tune threshold)
term_long_filtered <- term_long |>
  group_by(term) |>
  filter(sum(count) > 3) |>
  ungroup()

term_category_map <- stack(policy_dictionary)
colnames(term_category_map) <- c("term", "category")

term_long_filtered <- term_long_filtered |>
  left_join(term_category_map, by = "term")

term_order <- term_long_filtered |>
  group_by(term, category) |>
  summarise(total_freq = sum(freq_per_1000), .groups = "drop") |>
  arrange(category, desc(total_freq)) |>
  pull(term) |>
  unique()

term_long_filtered$term <- factor(term_long_filtered$term, levels = term_order)

p_term_heat <- ggplot(term_long_filtered, aes(x = doc_id, y = term, fill = freq_per_1000)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(option = "magma", name = "Per 1k words") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7)
  ) +
  labs(
    title = "EU policy corpus: term frequencies by document",
    subtitle = "Terms ordered by analytical category" #(filtered to remove ultra-rare terms)
  )

ggsave("outputs/eu_term_heatmap_by_category.png", p_term_heat, width = 12, height = 9, dpi = 300)

# ---- 8) Single combined word cloud (all EU documents), coloured by category ----
term_df <- term_long |>
  left_join(term_category_map, by = "term") |>
  filter(!is.na(category), count > 0)

term_agg <- term_df |>
  group_by(term, category) |>
  summarise(freq = sum(freq_per_1000), .groups = "drop") |>
  mutate(term_label = gsub("_", " ", term)) |>
  slice_max(order_by = freq, n = 100, with_ties = FALSE)

set.seed(123)

p_cloud <- ggplot(term_agg, aes(label = term_label, size = freq, colour = category)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 18) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(
    title = "EU policy corpus: dictionary term snapshot",
    subtitle = "Word cloud across all EU documents (size = aggregated frequency per 1k words; colour = category)",
    size = "Per 1k",
    colour = "Category"
  )

ggsave("outputs/eu_dictionary_wordcloud.png", p_cloud, width = 10, height = 6, dpi = 300)

# ---- 9) Quick ranking table to prioritise close reading later ----
# e.g. docs with highest procurement_mechanisms
priority <- cat_counts_norm |>
  select(doc_id, total_words, all_of(names(policy_dictionary))) |>
  arrange(desc(procurement_mechanisms)) |>
  mutate(rank_procurement = row_number())

write.csv(priority, "outputs/eu_priority_table_by_procurement.csv", row.names = FALSE)

message("Done. Outputs written to /outputs/")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## ADDITIONAL COOCCURANCE TESTING ON THE WHOLE EU SUITE ##
# Document level co-occ.
# create presence table (present 0 or 1)
  cooc_doc <- cat_counts_norm |>
  select(doc_id, all_of(names(policy_dictionary))) |>
  mutate(
    across(
      names(policy_dictionary),
      ~ ifelse(.x > 0, 1, 0)
    )
  )

# identify procurement + circularity: tells me wh/ docs look @ procurement + circularity
proc_circ_docs <- cooc_doc |>
  filter(procurement_mechanisms == 1,
         circularity_materials == 1) |>
  pull(doc_id)

proc_circ_docs

# identify procurement + production: tells me wh/ docs look @ procurement + production
proc_prod_docs <- cooc_doc |>
  filter(procurement_mechanisms == 1,
         production_pathways == 1) |>
  pull(doc_id)

proc_prod_docs

# identify procurement + efficiency: tells me wh/ docs look @ procurement + efficiency
proc_eff_docs <- cooc_doc |>
  filter(procurement_mechanisms == 1,
         efficiency == 1) |>
  pull(doc_id)

proc_eff_docs

# thresholded co-occurence (not just 1 reference, but some depth of discussion)
threshold <- 0.3  # per 1000 words (tune)

cooc_doc_thresh <- cat_counts_norm |>
  select(doc_id, all_of(names(policy_dictionary))) |>
  mutate(
    across(
      names(policy_dictionary),
      ~ ifelse(.x > threshold, 1, 0)
    )
  )

#visualisation: co-occurence heatmap
cooc_long <- cooc_doc |>
  pivot_longer(
    cols = -doc_id,
    names_to = "category",
    values_to = "present"
  )

ggplot(cooc_long,
       aes(x = doc_id, y = category, fill = factor(present))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "white", "1" = "#2c7fb8"),
                    labels = c("Absent", "Present")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Category Presence Across EU Policy Documents",
    fill = ""
  )

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## DIGGING DEEPER - TERMS NOT JUST CATEOGORIES FOR ALL EU DOCS ##
# Extract terms at dict. level
# Flatten dictionary
all_terms <- unlist(policy_dictionary)

# Select matching features
dfm_terms <- dfm_select(
  dfm_all,
  pattern = all_terms,
  valuetype = "glob"
)

# convert to tidy + normalise per 1000 words
term_counts <- convert(dfm_terms, to = "data.frame")

term_counts$total_words <- ntoken(dfm_all)

term_long <- term_counts |>
  pivot_longer(
    cols = -c(doc_id, total_words),
    names_to = "term",
    values_to = "count"
  ) |>
  mutate(
    freq_per_1000 = (count / total_words) * 1000
  )

#attach cat labels to each word
term_category_map <- stack(policy_dictionary)
colnames(term_category_map) <- c("term", "category")

term_long <- term_long |>
  left_join(term_category_map, by = "term")

#save output csv
write.csv(term_long,
          "outputs/eu_term_level_per_doc_per_1000.csv",
          row.names = FALSE)

#create table
term_agg <- term_long |>
  group_by(term, category) |>
  summarise(
    total_freq_per_1000 = sum(freq_per_1000),
    total_count = sum(count),
    .groups = "drop"
  ) |>
  arrange(desc(total_freq_per_1000))

write.csv(term_agg,
          "outputs/eu_term_level_aggregated.csv",
          row.names = FALSE)

# Visualise results - aggregated term freq plot (ranked bar)

# Optional: remove ultra-rare terms
term_plot <- term_agg |>
  filter(total_count > 3) |>
  slice_max(order_by = total_freq_per_1000, n = 25)

term_plot$term_label <- gsub("_", " ", term_plot$term)

ggplot(term_plot,
       aes(x = reorder(term_label, total_freq_per_1000),
           y = total_freq_per_1000,
           fill = category)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(
    title = "Most Frequent Dictionary Terms Across EU Policy Corpus",
    subtitle = "Frequency per 1,000 words (aggregated across documents)",
    x = "Term",
    y = "Frequency per 1,000 words",
    fill = "Category"
  )

# alternative - by category
ggplot(term_plot,
       aes(x = reorder(term_label, total_freq_per_1000),
           y = total_freq_per_1000)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  facet_wrap(~ category, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(
    title = "Top Terms Within Each Analytical Category",
    x = "Term",
    y = "Frequency per 1,000 words"
  )

## now just zooming into the documents that mention procurement: conditioning on procurement
## In the subset of EU documents that talk about procurement, what other concepts (macro / production / circularity / efficiency) show up, and which specific terms drive that?
# Identify the docs that ref procurement
proc_docs <- cat_counts_norm |>
  filter(procurement_mechanisms > 0) |>
  pull(doc_id)

length(proc_docs)
head(proc_docs)

#filtering term table to only procurement referring docs
term_long_proc <- term_long |>
  filter(doc_id %in% proc_docs)

write.csv(term_long_proc,
          "outputs/eu_term_level_per_doc_per_1000__procurement_docs_only.csv",
          row.names = FALSE)

# Plotting
#aggregate within the procurement docs
term_agg_proc <- term_long_proc |>
  group_by(term, category) |>
  summarise(
    total_freq_per_1000 = sum(freq_per_1000),
    total_count = sum(count),
    .groups = "drop"
  ) |>
  arrange(desc(total_freq_per_1000))

write.csv(term_agg_proc,
          "outputs/eu_term_level_aggregated__procurement_docs_only.csv",
          row.names = FALSE)

# top terms used in procurement referring docs

term_plot_proc <- term_agg_proc |>
  filter(total_count > 3) |>
  slice_max(order_by = total_freq_per_1000, n = 30)

term_plot_proc$term_label <- gsub("_", " ", term_plot_proc$term)

ggplot(term_plot_proc,
       aes(x = reorder(term_label, total_freq_per_1000),
           y = total_freq_per_1000,
           fill = category)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(
    title = "Top Dictionary Terms in EU Documents that Mention Procurement",
    subtitle = "Frequency per 1,000 words, aggregated across procurement-relevant documents",
    x = "Term",
    y = "Frequency per 1,000 words",
    fill = "Category"
  )

# version by category
# take top N per category
term_plot_proc_facet <- term_agg_proc |>
  filter(total_count > 1) |>
  group_by(category) |>
  slice_max(order_by = total_freq_per_1000, n = 12, with_ties = FALSE) |>
  ungroup()

term_plot_proc_facet$term_label <- gsub("_", " ", term_plot_proc_facet$term)

ggplot(term_plot_proc_facet,
       aes(x = reorder(term_label, total_freq_per_1000),
           y = total_freq_per_1000)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ category, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(
    title = "Term Profiles Within Procurement-Relevant EU Documents",
    subtitle = "Top terms per category (frequency per 1,000 words, aggregated)",
    x = "Term",
    y = "Frequency per 1,000 words"
  )

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## NATIONAL LEVEL ANALYSIS ##
# Read and prepare national corpus

# =====================================================
# NATIONAL POLICY CORPUS (DE, NL, SE)
# PDFs in data/nat/
# =====================================================


# ---- 1) Read PDFs ----
nat_dir <- "data/nat"
nat_files <- list.files(nat_dir, pattern = "\\.pdf$", full.names = TRUE)

nat_docs <- data.frame(
  doc_id = basename(nat_files),
  text = vapply(nat_files, function(f) paste(pdf_text(f), collapse = "\n"), character(1)),
  stringsAsFactors = FALSE
)

# ---- 2) Extract country from filename ----
nat_docs$country <- str_extract(nat_docs$doc_id, "(DE|NL|SE)")

table(nat_docs$country)


# Clean text
clean_text <- function(x) {
  r <- charToRaw(x)
  r <- r[r != as.raw(0)]
  y <- rawToChar(r)
  y <- iconv(y, from = "UTF-8", to = "UTF-8", sub = "")
  y <- gsub("\\s+", " ", y)
  y
}

nat_docs$text <- vapply(nat_docs$text, clean_text, character(1))

# Build corpus and apply dictionary
corp_nat <- corpus(nat_docs, text_field = "text")

toks_nat <- tokens(
  corp_nat,
  remove_punct = TRUE,
  remove_numbers = TRUE
)

# Same compounding phrases as EU script
toks_nat <- tokens_compound(toks_nat, pattern = compound_phrases)

dfm_nat <- dfm(toks_nat)

dfm_nat_dict <- dfm_lookup(
  dfm_nat,
  dictionary = dict,
  valuetype = "glob"
)

# category freq per document
nat_cat_counts <- convert(dfm_nat_dict, to = "data.frame")
nat_cat_counts$total_words <- ntoken(dfm_nat)

nat_cat_counts_norm <- nat_cat_counts |>
  mutate(across(
    names(policy_dictionary),
    ~ (.x / total_words) * 1000
  )) |>
  left_join(nat_docs[, c("doc_id", "country")], by = "doc_id")

write.csv(nat_cat_counts_norm,
          "outputs/nat_category_counts_per_1k.csv",
          row.names = FALSE)

# aggregate by country
nat_country_summary <- nat_cat_counts_norm |>
  group_by(country) |>
  summarise(
    across(names(policy_dictionary), mean),
    .groups = "drop"
  )

write.csv(nat_country_summary,
          "outputs/nat_country_category_means.csv",
          row.names = FALSE)

nat_country_summary

# Procurement co-occurrence @ nat. level
nat_cooc_doc <- nat_cat_counts_norm |>
  mutate(across(
    names(policy_dictionary),
    ~ ifelse(.x > 0, 1, 0)
  ))

# Procurement + Circularity
nat_proc_circ <- nat_cooc_doc |>
  filter(procurement_mechanisms == 1,
         circularity_materials == 1) |>
  select(doc_id, country)

# Procurement + Production
nat_proc_prod <- nat_cooc_doc |>
  filter(procurement_mechanisms == 1,
         production_pathways == 1) |>
  select(doc_id, country)

# Procurement + Efficiency
nat_proc_eff <- nat_cooc_doc |>
  filter(procurement_mechanisms == 1,
         efficiency == 1) |>
  select(doc_id, country)

# term level freq @ nat level
all_terms <- unlist(policy_dictionary)

dfm_nat_terms <- dfm_select(dfm_nat, pattern = all_terms, valuetype = "glob")

nat_term_counts <- convert(dfm_nat_terms, to = "data.frame")
nat_term_counts$total_words <- ntoken(dfm_nat)

nat_term_long <- nat_term_counts |>
  pivot_longer(
    cols = -c(doc_id, total_words),
    names_to = "term",
    values_to = "count"
  ) |>
  mutate(freq_per_1000 = (count / total_words) * 1000) |>
  left_join(nat_docs[, c("doc_id", "country")], by = "doc_id")

#aggregate by country
nat_term_country <- nat_term_long |>
  group_by(country, term) |>
  summarise(freq_per_1000 = sum(freq_per_1000), .groups = "drop")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

## compare EU v National
#comparison table
eu_country_summary <- cat_counts_norm |>
  summarise(across(names(policy_dictionary), mean)) |>
  mutate(country = "EU")

comparison_table <- bind_rows(
  eu_country_summary,
  nat_country_summary
)

write.csv(comparison_table,
          "outputs/eu_vs_national_category_comparison.csv",
          row.names = FALSE)

comparison_table

# comparison plot
comparison_long <- comparison_table |>
  pivot_longer(
    cols = -country,
    names_to = "category",
    values_to = "mean_freq_per_1000"
  )

ggplot(comparison_long,
       aes(x = country, y = mean_freq_per_1000, fill = category)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "EU vs National Category Emphasis",
    y = "Mean frequency per 1,000 words"
  )

## TERM LEVEL HEATMAPS FOR EACH COUNTRY
# attach category labels
term_category_map <- stack(policy_dictionary)
colnames(term_category_map) <- c("term", "category")

nat_term_long <- nat_term_long |>
  left_join(term_category_map, by = "term")

# aggregate per country
nat_term_country <- nat_term_long |>
  group_by(country, term, category) |>
  summarise(freq_per_1000 = sum(freq_per_1000),
            total_count = sum(count),
            .groups = "drop")

# heatmap for each country
plot_country_heatmap <- function(country_code) {

  data_plot <- nat_term_country |>
    filter(country == country_code,
           total_count > 2)

  # order by category then frequency
  term_order <- data_plot |>
    arrange(category, desc(freq_per_1000)) |>
    pull(term) |>
    unique()

  data_plot$term <- factor(data_plot$term, levels = term_order)

  ggplot(data_plot,
         aes(x = country, y = term, fill = freq_per_1000)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "magma", name = "Per 1k words") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_blank()
    ) +
    labs(
      title = paste("Dictionary Term Frequencies —", country_code),
      subtitle = "Aggregated across national documents"
    )
}

#run each of these separately to get the individual heatmaps
plot_country_heatmap("DE")
plot_country_heatmap("NL")
plot_country_heatmap("SE")

# COHERENCE INDEX PER COUNTRY: How many analytical categories are present together (non-zero) per document? -> Then average that per country
#binary presence per doc
nat_presence <- nat_cat_counts_norm |>
  mutate(across(
    names(policy_dictionary),
    ~ ifelse(.x > 0, 1, 0)
  ))

# coherence = number of categories present
nat_presence$coherence_score <- rowSums(
  nat_presence[, names(policy_dictionary)]
)

#country level coherence
#5 = all categories present
#1 = narrow focus
#Higher = broader policy integration
nat_coherence_country <- nat_presence |>
  group_by(country) |>
  summarise(
    mean_coherence = mean(coherence_score),
    max_coherence = max(coherence_score),
    .groups = "drop"
  )

nat_coherence_country

#eu comparison
eu_presence <- cat_counts_norm |>
  mutate(across(
    names(policy_dictionary),
    ~ ifelse(.x > 0, 1, 0)
  ))

eu_presence$coherence_score <- rowSums(
  eu_presence[, names(policy_dictionary)]
)

mean(eu_presence$coherence_score)

# RELATIVE SALIANCE: Which categories are disproportionately emphasised in DE/SE/NL relative to EU?
# combine EU + NAT into single dfm
dfm_combined <- rbind(
  dfm_all,
  dfm_nat
)

dfm_combined_dict <- dfm_lookup(
  dfm_combined,
  dictionary = dict,
  valuetype = "glob"
)

#add country labels
docvars(dfm_combined_dict, "country") <- c(
  rep("EU", ndoc(dfm_all)),
  nat_docs$country
)

#keyness EU v DE/SE/NL

key_de_vs_eu <- textstat_keyness(
  dfm_combined_dict,
  target = docvars(dfm_combined_dict, "country") == "DE",
  measure = "lr"
)

key_de_vs_eu

key_nl_vs_eu <- textstat_keyness(
  dfm_combined_dict,
  target = docvars(dfm_combined_dict, "country") == "NL",
  measure = "lr"
)

key_nl_vs_eu

key_se_vs_eu <- textstat_keyness(
  dfm_combined_dict,
  target = docvars(dfm_combined_dict, "country") == "SE",
  measure = "lr"
)

key_se_vs_eu

# Visualise - e.g. DE v EU
key_de_vs_eu |>
  slice_max(order_by = G2, n = 5) |>
  ggplot(aes(x = reorder(feature, G2), y = G2)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Categories Over-Emphasised in DE vs EU",
    x = "Category",
    y = "Log-likelihood (G²)"
  )

# Visualise - e.g. NL v EU
key_nl_vs_eu |>
  slice_max(order_by = G2, n = 5) |>
  ggplot(aes(x = reorder(feature, G2), y = G2)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Categories Over-Emphasised in NL vs EU",
    x = "Category",
    y = "Log-likelihood (G²)"
  )

#visulaise - e.g. SE v EU
key_se_vs_eu |>
  slice_max(order_by = G2, n = 5) |>
  ggplot(aes(x = reorder(feature, G2), y = G2)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Categories Over-Emphasised in SE vs EU",
    x = "Category",
    y = "Log-likelihood (G²)"
  )

## TERM LEVEL KEYNESS COMPARISON: So not just looking @ categories, but actual terms ##
# Combine datasets
dfm_combined <- rbind(dfm_all, dfm_nat)

docvars(dfm_combined, "country") <- c(
  rep("EU", ndoc(dfm_all)),
  nat_docs$country
)

# Restrict to dictionary terms
all_terms <- unlist(policy_dictionary)

dfm_combined_terms <- dfm_select(
  dfm_combined,
  pattern = all_terms,
  valuetype = "glob"
)

# DE v EU term level comparison
# results -> Positive G2 → overused in DE relative to EU & Negative → underused in DE

key_de_term <- textstat_keyness(
  dfm_combined_terms,
  target = docvars(dfm_combined_terms, "country") == "DE",
  measure = "lr"
)

head(key_de_term, 10)

# same for NL v EU
key_nl_term <- textstat_keyness(
  dfm_combined_terms,
  target = docvars(dfm_combined_terms, "country") == "NL",
  measure = "lr"
)

head(key_nl_term, 10)

# same for SE v EU
key_se_term <- textstat_keyness(
  dfm_combined_terms,
  target = docvars(dfm_combined_terms, "country") == "SE",
  measure = "lr"
)

head(key_se_term, 10)

# visualising overemphasised terms
# DE top 10
key_de_term |>
  slice_max(order_by = G2, n = 10) |>
  ggplot(aes(x = reorder(feature, G2), y = G2)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Terms Over-Emphasised in DE vs EU",
    y = "Log-likelihood (G²)",
    x = "Term"
  )

# NL top 10
key_nl_term |>
  slice_max(order_by = G2, n = 10) |>
  ggplot(aes(x = reorder(feature, G2), y = G2)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Terms Over-Emphasised in NL vs EU",
    y = "Log-likelihood (G²)",
    x = "Term"
  )

# SE top 10
key_se_term |>
  slice_max(order_by = G2, n = 10) |>
  ggplot(aes(x = reorder(feature, G2), y = G2)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Terms Over-Emphasised in SE vs EU",
    y = "Log-likelihood (G²)",
    x = "Term"
  )

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## TRANSLATION GAP INDEX: How much national emphasis diverges from EU emphasis per category ##
# Compute EU baseline (mean per doc)
  eu_category_mean <- cat_counts_norm |>
  summarise(across(names(policy_dictionary), mean)) |>
  pivot_longer(
    everything(),
    names_to = "category",
    values_to = "eu_mean"
  )

# Compute national means
nat_category_mean <- nat_cat_counts_norm |>
  group_by(country) |>
  summarise(across(names(policy_dictionary), mean)) |>
  pivot_longer(
    cols = -country,
    names_to = "category",
    values_to = "nat_mean"
  )

# Join and compute translation gap
# results -> Positive gap → national emphasises more than EU; Negative gap → national emphasises less than EU; Absolute gap → degree of divergence
translation_gap <- nat_category_mean |>
  left_join(eu_category_mean, by = "category") |>
  mutate(
    gap = nat_mean - eu_mean,
    gap_abs = abs(gap)
  )

translation_gap

# Overall translation divergence index per country
# result -> higher = stronger divergence from EU framing
translation_index <- translation_gap |>
  group_by(country) |>
  summarise(
    mean_abs_gap = mean(gap_abs),
    .groups = "drop"
  )

translation_index

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## RADAR PLOTS (EV v EACH COUNTRY) ##
#prepare EU and national means tables

categories <- c(
  "macro_outcomes",
  "production_pathways",
  "circularity_materials",
  "efficiency",
  "procurement_mechanisms"
)

# EU mean across EU documents
eu_means <- cat_counts_norm |>
  summarise(across(all_of(categories), mean, na.rm = TRUE)) |>
  mutate(group = "EU")

# National means by country
nat_means <- nat_cat_counts_norm |>
  group_by(country) |>
  summarise(across(all_of(categories), mean, na.rm = TRUE), .groups = "drop") |>
  rename(group = country)

means_all <- bind_rows(eu_means, nat_means)

means_all

# prepare plots
plot_radar_vs_eu <- function(country_code, means_all, categories) {

  d <- means_all |>
    filter(group %in% c("EU", country_code)) |>
    select(group, all_of(categories))

  stopifnot(nrow(d) == 2)

  d <- d |>
    mutate(across(all_of(categories), as.numeric))

  max_val <- max(as.matrix(d[, categories]), na.rm = TRUE)
  max_row <- as.data.frame(as.list(rep(max_val * 1.10, length(categories))))
  min_row <- as.data.frame(as.list(rep(0, length(categories))))
  colnames(max_row) <- categories
  colnames(min_row) <- categories

  data_rows <- as.data.frame(d[, categories])

  radar_df <- rbind(max_row, min_row, data_rows)
  rownames(radar_df) <- c("max", "min", d$group)

  # ---- Force a new plot page/device ----
  plot.new()   # <- this fixes "plot.new has not been called yet"

  radarchart(
    radar_df,
    axistype = 1,
    pcol = c("blue", "red"),
    plwd = 2,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    vlcex = 0.9,
    title = paste0(country_code, " vs EU — Category Emphasis (per 1k words)")
  )

  legend("topright",
         legend = c("EU", country_code),
         col = c("blue", "red"),
         lwd = 2,
         bty = "n")
}

#plot
plot_radar_vs_eu("DE", means_all, categories)
plot_radar_vs_eu("NL", means_all, categories)
plot_radar_vs_eu("SE", means_all, categories)

#saving plots
# DE v EU radar
png("outputs/radar_DE_vs_EU.png", width = 900, height = 900, res = 150)
plot_radar_vs_eu("DE", means_all, categories)
dev.off()

# NL v EU radar
png("outputs/radar_NL_vs_EU.png", width = 900, height = 900, res = 150)
plot_radar_vs_eu("NL", means_all, categories)
dev.off()

# SE v EU radar
png("outputs/radar_SE_vs_EU.png", width = 900, height = 900, res = 150)
plot_radar_vs_eu("SE", means_all, categories)
dev.off()

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## QUALITATIVE ANALYSIS: So now we know quantitatively where and how procurement is occuring and co-occurring, but we want to go deeper now. When procurement is mentioned, how is it framed, and how (if at all) is it operationalised for green steel decarbonisation? ##
## STEP 1: Subset of documents for deductive QCA (EU + NAT docs with procurement hits ##
# EU docs with procurement hits
  eu_proc_docs <- cat_counts_norm |>
  filter(procurement_mechanisms > 0) |>
  pull(doc_id)

eu_proc_docs

# NAT docs with procurement hits
nat_proc_docs <- nat_cat_counts_norm |>
  filter(procurement_mechanisms > 0) |>
  pull(doc_id)

nat_proc_docs


# -----------------------------
# 0) Preconditions
# -----------------------------
stopifnot(all(c("doc_id","text") %in% names(docs)))
stopifnot(all(c("doc_id","text","country") %in% names(nat_docs)))

# dict and compound_phrases must already exist from your quantitative pipeline
# dict <- dictionary(policy_dictionary)
# compound_phrases <- phrase(c(...))

# -----------------------------
# 1) Cleaning that preserves paragraph breaks
# -----------------------------
clean_text_qca <- function(x) {
  # remove NUL bytes
  r <- charToRaw(x)
  r <- r[r != as.raw(0)]
  y <- rawToChar(r)

  y <- iconv(y, from = "UTF-8", to = "UTF-8", sub = "")
  y <- gsub("\r\n", "\n", y, fixed = TRUE)
  y <- gsub("\r", "\n", y, fixed = TRUE)

  # collapse spaces/tabs but KEEP newlines
  y <- gsub("[ \t]+", " ", y)

  # keep paragraph breaks (2 newlines), reduce excessive newlines
  y <- gsub("\n{3,}", "\n\n", y)

  str_trim(y)
}

# -----------------------------
# 2) Harmonise EU + national docs into one table
# -----------------------------
eu_docs_qca <- docs %>%
  transmute(doc_id = doc_id, text = text, level = "EU", country = "EU")

nat_docs_qca <- nat_docs %>%
  transmute(doc_id = doc_id, text = text, level = "NAT", country = country)

all_docs_qca <- bind_rows(eu_docs_qca, nat_docs_qca)
all_docs_qca$text <- vapply(all_docs_qca$text, clean_text_qca, character(1))

# Diagnostics: check empties early
all_docs_qca$n_chars <- nchar(all_docs_qca$text)
cat("Docs:", nrow(all_docs_qca), "\n")
cat("Empty/near-empty docs (<200 chars):", sum(all_docs_qca$n_chars < 200), "\n")
print(all_docs_qca %>% arrange(n_chars) %>% select(doc_id, country, level, n_chars) %>% head(10))

# -----------------------------
# 3) Split each document into units
#    Prefer paragraphs if we see any \n\n; otherwise fallback to sentences.
# -----------------------------
split_units <- function(txt) {
  # paragraph split if present
  if (str_detect(txt, "\n\n")) {
    units <- unlist(strsplit(txt, "\n\\s*\n", perl = TRUE))
  } else {
    # sentence-ish split fallback (not perfect, but robust)
    units <- unlist(strsplit(txt, "(?<=[\\.\\!\\?])\\s+", perl = TRUE))
  }
  units <- str_squish(units)
  units[nchar(units) > 0]
}

units_df <- all_docs_qca %>%
  mutate(unit_text = lapply(text, split_units)) %>%
  select(doc_id, level, country, unit_text) %>%
  unnest(unit_text) %>%
  group_by(doc_id) %>%
  mutate(unit_id = row_number()) %>%
  ungroup() %>%
  mutate(unit_uid = paste0(doc_id, "::", unit_id)) %>%
  filter(nchar(unit_text) >= 120)  # remove tiny fragments

cat("Units (after filtering short):", nrow(units_df), "\n")
if (nrow(units_df) == 0) stop("No usable units were created. Likely many PDFs extracted as empty text.")

# -----------------------------
# 4) Create unit-level corpus and run dictionary lookup
# -----------------------------
corp_units <- corpus(units_df, text_field = "unit_text", docid_field = "unit_uid")
docvars(corp_units, "doc_id") <- units_df$doc_id
docvars(corp_units, "level")  <- units_df$level
docvars(corp_units, "country") <- units_df$country
docvars(corp_units, "unit_id") <- units_df$unit_id

toks_units <- tokens(corp_units, remove_punct = TRUE, remove_numbers = TRUE)
toks_units <- tokens_compound(toks_units, pattern = compound_phrases)

dfm_units <- dfm(toks_units)

dfm_units_dict <- dfm_lookup(dfm_units, dictionary = dict, valuetype = "glob")
unit_counts <- convert(dfm_units_dict, to = "data.frame")

# Ensure all category columns exist (even if all-zero)
for (nm in names(policy_dictionary)) {
  if (!nm %in% names(unit_counts)) unit_counts[[nm]] <- integer(nrow(unit_counts))
}

# Add text + metadata back in
unit_df <- unit_counts %>%
  mutate(
    unit_text = as.character(corp_units),
    doc_id = docvars(corp_units, "doc_id"),
    level  = docvars(corp_units, "level"),
    country = docvars(corp_units, "country"),
    unit_id = docvars(corp_units, "unit_id")
  )

###### MODALITY CODING BLOCK (APPLIED BEFORE PROCUREMENT FILTERS)
# =====================================================
# MODALITY CODING (unit-level)
# =====================================================

# 1) Define modality dictionary (glob patterns allowed)
modality_dictionary <- dictionary(list(
  high_binding   = c("shall", "must", "require*", "oblig*", "mandat*", "is required to", "are required to", "shall ensure", "must ensure", "shall establish", "shall adopt"),
  medium_binding = c("should", "recommend*", "expect*"),
  low_binding    = c("may", "can", "encourage*", "support*", "promote*", "facilitat*"),
  negation       = c("shall not", "must not", "may not", "cannot", "can't", "not allow*")
))

# 2) Count modality terms per unit using dfm_lookup
dfm_units_mod <- dfm_lookup(dfm_units, dictionary = modality_dictionary, valuetype = "glob")
mod_counts <- convert(dfm_units_mod, to = "data.frame")

# Ensure modality columns exist even if all-zero
for (nm in c("high_binding","medium_binding","low_binding","negation")) {
  if (!nm %in% names(mod_counts)) mod_counts[[nm]] <- integer(nrow(mod_counts))
}

# 3) Attach to unit_df (rows align because both come from dfm_units)
unit_df <- unit_df %>%
  mutate(
    high_binding   = mod_counts$high_binding,
    medium_binding = mod_counts$medium_binding,
    low_binding    = mod_counts$low_binding,
    negation       = mod_counts$negation
  ) %>%
  mutate(
    # strongest signal present in the unit
    modality_strength = case_when(
      high_binding > 0 ~ "High (binding)",
      medium_binding > 0 ~ "Medium (soft obligation)",
      low_binding > 0 ~ "Low (aspirational/enablement)",
      TRUE ~ "None/unclear"
    ),
    # helpful summary score (optional)
    modality_score = high_binding*3 + medium_binding*2 + low_binding*1 - negation*1
  )

# =====================================================
# PROCUREMENT-FOCUSED QCA EXPORTS (now includes modality)
# =====================================================

proc_units <- unit_df %>%
  filter(.data[["procurement_mechanisms"]] > 0)

cat("Procurement units:", nrow(proc_units), "\n")

dir.create("outputs", showWarnings = FALSE)

write.csv(proc_units, "outputs/qca_procurement_units.csv", row.names = FALSE)

write.csv(proc_units %>% filter(production_pathways > 0),
          "outputs/qca_procurement_plus_production_units.csv", row.names = FALSE)

write.csv(proc_units %>% filter(circularity_materials > 0),
          "outputs/qca_procurement_plus_circularity_units.csv", row.names = FALSE)

write.csv(proc_units %>% filter(efficiency > 0),
          "outputs/qca_procurement_plus_efficiency_units.csv", row.names = FALSE)


# ---------------------------------------------------
# Procurement + Efficiency units
# ---------------------------------------------------

proc_eff_units <- proc_units %>%
  filter(.data[["efficiency"]] > 0)

cat("Procurement + Efficiency units:", nrow(proc_eff_units), "\n")

write.csv(proc_eff_units,
          "outputs/qca_procurement_plus_efficiency_units.csv",
          row.names = FALSE)

## STEP 3: ADD MODALITY DEDUCTION: Identify strength of language: (a) SHALL / MUST (binding), (b) SHOULD (soft obligation), (c) MAY / ENCOURAGE / SUPPORT (weak/aspirational)
# =====================================================
# PROCUREMENT-FOCUSED QCA EXPORTS (now includes modality)
# =====================================================

proc_units <- unit_df %>%
  filter(.data[["procurement_mechanisms"]] > 0)

cat("Procurement units:", nrow(proc_units), "\n")

dir.create("outputs", showWarnings = FALSE)

write.csv(proc_units, "outputs/qca_procurement_units.csv", row.names = FALSE)

write.csv(proc_units %>% filter(production_pathways > 0),
          "outputs/qca_procurement_plus_production_units.csv", row.names = FALSE)

write.csv(proc_units %>% filter(circularity_materials > 0),
          "outputs/qca_procurement_plus_circularity_units.csv", row.names = FALSE)

write.csv(proc_units %>% filter(efficiency > 0),
          "outputs/qca_procurement_plus_efficiency_units.csv", row.names = FALSE)


# Sense checks
# How "binding" is procurement language, by level/country
# result -> EU (signaller): high/binding (315), medium (79), low (64), none/unclear (93) (tf: EU procurement passages are dominated by binding language and less aspirational than might have been expecting - strong regulatory framing)
# result -> DE (messenger): high (132), medium (14), low (90), none (150) (tf: DE either provides concrete binding rules or descriptive text without modal verbs)
# result -> NL (messenger) v. similar to DE
# result -> SE (messenger): high (417), medium (14), low (200), none (141) (tf: v.hgh binding language and also high aspirational language - strong governance framing)
proc_units %>%
  count(level, country, modality_strength) %>%
  arrange(level, country, desc(n))

# Same but within procurement + production (steel transition relevance)
proc_units %>%
  filter(production_pathways > 0) %>%
  count(level, country, modality_strength) %>%
  arrange(level, country, desc(n))

## MODALITY IN PP
# refinement to make sure the text like "shall", "must" etc refers to procurement, rather than just generally used
# "What is the modality within procurement + production or procurement + circularity units?"
proc_units %>%
  filter(production_pathways > 0) %>%
  count(level, country, modality_strength) %>%
  arrange(level, country, desc(n))

proc_units %>%
  filter(circularity_materials > 0) %>%
  count(level, country, modality_strength) %>%
  arrange(level, country, desc(n))

# results -> At EU level, procurement language linked to production pathways is predominantly binding in character. By contrast, national-level documents more frequently frame procurement in descriptive or aspirational terms, suggesting a softening in translation from supranational signalling to domestic strategy.

