# TED Contract Award Notice analysis for green steel procurement paper
# ------------------------------------------------------------------------------
# Purpose: Filter TED CAN data and analyse MEAT and environmental procurement criteria.
# Author: Natalie Gravett-Foyn
# Project: Green steel public procurement / lead markets
#
# Notes for reuse:
# - Run from the project root directory (for example, with an RStudio Project).
# - Expected folders include data/, data_intermediate/, outputs/ and figures/.
# - The script does not set a working directory, to keep it portable for GitHub.
# ------------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "stringr",
  "stringi",
  "readr",
  "ggplot2",
  "tidyr",
  "lubridate",
  "scales",
  "purrr"
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
# 1. Read, filter and save TED data
# ------------------------------------------------------------------------------

files <- c(
  paste0("data/TED_CAN_", 2010:2017, ".csv"),
  "data/TED_CAN_2018_2023.csv"
)

out_path <- "data_intermediate/ted_core_2010_2023_CPV35_45_no_framework.csv"
dir.create("data_intermediate", showWarnings = FALSE)
if (file.exists(out_path)) file.remove(out_path)

cols_needed <- c(
  "ID_NOTICE_CAN", "YEAR", "DT_DISPATCH", "DT_AWARD",
  "ISO_COUNTRY_CODE",
  "TYPE_OF_CONTRACT",
  "B_FRA_AGREEMENT",
  "CPV", "ADDITIONAL_CPVS",
  "VALUE_EURO", "AWARD_VALUE_EURO",
  "CRIT_CODE", "CRIT_CRITERIA", "CRIT_WEIGHTS",
  "TITLE"
)

cb <- DataFrameCallback$new(function(chunk, pos) {

  chunk <- chunk %>%
    select(any_of(cols_needed)) %>%
    mutate(
      YEAR = as.integer(YEAR),
      ISO_COUNTRY_CODE = toupper(trimws(ISO_COUNTRY_CODE)),
      CPV = as.character(CPV),
      cpv2 = str_sub(CPV, 1, 2),

      # EU vs focal countries flag
      country_group = case_when(
        ISO_COUNTRY_CODE == "DE" ~ "DE",
        ISO_COUNTRY_CODE == "NL" ~ "NL",
        ISO_COUNTRY_CODE == "SE" ~ "SE",
        TRUE ~ "EU_other"
      )
    ) %>%
    # robustness check
    filter(YEAR >= 2010, YEAR <= 2023) %>%
    # steel-relevant sectors
    filter(cpv2 %in% c("45", "35")) %>%
    # exclude framework agreements
    filter(is.na(B_FRA_AGREEMENT) | B_FRA_AGREEMENT != "Y") %>%
    # drop cancelled if you want (optional)
    # filter(is.na(CANCELLED) | CANCELLED != "Y")
    mutate(
      TITLE = ifelse(is.na(TITLE), "", TITLE),
      TITLE = str_squish(TITLE)
    )

  # Append filtered chunk to disk
  write_csv(chunk, out_path, append = file.exists(out_path))
})

for (f in files) {
  message("Chunk-reading: ", basename(f))
  read_csv_chunked(
    f,
    callback = cb,
    chunk_size = 200000,
    progress = TRUE,
    show_col_types = FALSE
  )
}

# Read back the filtered dataset for analysis
ted_core <- read_csv(out_path, show_col_types = FALSE)

# quick check
ted_core %>%
  count(YEAR, country_group) %>%
  arrange(YEAR, country_group) %>%
  print(n = 40)

## TEMPORAL TRENDS
# EU v NATIONAL TRENDS (vol + val)

# ---- Prepare "EU total" (EU = DE+NL+SE+EU_other) and keep DE/NL/SE ----
trend_base <- ted_core %>%
  mutate(
    award_value = suppressWarnings(as.numeric(AWARD_VALUE_EURO)),
    est_value   = suppressWarnings(as.numeric(VALUE_EURO)),
    value_use   = ifelse(!is.na(award_value), award_value, est_value)
  ) %>%
  group_by(YEAR, country_group) %>%
  summarise(
    n_notices = n(),
    total_value_eur = sum(value_use, na.rm = TRUE),
    .groups = "drop"
  )

# EU aggregate across all country groups
trend_eu <- trend_base %>%
  group_by(YEAR) %>%
  summarise(
    country_group = "EU",
    n_notices = sum(n_notices, na.rm = TRUE),
    total_value_eur = sum(total_value_eur, na.rm = TRUE),
    .groups = "drop"
  )

trend_focus <- trend_base %>%
  filter(country_group %in% c("DE","NL","SE"))

trend_plot_df <- bind_rows(trend_eu, trend_focus)

# ---- Colours requested ----
cols <- c("EU"="grey30", "NL"="orange", "SE"="blue", "DE"="darkred")

# ---- Volume plot ----
p_volume <- ggplot(trend_plot_df, aes(x = YEAR, y = n_notices, color = country_group)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "TED CAN volume in steel-relevant sectors (CPV 35 + 45)",
    subtitle = "EU vs DE/NL/SE, 2010–2023 (excluding framework agreements)",
    x = NULL, y = "Number of contract award notices", color = NULL
  )

p_volume

# ---- Value plot ----
# looks odd due to scale (EU so much higher than national values)

p_value <- ggplot(trend_plot_df, aes(x = YEAR, y = total_value_eur, color = country_group)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal(base_size = 12) +
  labs(
    title = "TED CAN total award value in steel-relevant sectors (CPV 35 + 45)",
    subtitle = "EU vs DE/NL/SE, 2010–2023 (AWARD_VALUE_EURO, fallback to VALUE_EURO)",
    x = NULL, y = "Total value (EUR)", color = NULL
  )

p_value

scale_y_continuous(labels = label_number(scale_cut = cut_si("kM")))

#revised value plotting with different scales
#building the trend tables

cols_nat <- c("NL"="orange", "SE"="blue", "DE"="darkred")

trend_base <- ted_core %>%
  mutate(
    award_value = suppressWarnings(as.numeric(AWARD_VALUE_EURO)),
    est_value   = suppressWarnings(as.numeric(VALUE_EURO)),
    value_use   = ifelse(!is.na(award_value), award_value, est_value)
  ) %>%
  group_by(YEAR, country_group) %>%
  summarise(
    n_notices = n(),
    total_value_eur = sum(value_use, na.rm = TRUE),
    .groups = "drop"
  )

# EU total across all country groups
trend_eu <- trend_base %>%
  group_by(YEAR) %>%
  summarise(
    country_group = "EU",
    n_notices = sum(n_notices, na.rm = TRUE),
    total_value_eur = sum(total_value_eur, na.rm = TRUE),
    .groups = "drop"
  )

# National only
trend_nat <- trend_base %>%
  filter(country_group %in% c("DE","NL","SE"))

#developing the separate plots
#eu only - value
p_value_eu <- ggplot(trend_eu, aes(x = YEAR, y = total_value_eur)) +
  geom_line(linewidth = 1, color = "grey30") +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal(base_size = 12) +
  labs(
    title = "EU total award value in steel-relevant sectors (CPV 35 + 45)",
    subtitle = "2010–2023 (excluding framework agreements)",
    x = NULL, y = "Total value (EUR)"
  )

p_value_eu

#national only
p_value_nat <- ggplot(trend_nat, aes(x = YEAR, y = total_value_eur, color = country_group)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_nat) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal(base_size = 12) +
  labs(
    title = "National award value in steel-relevant sectors (CPV 35 + 45)",
    subtitle = "DE vs NL vs SE, 2010–2023 (excluding framework agreements)",
    x = NULL, y = "Total value (EUR)", color = NULL
  )

p_value_nat

# TREND ANALYSIS - PROCUREMENT VOLUMES
p_vol_eu <- ggplot(trend_eu, aes(x = YEAR, y = n_notices)) +
  geom_line(linewidth = 1, color = "grey30") +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "EU volume of awards in steel-relevant sectors (CPV 35 + 45)",
    subtitle = "2010–2023 (excluding framework agreements)",
    x = NULL, y = "Number of contract award notices"
  )

p_vol_eu

p_vol_nat <- ggplot(trend_nat, aes(x = YEAR, y = n_notices, color = country_group)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = cols_nat) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "National volume of awards in steel-relevant sectors (CPV 35 + 45)",
    subtitle = "DE vs NL vs SE, 2010–2023 (excluding framework agreements)",
    x = NULL, y = "Number of contract award notices", color = NULL
  )

p_vol_nat

# exporting figures
# create figs folder
dir.create("figures", showWarnings = FALSE)

#export all figs for value and volume over time
# ---- EU award value ----
ggsave(
  filename = "figures/Figure1_EU_award_value_2010_2023.png",
  plot = p_value_eu,
  width = 8, height = 5, dpi = 300
)

ggsave(
  filename = "figures/Figure1_EU_award_value_2010_2023.pdf",
  plot = p_value_eu,
  width = 8, height = 5
)

# ---- National award value ----
ggsave(
  filename = "figures/Figure2_National_award_value_DE_NL_SE.png",
  plot = p_value_nat,
  width = 8, height = 5, dpi = 300
)

ggsave(
  filename = "figures/Figure2_National_award_value_DE_NL_SE.pdf",
  plot = p_value_nat,
  width = 8, height = 5
)

# ---- EU volume ----
ggsave(
  filename = "figures/Figure3_EU_volume_2010_2023.png",
  plot = p_vol_eu,
  width = 8, height = 5, dpi = 300
)

ggsave(
  filename = "figures/Figure3_EU_volume_2010_2023.pdf",
  plot = p_vol_eu,
  width = 8, height = 5
)

# ---- National volume ----
ggsave(
  filename = "figures/Figure4_National_volume_DE_NL_SE.png",
  plot = p_vol_nat,
  width = 8, height = 5, dpi = 300
)

ggsave(
  filename = "figures/Figure4_National_volume_DE_NL_SE.pdf",
  plot = p_vol_nat,
  width = 8, height = 5
)


## CALCULATING THE SHARE OF MEAT PROCUREMENTS
# MEAT SHARE SLICED BY SUBSECTOR

ted_sub <- ted_core %>%
  mutate(
    cpv = as.character(CPV),
    cpv2 = str_sub(cpv, 1, 2),
    cpv4 = str_sub(cpv, 1, 4),

    subsector = case_when(
      cpv2 == "35" ~ "defence",
      cpv4 == "4521" ~ "buildings",
      cpv4 == "4523" ~ "transport",
      cpv2 == "45" ~ "construction_other",
      TRUE ~ "other"
    ),

    # MEAT flag (conservative)
    crit_code = toupper(trimws(CRIT_CODE)),
    crit_criteria = tolower(trimws(CRIT_CRITERIA)),
    is_meat = case_when(
      str_detect(crit_code, "M") ~ 1L,
      TRUE ~ 0L
    ),

    # EU vs focal countries
    line_group = case_when(
      ISO_COUNTRY_CODE %in% c("DE","NL","SE") ~ ISO_COUNTRY_CODE,
      TRUE ~ "EU_other"
    )
  ) %>%
  filter(subsector %in% c("buildings","transport","defence"))

# MEAT SHARE BY YR x SUBSEC x DE/SE/NL
# Country-level (DE/NL/SE + EU_other)


# ------------------------------------------------------------
# 1) Build subsector + MEAT flag (row level)
# ------------------------------------------------------------
ted_sub <- ted_core %>%
  mutate(
    CPV = as.character(CPV),
    cpv2 = str_sub(CPV, 1, 2),
    cpv4 = str_sub(CPV, 1, 4),

    subsector = case_when(
      cpv2 == "35" ~ "defence",
      cpv4 == "4521" ~ "buildings",
      cpv4 == "4523" ~ "transport",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(subsector)) %>%
  mutate(
    ISO_COUNTRY_CODE = toupper(trimws(ISO_COUNTRY_CODE)),
    line_group = if_else(ISO_COUNTRY_CODE %in% c("DE","NL","SE"),
                         ISO_COUNTRY_CODE, "EU_other"),

    crit_code = toupper(trimws(CRIT_CODE)),
    is_meat_row = if_else(crit_code == "M", 1L, 0L)
  )

# ------------------------------------------------------------
# 2) Collapse to NOTICE level (prevents multiple-criteria rows bias)
# ------------------------------------------------------------
notice_meat <- ted_sub %>%
  group_by(ID_NOTICE_CAN, YEAR, subsector, ISO_COUNTRY_CODE, line_group) %>%
  summarise(
    is_meat = as.integer(any(is_meat_row == 1L, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3) EU aggregate + national series
# ------------------------------------------------------------
meat_eu <- notice_meat %>%
  group_by(YEAR, subsector) %>%
  summarise(
    line_group = "EU",
    n_notices = n(),
    share_meat = mean(is_meat),
    .groups = "drop"
  )

meat_nat <- notice_meat %>%
  filter(line_group %in% c("DE","NL","SE")) %>%
  group_by(YEAR, subsector, line_group) %>%
  summarise(
    n_notices = n(),
    share_meat = mean(is_meat),
    .groups = "drop"
  )

meat_plot_df <- bind_rows(meat_eu, meat_nat)

write.csv(meat_plot_df, "outputs/meat_share_by_subsector_eu_de_nl_se.csv", row.names = FALSE)

# ------------------------------------------------------------
# 4) Plot
# ------------------------------------------------------------
cols <- c("EU"="grey30", "NL"="orange", "SE"="blue", "DE"="darkred")

p_meat_subsector <- ggplot(meat_plot_df, aes(x = YEAR, y = share_meat, color = line_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Share of MEAT awards by subsector (steel-relevant procurement)",
    subtitle = "EU vs DE/NL/SE, 2010–2023",
    x = NULL, y = "Share MEAT", color = NULL
  )

p_meat_subsector

#sanity check
notice_meat %>% summarise(overall_share_meat = mean(is_meat))
notice_meat %>% count(is_meat)
notice_meat %>% count(subsector, is_meat)

## GREEN FLAG

# ------------------------------------------------------------
# 1) Build a text blob for keyword matching
#    (CRIT_CRITERIA is a code; CRIT_WEIGHTS often contains useful text/structure)
# ------------------------------------------------------------
ted_sub2 <- ted_sub %>%
  mutate(
    title_txt = ifelse(is.na(TITLE), "", TITLE),
    weights_txt = ifelse(is.na(CRIT_WEIGHTS), "", as.character(CRIT_WEIGHTS)),

    text_blob = str_to_lower(str_squish(paste(title_txt, weights_txt, sep = " "))),
    # normalise accents (å/ä/ö, ü, etc.) to make matching robust
    text_blob = stringi::stri_trans_general(text_blob, "Latin-ASCII")
  )

# ------------------------------------------------------------
# 2) Multilingual "green" dictionary (starter set)
#    You can expand this with your Yang & Morotomi list later.
# ------------------------------------------------------------
green_terms <- c(
  # ---- English ----
  "green", "sustainab", "environment", "climate", "carbon", "co2", "emission",
  "life cycle", "lifecycle", "lca", "epd", "environmental product declaration",
  "product carbon footprint", "pcf", "embodied carbon", "embodied emission",
  "circular", "recycl", "reuse", "remanufact", "durab", "resource efficien",
  "energy efficien", "renewable", "net zero", "climate neutral",

  # ---- German (ASCII) ----
  "nachhalt", "umwelt", "klima", "co2", "emission",
  "lebenszyklus", "okobilanz", "oekobilanz",
  "umweltproduktdeklaration", "epd",
  "kreislauf", "recyc", "wiederverwend", "energieeffizienz", "erneuerbar",
  "klimaneutral", "netto null",

  # ---- Dutch (ASCII) ----
  "duurzaam", "milieu", "klimaat", "co2", "emissie",
  "levenscyclus", "lca", "epd", "milieuproductverklaring",
  "circulair", "recycl", "hergebruik", "energie efficient", "energie-efficient",
  "hernieuwbaar", "klimaatneutraal", "netto nul",

  # ---- Swedish (ASCII) ----
  "hallbar", "miljo", "klimat", "koldioxid", "co2", "utslapp",
  "livscykel", "lca", "epd", "miljovarudeklaration",
  "cirkular", "atervinn", "aterbruk", "energieffektiv", "fornybar",
  "klimatneutral", "netto noll"
)

# Regex: simple OR. (We deliberately avoid strict word boundaries because of stems like sustainab*)
green_regex <- paste0("(", paste(unique(green_terms), collapse = "|"), ")")

ted_sub2 <- ted_sub2 %>%
  mutate(
    green_hit = ifelse(str_detect(text_blob, green_regex), 1L, 0L)
  )

# ------------------------------------------------------------
# 3) Collapse to NOTICE level (same idea as MEAT)
# ------------------------------------------------------------
notice_green <- ted_sub2 %>%
  group_by(ID_NOTICE_CAN, YEAR, subsector, ISO_COUNTRY_CODE, line_group) %>%
  summarise(
    green_flag = as.integer(any(green_hit == 1L, na.rm = TRUE)),
    # keep MEAT at notice level too (from your existing logic)
    is_meat = as.integer(any(is_meat_row == 1L, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 4) Compute EU + national series
# ------------------------------------------------------------
# EU overall (all EU countries)
green_eu <- notice_green %>%
  group_by(YEAR, subsector) %>%
  summarise(
    line_group = "EU",
    n_notices = n(),
    share_green = mean(green_flag),
    share_green_among_meat = mean(green_flag[is_meat == 1], na.rm = TRUE),
    .groups = "drop"
  )

# DE/NL/SE
green_nat <- notice_green %>%
  filter(line_group %in% c("DE","NL","SE")) %>%
  group_by(YEAR, subsector, line_group) %>%
  summarise(
    n_notices = n(),
    share_green = mean(green_flag),
    share_green_among_meat = mean(green_flag[is_meat == 1], na.rm = TRUE),
    .groups = "drop"
  )

green_plot_df <- bind_rows(green_eu, green_nat)

dir.create("outputs", showWarnings = FALSE)
write.csv(green_plot_df, "outputs/green_share_by_subsector_eu_de_nl_se.csv", row.names = FALSE)

# ------------------------------------------------------------
# 5) Plot: share green (overall)
# ------------------------------------------------------------
cols <- c("EU"="grey30", "NL"="orange", "SE"="blue", "DE"="darkred")

p_green_subsector <- ggplot(green_plot_df, aes(x = YEAR, y = share_green, color = line_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Share of green-flagged awards by subsector (CPV 4521/4523/35)",
    subtitle = "Flag applied to TITLE + CRIT_WEIGHTS (multilingual: EN/DE/NL/SE)",
    x = NULL, y = "Share green-flagged", color = NULL
  )

p_green_subsector

# ------------------------------------------------------------
# 6) Plot: share green among MEAT only
# ------------------------------------------------------------
p_green_meat_subsector <- ggplot(green_plot_df, aes(x = YEAR, y = share_green_among_meat, color = line_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Share of green-flagged awards among MEAT awards, by subsector",
    subtitle = "EU vs DE/NL/SE, 2010–2023",
    x = NULL, y = "Share green among MEAT", color = NULL
  )

p_green_meat_subsector


## DEBUGGING - prev. SE was 0 for green w/in MEAT + the green flag was not broad enough
# Building comprehensive multilingual green dictionary

# -----------------------------
# A) EN (Yang/Morotomi core + extensions for built env / procurement text)
# -----------------------------
green_keywords_en <- c(
  # General env/sustainability
  "environment", "environmental", "eco", "ecological", "green",
  "sustainab*", "sustainable", "sustainability",

  # Energy & efficiency
  "energy", "energy efficien*", "energy saving", "energy-efficient", "energy efficient",
  "renewable*", "solar", "wind", "geothermal", "bioenergy",
  "electrification", "electrif*", "heat pump*", "district heating",

  # Emissions / climate
  "emission*", "co2", "carbon", "carbon footprint",
  "greenhouse gas*", "ghg", "climate", "climate change",
  "net zero", "net-zero", "climate neutral*", "carbon neutral*",

  # Resources / circularity / waste
  "resource efficien*", "material efficien*",
  "recycl*", "recycled", "recyclable", "reuse", "reused", "remanufact*",
  "circular*", "waste", "waste reduction", "waste management",
  "low impact", "environmental impact",

  # Life-cycle / embodied / product metrics
  "life cycle", "lifecycle",
  "life cycle costing", "lcc",
  "life cycle assessment", "lca",
  "environmental product declaration", "epd",
  "product carbon footprint", "pcf",
  "embodied carbon", "embodied emission*", "embodied co2",
  "whole life carbon", "wlc",

  # Environmental management / standards / labels
  "iso 14001", "iso14001", "emas",
  "fsc", "pefc",
  "breeam", "leed"
)

# -----------------------------
# B) Swedish (SV) - include stems; note we’ll ASCII-normalise later
# -----------------------------
green_keywords_sv <- c(
  "miljö", "miljömässig", "miljövänlig", "miljökrav", "miljöcertifier*",
  "hållbar*", "hållbarhet",
  "energi", "energieffektiv*", "energisnål*",
  "förnybar*", "förnybara",
  "koldioxid", "utsläpp*", "klimat*", "klimatpåverkan",
  "livscykel*", "livscykelkostnad*",
  "återvinn*", "återvunnet", "återbruk*",
  "resurseffektiv*", "materialeffektiv*",
  "cirkulär*", "cirkular*",
  "miljöbyggnad", "svanen", "nordic swan"  # swan often appears in EN too
)

# -----------------------------
# C) German (DE)
# -----------------------------
green_keywords_de <- c(
  "umwelt*", "umweltfreund*", "okologisch*", "oekologisch*", "nachhalt*",
  "energie", "energieeffizienz*", "energiespar*", "energieeffizient*",
  "erneuerbar*", "erneuerbare",
  "co2", "kohlendioxid", "emission*", "treibhausgas*", "klima*", "klimawandel",
  "klimaneutral*", "netto null", "net-zero",
  "ressourceneffizienz*", "materialeffizienz*",
  "recycl*", "wiederverwend*", "wiederverwertung", "abfall*", "kreislauf*",
  "lebenszyklus*", "lebenszykluskosten*", "lebenszykluskalkulation*",
  "okobilanz*", "oekobilanz*",
  "umweltproduktdeklaration*", "epd",
  "produkt carbon footprint", "pcf",
  "iso 14001", "iso14001", "emas",
  "fsc", "pefc",
  "breeam", "leed"
)

# -----------------------------
# D) Dutch (NL)
# -----------------------------
green_keywords_nl <- c(
  "milieu*", "milieuvriend*", "ecologisch*", "duurzaam*",
  "energie", "energie-efficient*", "energie efficient", "energiebesparing*",
  "hernieuwbaar*", "duurzame energie",
  "co2", "koolstof", "emissie*", "broeikasgas*", "klimaat*", "klimaatverandering",
  "klimaatneutraal*", "netto nul", "net-zero",
  "grondstoff*", "resource", "materiaalefficient*", "materiaal efficient*",
  "recycl*", "hergebruik*", "afval*", "circulair*", "kringloop*",
  "levenscyclus*", "levenscycluskosten*", "lcc", "lca",
  "milieuproductverklaring*", "epd",
  "product carbon footprint", "pcf",
  "iso 14001", "iso14001", "emas",
  "fsc", "pefc",
  "breeam", "leed"
)

# -----------------------------
# E) Combine: MULTI-LANG + English ALWAYS
# -----------------------------
green_terms_full <- unique(c(
  green_keywords_en,
  green_keywords_sv,
  green_keywords_de,
  green_keywords_nl
))

# Normalise dictionary terms (ASCII) for matching robustness
green_terms_full_norm <- stringi::stri_trans_general(green_terms_full, "Latin-ASCII")
green_terms_full_norm <- tolower(green_terms_full_norm)

# build safe regex
# Escape regex special chars, then convert glob * to regex .*
escape_regex_glob <- function(x) {
  x <- str_replace_all(x, "([\\.^$|()\\[\\]{}+?\\\\])", "\\\\\\1") # escape
  x <- str_replace_all(x, "\\*", ".*")                             # glob star -> regex
  x
}

green_regex <- paste0("(", paste(escape_regex_glob(green_terms_full_norm), collapse="|"), ")")

# Apply green flag to the TED data
ted_sub2 <- ted_sub %>%
  mutate(
    title_txt = ifelse(is.na(TITLE), "", TITLE),
    weights_txt = ifelse(is.na(CRIT_WEIGHTS), "", as.character(CRIT_WEIGHTS)),
    code_txt = ifelse(is.na(CRIT_CODE), "", as.character(CRIT_CODE)),

    text_blob = str_to_lower(str_squish(paste(title_txt, weights_txt, code_txt, sep = " "))),
    text_blob = stringi::stri_trans_general(text_blob, "Latin-ASCII"),

    green_hit = as.integer(str_detect(text_blob, green_regex))
  )

# test if SE working better now
ted_sub2 %>%
  mutate(txt_len = nchar(text_blob)) %>%
  group_by(ISO_COUNTRY_CODE) %>%
  summarise(
    n = n(),
    share_empty = mean(txt_len == 0),
    median_len = median(txt_len),
    p90_len = quantile(txt_len, 0.9),
    green_rate = mean(green_hit),
    .groups = "drop"
  ) %>%
  arrange(desc(share_empty))

# rerun

# ------------------------------------------------------------
# 1) Collapse to NOTICE level (prevents multiple criterion rows bias)
#    Keep only the three subsectors (already filtered in ted_sub)
# ------------------------------------------------------------
notice_green <- ted_sub2 %>%
  mutate(
    # MEAT at row level (already exists as is_meat_row in your pipeline)
    is_meat_row = ifelse(toupper(trimws(CRIT_CODE)) == "M", 1L, 0L)
  ) %>%
  group_by(ID_NOTICE_CAN, YEAR, subsector, ISO_COUNTRY_CODE, line_group) %>%
  summarise(
    green_flag = as.integer(any(green_hit == 1L, na.rm = TRUE)),
    is_meat    = as.integer(any(is_meat_row == 1L, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 2) Aggregate: EU and DE/NL/SE
# ------------------------------------------------------------
# EU trend (all EU countries)
green_eu <- notice_green %>%
  group_by(YEAR, subsector) %>%
  summarise(
    line_group = "EU",
    n_notices = n(),
    share_green = mean(green_flag),
    share_green_among_meat = mean(green_flag[is_meat == 1], na.rm = TRUE),
    .groups = "drop"
  )

# National trends
green_nat <- notice_green %>%
  filter(line_group %in% c("DE","NL","SE")) %>%
  group_by(YEAR, subsector, line_group) %>%
  summarise(
    n_notices = n(),
    share_green = mean(green_flag),
    share_green_among_meat = mean(green_flag[is_meat == 1], na.rm = TRUE),
    .groups = "drop"
  )

green_plot_df <- bind_rows(green_eu, green_nat)

dir.create("outputs", showWarnings = FALSE)
write.csv(green_plot_df, "outputs/green_share_by_subsector_eu_de_nl_se.csv", row.names = FALSE)

# ------------------------------------------------------------
# 3) Plot: share green overall
# ------------------------------------------------------------
cols <- c("EU"="grey30", "NL"="orange", "SE"="blue", "DE"="darkred")

p_green_subsector <- ggplot(green_plot_df, aes(x = YEAR, y = share_green, color = line_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Share of green-flagged awards by subsector (steel-relevant procurement)",
    subtitle = "Multilingual dictionary applied to TITLE + CRIT_WEIGHTS; EU vs DE/NL/SE, 2010–2023",
    x = NULL, y = "Share green-flagged", color = NULL
  )

p_green_subsector

# ------------------------------------------------------------
# 4) Plot: share green among MEAT only (often most meaningful)
# ------------------------------------------------------------
p_green_among_meat <- ggplot(green_plot_df, aes(x = YEAR, y = share_green_among_meat, color = line_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Share of green-flagged awards among MEAT awards, by subsector",
    subtitle = "Approximates GPP/IGPP intensity in steel-relevant procurement",
    x = NULL, y = "Share green among MEAT", color = NULL
  )

p_green_among_meat

# SE re-diagnosis
ted_sub2 %>%
  mutate(txt_len = nchar(text_blob)) %>%
  group_by(ISO_COUNTRY_CODE) %>%
  summarise(
    n_rows = n(),
    share_empty = mean(txt_len == 0),
    median_len = median(txt_len),
    p90_len = quantile(txt_len, 0.9),
    green_hit_rate_rows = mean(green_hit),
    .groups = "drop"
  ) %>%
  arrange(desc(share_empty))

# SE amendment - condition so that it only does green share where MEAT has specifications
notice_textcov <- ted_sub2 %>%
  mutate(has_text_row = as.integer(nchar(text_blob) >= 20)) %>%   # threshold you choose
  group_by(ID_NOTICE_CAN, YEAR, subsector, ISO_COUNTRY_CODE, line_group) %>%
  summarise(
    has_text = as.integer(any(has_text_row == 1L)),
    green_flag = as.integer(any(green_hit == 1L, na.rm = TRUE)),
    is_meat = as.integer(any(toupper(trimws(CRIT_CODE)) == "M", na.rm = TRUE)),
    .groups = "drop"
  )

# Coverage + green rate among text-available notices
green_cov <- notice_textcov %>%
  group_by(YEAR, subsector, line_group) %>%
  summarise(
    n_notices = n(),
    share_with_text = mean(has_text),
    share_green_given_text = mean(green_flag[has_text == 1], na.rm = TRUE),
    share_green_meat_given_text = mean(green_flag[has_text == 1 & is_meat == 1], na.rm = TRUE),
    .groups = "drop"
  )

# replot
p_green_among_meat <- ggplot(green_plot_df, aes(x = YEAR, y = share_green_among_meat, color = line_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Share of green-flagged awards among MEAT awards, by subsector",
    subtitle = "Approximates GPP/IGPP intensity in steel-relevant procurement",
    x = NULL, y = "Share green among MEAT", color = NULL
  )

p_green_among_meat

notice_textcov %>%
  group_by(line_group) %>%
  summarise(share_with_text = mean(has_text), n = n())

# decided new approach to control for limited actual text in SE TED
#So Sweden is not zero because procurement isn’t green — it’s because most Swedish CAN records simply don’t contain usable text fields in this dataset.This is a known structural issue with TED CAN extracts.
#The green procurement flag is derived from keyword matching applied to the limited textual fields available in Contract Award Notices (primarily titles and criteria fields). Text coverage varies substantially across countries, with Swedish notices containing usable text in only ~28% of cases. Consequently, green procurement estimates for Sweden are interpreted cautiously and reported conditional on text availability.”
# build final notice-level dataset with coverage + meat + green (master analytical dataset)
notice_final <- ted_sub2 %>%
  mutate(
    is_meat_row = ifelse(toupper(trimws(CRIT_CODE)) == "M", 1L, 0L),
    has_text_row = as.integer(nchar(text_blob) >= 20)  # threshold for usable text
  ) %>%
  group_by(ID_NOTICE_CAN, YEAR, subsector, ISO_COUNTRY_CODE, line_group) %>%
  summarise(
    has_text = as.integer(any(has_text_row == 1)),
    green_flag = as.integer(any(green_hit == 1)),
    is_meat = as.integer(any(is_meat_row == 1)),
    .groups = "drop"
  )

#text coverage analysis - methodological robustness figure
coverage_year <- notice_final %>%
  group_by(YEAR, line_group) %>%
  summarise(
    share_with_text = mean(has_text),
    .groups = "drop"
  )

#plot coverage over time
cols <- c("EU_other"="grey70","EU"="black",
          "NL"="orange","SE"="blue","DE"="darkred")

ggplot(coverage_year, aes(YEAR, share_with_text, color=line_group)) +
  geom_line(linewidth=1) +
  scale_y_continuous(labels=scales::percent_format()) +
  theme_minimal(base_size=12) +
  labs(
    title="Availability of usable textual data in TED CAN records",
    subtitle="Share of notices with sufficient text for keyword analysis",
    y="Share with usable text", x=NULL
  )

#green procurement shares (conditional on text)
green_conditional <- notice_final %>%
  filter(has_text == 1) %>%
  group_by(YEAR, subsector, line_group) %>%
  summarise(
    n_notices = n(),
    share_green = mean(green_flag),
    share_green_meat = mean(green_flag[is_meat == 1], na.rm = TRUE),
    .groups = "drop"
  )

#add eu aggregate
green_eu <- notice_final %>%
  filter(has_text == 1) %>%
  group_by(YEAR, subsector) %>%
  summarise(
    line_group = "EU",
    share_green = mean(green_flag),
    share_green_meat = mean(green_flag[is_meat == 1], na.rm=TRUE),
    .groups="drop"
  )

green_plot_df <- bind_rows(green_conditional, green_eu)

#plot green share (conditional on text)
ggplot(green_plot_df,
       aes(YEAR, share_green, color=line_group)) +
  geom_line(linewidth=1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values=c("EU"="black","NL"="orange","SE"="blue","DE"="darkred")) +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_vline(xintercept=2014, linetype="dashed") +
  geom_vline(xintercept=2016, linetype="dotted") +
  geom_vline(xintercept=2017, linetype="dotted") +
  theme_minimal(base_size=12) +
  labs(
    title="Green procurement in steel-relevant sectors (conditional on text)",
    subtitle="EU vs DE/NL/SE, 2010–2023",
    y="Share green-flagged", x=NULL
  )

# green procurement among meat figure
ggplot(green_plot_df,
       aes(YEAR, share_green_meat, color=line_group)) +
  geom_line(linewidth=1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values=c("EU"="black","NL"="orange","SE"="blue","DE"="darkred")) +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_vline(xintercept=2014, linetype="dashed") +
  geom_vline(xintercept=2016, linetype="dotted") +
  geom_vline(xintercept=2017, linetype="dotted") +
  theme_minimal(base_size=12) +
  labs(
    title="Environmental considerations within MEAT awards",
    subtitle="Conditional on text availability",
    y="Share green among MEAT", x=NULL
  )

#save outputs
write.csv(notice_final, "outputs/ted_notice_level_dataset.csv", row.names=FALSE)
write.csv(green_plot_df, "outputs/green_procurement_results.csv", row.names=FALSE)

# text -> Green procurement was identified using a multilingual keyword dictionary applied to textual fields available in TED Contract Award Notices (primarily titles and award criteria fields). Because text availability varies across countries, green procurement indicators are calculated conditional on notices containing sufficient textual information. This approach avoids bias arising from missing or sparsely populated text fields, which are particularly prevalent in some national reporting practices.

# developing a table version of this graph (for reporting)

dir.create("outputs", showWarnings = FALSE)

# 1) Country-level results (conditional on text)
green_country_table <- notice_final %>%
  filter(has_text == 1) %>%
  group_by(YEAR, subsector, line_group) %>%
  summarise(
    n_notices = n(),
    share_green = mean(green_flag),
    share_green_among_meat = mean(green_flag[is_meat == 1], na.rm = TRUE),
    meat_share = mean(is_meat),
    .groups = "drop"
  )

# 2) EU aggregate (conditional on text)
green_eu_table <- notice_final %>%
  filter(has_text == 1) %>%
  group_by(YEAR, subsector) %>%
  summarise(
    line_group = "EU",
    n_notices = n(),
    share_green = mean(green_flag),
    share_green_among_meat = mean(green_flag[is_meat == 1], na.rm = TRUE),
    meat_share = mean(is_meat),
    .groups = "drop"
  )

green_table_full <- bind_rows(
  green_country_table %>% filter(line_group %in% c("DE","NL","SE")),
  green_eu_table
) %>%
  arrange(subsector, line_group, YEAR)

# 3) Export
write_csv(green_table_full, "outputs/green_percentages_by_year_country_sector.csv")


green_table_pretty <- green_table_full %>%
  mutate(
    share_green_pct = round(share_green * 100, 1),
    share_green_among_meat_pct = round(share_green_among_meat * 100, 1),
    meat_share_pct = round(meat_share * 100, 1)
  ) %>%
  select(YEAR, subsector, line_group, n_notices,
         share_green_pct, share_green_among_meat_pct, meat_share_pct)

write_csv(green_table_pretty, "outputs/green_percentages_by_year_country_sector_pretty.csv")


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
## GREEN EVAL WEIGHTINGS

# ---- helper: split on | and clean ----
split_pipe <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- str_squish(x)
  if (x == "") return(character(0))
  unlist(str_split(x, "\\|", n = Inf))
}

# ---- helper: parse weights like "40", "40.5", "40%", "40/60" (we'll use first number per element) ----
parse_weight_vec <- function(w) {
  w <- str_replace_all(w, ",", ".")
  as.numeric(str_extract(w, "\\d+\\.?\\d*"))
}

# ---- main: explode one record into criterion-weight pairs ----
explode_criteria_weights <- function(criteria_str, weights_str) {
  crit <- split_pipe(criteria_str) %>% str_squish()
  wraw <- split_pipe(weights_str) %>% str_squish()
  wnum <- parse_weight_vec(wraw)

  # handle empties
  if (length(crit) == 0 && length(wnum) == 0) {
    return(tibble(criterion = character(0), weight = numeric(0)))
  }

  # if one side is length 1 and the other >1, replicate the 1 (rare but happens)
  if (length(crit) == 1 && length(wnum) > 1) crit <- rep(crit, length(wnum))
  if (length(wnum) == 1 && length(crit) > 1) wnum <- rep(wnum, length(crit))

  # if mismatch persists, keep only aligned pairs up to min length
  m <- min(length(crit), length(wnum))
  crit <- crit[seq_len(m)]
  wnum <- wnum[seq_len(m)]

  tibble(criterion = crit, weight = wnum)
}

# ---- build long table: one row per criterion per notice ----
criteria_long <- ted_sub %>%  # or ted_sub2 if you already have it; ted_sub is fine here
  select(ID_NOTICE_CAN, YEAR, subsector, ISO_COUNTRY_CODE, line_group,
         CRIT_CODE, CRIT_CRITERIA, CRIT_WEIGHTS) %>%
  mutate(
    # robust text clean (handles weird PDF encoding artifacts)
    CRIT_CRITERIA = ifelse(is.na(CRIT_CRITERIA), "", CRIT_CRITERIA),
    CRIT_CRITERIA = str_squish(CRIT_CRITERIA),
    CRIT_CRITERIA = stringi::stri_trans_general(CRIT_CRITERIA, "Latin-ASCII"),

    CRIT_WEIGHTS  = ifelse(is.na(CRIT_WEIGHTS), "", as.character(CRIT_WEIGHTS)),
    CRIT_WEIGHTS  = str_squish(CRIT_WEIGHTS)
  ) %>%
  mutate(
    is_meat_row = as.integer(toupper(trimws(CRIT_CODE)) == "M")
  ) %>%
  # explode row-wise using pmap
  mutate(pairs = pmap(list(CRIT_CRITERIA, CRIT_WEIGHTS), explode_criteria_weights)) %>%
  unnest(pairs) %>%
  # remove unusable
  filter(!is.na(weight), weight > 0, !is.na(criterion), criterion != "")


criteria_long <- criteria_long %>%
  mutate(
    criterion_norm = tolower(criterion),
    criterion_norm = stringi::stri_trans_general(criterion_norm, "Latin-ASCII"),
    green_criterion = as.integer(str_detect(criterion_norm, green_regex))
  )


notice_weights <- criteria_long %>%
  group_by(ID_NOTICE_CAN, YEAR, subsector, ISO_COUNTRY_CODE, line_group) %>%
  summarise(
    is_meat = as.integer(any(is_meat_row == 1L)),
    total_weight = sum(weight, na.rm = TRUE),
    green_weight = sum(weight[green_criterion == 1L], na.rm = TRUE),
    green_weight_share = ifelse(total_weight > 0, green_weight / total_weight, NA_real_),
    n_criteria = n(),
    .groups = "drop"
  )


notice_weights_clean <- notice_weights %>%
  filter(
    is_meat == 1,
    total_weight >= 80, total_weight <= 120  # adjustable
  )



# country-level summary
weight_country <- notice_weights_clean %>%
  group_by(YEAR, subsector, line_group) %>%
  summarise(
    n_notices = n(),
    mean_green_weight_share = mean(green_weight_share, na.rm = TRUE),
    median_green_weight_share = median(green_weight_share, na.rm = TRUE),
    .groups = "drop"
  )

# EU aggregate across all EU (use notice_weights_clean before filtering by DE/NL/SE)
weight_eu <- notice_weights_clean %>%
  group_by(YEAR, subsector) %>%
  summarise(
    line_group = "EU",
    n_notices = n(),
    mean_green_weight_share = mean(green_weight_share, na.rm = TRUE),
    median_green_weight_share = median(green_weight_share, na.rm = TRUE),
    .groups = "drop"
  )

weight_table <- bind_rows(
  weight_country %>% filter(line_group %in% c("DE","NL","SE")),
  weight_eu
) %>%
  arrange(subsector, line_group, YEAR)

dir.create("outputs", showWarnings = FALSE)
write.csv(weight_table, "outputs/green_weight_share_by_year_country_subsector.csv", row.names = FALSE)

# pretty percent version
weight_table_pretty <- weight_table %>%
  mutate(
    mean_green_weight_pct = round(mean_green_weight_share * 100, 1),
    median_green_weight_pct = round(median_green_weight_share * 100, 1)
  )

write.csv(weight_table_pretty, "outputs/green_weight_share_by_year_country_subsector_pretty.csv", row.names = FALSE)


# Plotting

cols <- c("EU"="grey30", "NL"="orange", "SE"="blue", "DE"="darkred")

p_weight <- ggplot(weight_table, aes(x = YEAR, y = mean_green_weight_share, color = line_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Weight assigned to environmental criteria within MEAT evaluations",
    subtitle = "Computed from pipe-separated CRIT_CRITERIA and CRIT_WEIGHTS fields in TED CAN",
    x = NULL, y = "Mean environmental weight share", color = NULL
  )

p_weight

# 1) Do criteria and weights align?
criteria_long %>% count(n_criteria = 1) # placeholder; better:
criteria_long %>%
  group_by(ID_NOTICE_CAN) %>%
  summarise(k = n(), total_weight = sum(weight), .groups="drop") %>%
  summary()

# 2) See a few exploded examples
criteria_long %>%
  select(ID_NOTICE_CAN, CRIT_CODE, criterion, weight, green_criterion) %>%
  slice_sample(n = 20)

# EU line here makes no sense as the green flag is not going to work with all EU languages
# result -> I ignore EU and just do individual countries
# Remove EU line and plot only DE/NL/SE
weight_table_no_eu <- weight_table %>%
  filter(line_group %in% c("DE", "NL", "SE"))

p_weight_no_eu <- ggplot(weight_table_no_eu,
                         aes(x = YEAR, y = mean_green_weight_share, color = line_group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ subsector) +
  scale_color_manual(values = c("NL"="orange", "SE"="blue", "DE"="darkred")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  geom_vline(xintercept = 2016, linetype = "dotted") +
  geom_vline(xintercept = 2017, linetype = "dotted") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Weight assigned to environmental criteria within MEAT evaluations",
    subtitle = "DE vs NL vs SE (computed from CRIT_CRITERIA and CRIT_WEIGHTS)",
    x = NULL, y = "Mean environmental weight share", color = NULL
  )

p_weight_no_eu

