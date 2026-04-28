# =============================================================================
# HERBARIUM DATA WRANGLING & ANALYSIS
# =============================================================================
# Steps:
#   1. Remove cultivated species
#   2. Remove garden/planted localities
#   3. Fill missing coordinates from Gazetteer
#   4. Extract species names from habitat notes
#   5. Remove non-species rows
#   6. Update/validate species names via Taxonstand
#   7. Create a density map of all collection points
# =============================================================================

# ── Packages ──────────────────────────────────────────────────────────────────
# Install any missing packages automatically
required_pkgs <- c(
  "tidyverse", # data manipulation
  "readxl", # read excel files
  "writexl", # write excel files
  "ggplot2", # plotting
  "sf", # handle spatial data
  "rnaturalearth", # provide high-quality vector data
  "rnaturalearthdata", # provide low- and medi- resolution vector map
  "viridis" # map data & colour scale
)
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}

library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

devtools::install_github("ecoinfor/U.Taxonstand") # updating plant species
library(U.Taxonstand)

rm(new_pkgs, required_pkgs) # Tidy

# =============================================================================
# DATA LOADING
# =============================================================================

# NOTE: Save 'Herb_collection_25-09-2023' as CSV UTF-8 first to handle special
# characters correctly. The gazetteer can stay as .xls.
herb <- read.csv(
  "data/Herb_collection_25-09-2023.csv",
  na.strings = c("", "NA")
)

gazet <- read_csv("data/gazetteer.csv")

# =============================================================================
# COLUMN SELECTION & RENAMING
# =============================================================================

herb <- herb |>
  select(-c(1:3, 6, 8:12, 14, 15, 19, 21, 22, 33, 35:45, 47:61, 63, 66:97)) |>
  rename(
    id = BRAHMS,
    cat = CATEGORY,
    name = NAMESTRING,
    year = YEAR,
    group = GROUP,
    family = FANAME,
    Genus = GENAME,
    spp = FULLNAME,
    country = CONAME,
    major = MAJOR,
    minor = MINOR,
    locality = LOCALITY,
    loc_note = LOCNOTES,
    y = LAT,
    ns = NS,
    x = LONG,
    ew = EW,
    xy_unit = LLUNIT,
    xy_source = LLORIG,
    habitat_note = HABITATTXT,
    cul = CULTIVATED,
    cul_note = CULTNOTES,
    notes = NOTES
  )

gazet <- gazet |>
  select(-c(1:4, 14, 16:23)) |>
  rename(
    country = coname,
    y = lat,
    x = long,
    xy_unit = llunit,
    xy_source = llorig
  )

# Remove rows with no location data
herb <- herb |> filter(locality != "No loc." | is.na(locality))
gazet <- gazet |> filter(x != 0 | is.na(x))

cat(sprintf("  After location filter — herb: %d rows\n", nrow(herb)))


# =============================================================================
# STEP 1 – Remove cultivated specimens
# =============================================================================

herb <- herb |> filter(!cul %in% "Yes")

# =============================================================================
# STEP 2 – Remove garden / planted localities
# =============================================================================

garden_localities <- c(
  "University Farm",
  "Pamplemousses Botanic Garden",
  "Curepipe Botanic Garden",
  "Ebène",
  "Curepipe Forestry Nursery",
  "Beau Bassin",
  "Trianon",
  "Queen Elizabeth College",
  "Long Mountain Nursery",
  "Chebelle",
  "Morcellement St. André",
  "Pamplemousses MSIRI station",
  "Government House",
  "Royal Botanic Gardens Pamplemousses",
  "Barkly Experimental Station",
  "Barkly",
  "Nursery NPCS",
  "Champs de Mars",
  "Cassis Nursery",
  "Rose Hill",
  "Rose-Hill",
  "MSIRI",
  "Balfour Garden",
  "Richelieu Experimental Station",
  "Bagatelle",
  "Bagatelle S.E.",
  "Roches Brunes",
  "Curepipe C. L. Arboretum",
  "Bramsthan",
  "Endémika",
  "Anahita"
)

# Ferns are kept even from garden localities (as per original logic)
herb <- herb |>
  filter(!(locality %in% garden_localities & group != "fern"))

rm(garden_localities)

# =============================================================================
# STEP 3 – Fill missing coordinates from Gazetteer
# =============================================================================

# Deduplicate gazetteer on locality + minor (keep first occurrence)
gazet_unique <- gazet |>
  group_by(locality, minor) |>
  slice(1) |>
  ungroup()

# Left-join, then coalesce herb coords with gazet coords where herb is missing
herb <- herb |>
  left_join(
    gazet_unique,
    by = c("locality", "minor"),
    suffix = c(".herb", ".gazet")
  ) |>
  mutate(
    x = if_else(is.na(x.herb) | x.herb == 0, x.gazet, x.herb),
    y = if_else(is.na(y.herb) | y.herb == 0, y.gazet, y.herb),
    ns = coalesce(ns.herb, ns.gazet),
    ew = coalesce(ew.herb, ew.gazet),
    xy_unit = coalesce(xy_unit.herb, xy_unit.gazet),
    xy_source = coalesce(xy_source.herb, xy_source.gazet)
  ) |>
  # Drop all the suffixed columns and redundant fields
  select(
    -ends_with(".herb"),
    -ends_with(".gazet"),
    -country.gazet,
    -major.gazet, # these come from the gazet join artefact
    -cul,
    -cul_note
  ) |>
  rename(country = country.herb, major = major.herb)

write_xlsx(herb, "herb_f.xlsx")

rm(gazet, gazet_unique)

# =============================================================================
# STEP 4 – Extract species names from habitat_note column
# =============================================================================

# Regex: capitalised word followed by a lowercase word (Genus species pattern)
extract_species <- function(note) {
  matches <- str_extract_all(note, "\\b[A-Z][a-z]+\\s[a-z]+\\b")
  sapply(matches, paste, collapse = ", ")
}

# Apply extraction and unnest so each extracted name gets its own row
extracted_data <- herb |>
  mutate(species_names = extract_species(habitat_note)) |>
  mutate(species_names = strsplit(species_names, ", ")) |>
  unnest(species_names) |>
  # Clear fields that are irrelevant for extracted records
  mutate(
    across(
      c(group, family, Genus, loc_note, habitat_note, notes, cat, name),
      ~NA
    ),
    source = "H.extract"
  ) |>
  select(-country, -major, -spp) |>
  rename(spp = species_names)

rm(extract_species)

# =============================================================================
# STEP 5 – Remove non-species rows
# =============================================================================

non_species <- c(
  "About one",
  "Additional det",
  "Aigrettes can",
  "Almost in",
  "Along bank",
  "Along crestline",
  "Along forest",
  "Along muddy",
  "Along path",
  "Along ridge",
  "Along ridgeline",
  "Along river",
  "Along road",
  "Along roadside",
  "Along steep",
  "Along stream",
  "Along streams",
  "Along the",
  "Along top",
  "Along valley",
  "Along verandah",
  "Along west",
  "Along with",
  "Also in",
  "Also occurs",
  "Also on",
  "Among basalt",
  "Among dense",
  "Among grasses",
  "Among low",
  "Among the",
  "Amongst dense",
  "Amongst grass",
  "An endemic",
  "An extremely",
  "An important",
  "An occasional",
  "An undetermined",
  "Annotation by",
  "Annual phase",
  "Annual rainfall",
  "Another large",
  "Appears to",
  "Aquatic plant",
  "Area heavily",
  "Area invaded",
  "Area weeded",
  "Argent in",
  "Arid soil",
  "Around the",
  "As a",
  "As an",
  "Ascendant up",
  "Asia and",
  "Associated native",
  "Associated species",
  "Associated with",
  "Association of",
  "At all",
  "At base",
  "At edge",
  "At foot",
  "At least",
  "At sea",
  "At summit",
  "At the",
  "At this",
  # ── S ──
  "Sapotaceae formation",
  "Sapotaceae invaded",
  "Sapotaceae over",
  "Sapotaceae primary",
  "Sapotaceae rich",
  "Sapotaceae spp",
  "Sapotaceae wet",
  "Sappotaceae etc",
  "Savannahs on",
  "Scandant climber",
  "Scandent at",
  "Scarce locally",
  "Scatter isolated",
  "Scattered trees",
  "Scrambler on",
  "Scrambling in",
  "Scrambling over",
  "Scrub land",
  "Scrub vegetation",
  "Scrub with",
  "Scrubland with",
  "Sea level",
  "Seaside on",
  "Seconadry forest",
  "Secondary forest",
  "Secondary foret",
  "Secondary scrub",
  "Secondary scrubland",
  "Secondary thicket",
  "Secondary thorn",
  "Sedge forming",
  "Sedge growing",
  "Sedge rooting",
  "Sedge swamp",
  "Seedlings and",
  "Seedlings growing",
  "Seedlings of",
  "Seems to",
  "Seen on",
  "Semi deciduous",
  "Semideciduous dry",
  "Several dry",
  "Several individuals",
  "Several trees",
  "Sgrowing on",
  "Shaddy area",
  "Shade of",
  "Shady bank",
  "Shady ditch",
  "Shady habitat",
  "Shady humid",
  "hady moist",
  "Shady native",
  "Shady places",
  "Shady ravine",
  "Shady site",
  "Shady sites",
  "Shallow bay",
  "Shallow marshy",
  "Shallow rocky",
  "Shallow swamp",
  "Sheltered behind",
  "Shrub in",
  "Shrub naturalized",
  "Shrubbery vegetation",
  "Shurb in",
  "Side of",
  "Single individual",
  "Site with",
  "Six clumps",
  "Slightly invaded",
  "Slope ca",
  "Slopes in",
  "Slopes of",
  "Sloping and",
  "Small clumps",
  "Small colony",
  "Small ground",
  "Small patch",
  "Small plant",
  "Small population",
  "Small prostrate",
  "Small relict",
  "Small roadside",
  "Small stream",
  "Small tufts",
  "Soil appearing",
  "Soil full",
  "Soil more",
  "Soil shallow",
  "Soil unweathered",
  "Sol sale",
  "Solitary in",
  "Some branches",
  "Some indigenous",
  "Some large",
  "Some of",
  "Some patches",
  "Some plants",
  "Some regeneration",
  "Sometimes cultivated",
  "Sometimes in",
  "Sometimes with",
  "Somewhat nearer",
  "Souillac between",
  "Souillac cemetery",
  "Sous bois",
  "South coast",
  "South of",
  "South west",
  "Southern flanks",
  "Sparse clump",
  "Species known",
  "Species nearby",
  "Specimen growing",
  "Spores absent",
  "Spores present",
  "Sporling growing",
  "Spreading from",
  "Sprouting from",
  "Spur forest",
  "Sream bank",
  "Sriana near",
  "Standing water",
  "Steam side",
  "Steep dry",
  "Steep hillside",
  "Steep rocky",
  "Steep slope",
  "Steep slopes",
  "Steep south",
  "Steep stony",
  "Stony bare",
  "Stony ground",
  "Strand species",
  "Strand vegetation",
  "Stream and",
  "Stream bank",
  "Stream banks",
  "Stream near",
  "Stream of",
  "Stream side",
  "Sttep forested",
  "Stump of",
  "Stunted and",
  "Stunted forest",
  "Stunted thicket",
  "Sub humid",
  "Subhumid forest",
  "Subhumid forests",
  "Sugar cane",
  "Summit ridge",
  "Sun or",
  "Sun rocky",
  "Sunny sea",
  "Sunny site",
  "Sunny situation",
  "Sur crete",
  "Sur forte",
  "Sur la",
  "Sur le",
  "Sur les",
  "Sur toutes",
  "Sus bois",
  "Swampy valley",
  "Tall tufted",
  "Tamarin and",
  "Tea plantation",
  "Temperature of",
  "Terrestrial fern",
  "Terrestrial growing",
  "Terrestrial species",
  "The forest",
  "The trail",
  "Thirteen clumps",
  "This individual",
  "This is",
  "This liana",
  "This plant",
  "This species",
  "Those growing",
  "Tifted in",
  "To salt",
  "Together with",
  "Top strata",
  "Totally degraded",
  "Trailing herb",
  "Transitional evergreen",
  "Tree by",
  "Tree growing",
  "Tree in",
  "Tree thicket",
  "Tufted with",
  "Twining around",
  "Twining over",
  "Two leaning",
  "Two smaller",
  "Typical of",
  "Una inter",
  "Under canopy",
  "Under cover",
  "Under intermediate",
  "Under shade",
  "Under the",
  "Under tree",
  "Undershrub in",
  "Underside of",
  "University of",
  "Upland bogs",
  "Upland climax",
  "Upland forest",
  "Upland forests",
  "Upland heath",
  "Upland marshes",
  "Upland native",
  "Upland pasture",
  "Upland regions",
  "Upland scrub",
  "Upland shrub",
  "Upland stream",
  "Upland thickets",
  "Upland turf",
  "Upland woods",
  "Uplands and",
  "Uplands generally",
  "Uplands high",
  "Uplands in",
  "Used as",
  "Usually dry",
  "Usually found",
  "Usually growing",
  "Usually more",
  "Usually on",
  "Usually pendent",
  "Usually riverine",
  "Usually with",
  "Vacoas in",
  "Vacoas ridge",
  "Valley forest",
  "Valley slopes",
  "Valley with",
  "Vaughan annotation",
  "Vegetable garden",
  "Vegetation cover",
  "Vegetation incl",
  "Vegetation is",
  "Vegetation low",
  "Vegetation slightly",
  "Vegetation totally",
  "Vegetation very",
  "Verge of",
  "Vertical moist",
  "Vertical rock",
  "Very close",
  "Very common",
  "Very degraded",
  "Very dense",
  "Very disturbed",
  "Very exposed",
  "Very good",
  "Very high",
  "Very humid",
  "Very invaded",
  "Very little",
  "Very localised",
  "Very mossy",
  "Very much",
  "Very occasional",
  "Very recently",
  "Very rocky",
  "Very shady",
  "Very steep",
  "Very wet",
  "Very widely",
  "Vey common",
  "Volcanic crater",
  "Vulnerable to",
  "Waste ground",
  "Waste land",
  "Wasteland near",
  "Water saturated",
  "Waterfalls opposite",
  "Weed by",
  "Weed common",
  "Weed growing",
  "Weed in",
  "Weed of",
  "Weed on",
  "Weeded area",
  "Weedy species",
  "Well preserved",
  "West of",
  "West part",
  "West slopes",
  "Western mountain",
  "Western range",
  "Wet forest",
  "Wet ground",
  "Wet primary",
  "Where is",
  "White or",
  "Whole colony",
  "Wide flattish",
  "Widely distributed",
  "Widely naturalized",
  "Widely planted",
  "Wild cane",
  "Windy site",
  "With hygrophilous",
  "With indigenous",
  "With low",
  "With scattered",
  "Within a",
  "Woodland forest",
  "Woody liane",
  "Young pine"
)

extracted_data <- extracted_data |>
  filter(!spp %in% non_species)

rm(non_species)

# Mark and align original data, then combine
original_data <- herb |>
  mutate(
    species_names = extract_species(habitat_note), # kept for structure parity
    source = "H.original"
  ) |>
  select(-country, -major, -species_names)

# Re-define extract_species here since it was removed above — add it back briefly
extract_species <- function(note) {
  matches <- str_extract_all(note, "\\b[A-Z][a-z]+\\s[a-z]+\\b")
  sapply(matches, paste, collapse = ", ")
}
original_data <- herb |>
  mutate(source = "H.original") |>
  select(-country, -major)

rm(extract_species)

final_data <- bind_rows(original_data, extracted_data) |>
  arrange(id)

write_xlsx(final_data, "herb_f_e.xlsx")

rm(extracted_data, original_data)

# =============================================================================
# STEP 6 – Update / validate species names with Taxonstand
# =============================================================================

# Extract unique species names that look like binomials (Genus species)
unique_spp <- final_data |>
  filter(str_detect(spp, "^[A-Z][a-z]+ [a-z]+")) |>
  distinct(spp) |>
  pull(spp)

# Run Taxonstand — may take several minutes for large lists
# corr = TRUE  : allow minor spelling corrections
# diffchar = 2 : tolerate up to 2 character differences
# max.distance = 1 : Levenshtein distance threshold
taxon_results <- TPL(
  unique_spp,
  corr = TRUE,
  diffchar = 2,
  max.distance = 1
)

# Build a clean lookup: original name → accepted name
name_lookup <- taxon_results |>
  transmute(
    spp_original = paste(Genus, Species), # original queried name
    spp_accepted = case_when(
      Taxonomic.status == "Accepted" ~ paste(New.Genus, New.Species),
      Taxonomic.status == "Synonym" ~ paste(New.Genus, New.Species),
      TRUE ~ paste(Genus, Species) # keep original if unresolved
    ),
    tax_status = Taxonomic.status,
    tax_authority = Authority
  ) |>
  filter(!is.na(spp_accepted))

# Save the full Taxonstand results for audit purposes
write_xlsx(taxon_results, "taxonstand_results.xlsx")
cat("  Saved: taxonstand_results.xlsx\n")

# Join accepted names back into the main dataset
final_data <- final_data |>
  left_join(name_lookup, by = c("spp" = "spp_original")) |>
  mutate(
    spp_updated = if_else(!is.na(spp_accepted), spp_accepted, spp)
  ) |>
  select(-spp_accepted) # keep spp (original), spp_updated (accepted), tax_status, tax_authority

write_xlsx(final_data, "herb_final.xlsx")

rm(taxon_results, name_lookup, unique_spp)

# =============================================================================
# STEP 7 – Density map of all collection points
# =============================================================================

# Keep only records with valid coordinates and convert sign based on NS/EW fields
map_data <- final_data |>
  filter(!is.na(x), !is.na(y), x != 0, y != 0) |>
  mutate(
    lon = if_else(tolower(ew) == "w", -abs(x), abs(x)),
    lat = if_else(tolower(ns) == "s", -abs(y), abs(y))
  ) |>
  filter(between(lon, -180, 180), between(lat, -90, 90)) |>
  distinct(lon, lat) # one point per unique coordinate

cat(sprintf("  Valid coordinate pairs for mapping: %d\n", nrow(map_data)))

# Base map — Mauritius region
world <- ne_countries(scale = "medium", returnclass = "sf")

# Determine bounding box with a small buffer
lon_range <- range(map_data$lon, na.rm = TRUE)
lat_range <- range(map_data$lat, na.rm = TRUE)
buf <- 0.5 # degrees buffer around the data extent

density_map <- ggplot() +
  # Country polygons
  geom_sf(data = world, fill = "grey90", colour = "grey60", linewidth = 0.3) +
  # 2-D density (filled contours)
  stat_density_2d(
    data = map_data,
    aes(x = lon, y = lat, fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.75,
    contour = TRUE,
    bins = 15
  ) +
  # Individual collection points (semi-transparent)
  geom_point(
    data = map_data,
    aes(x = lon, y = lat),
    colour = "white",
    alpha = 0.25,
    size = 0.8
  ) +
  # Colour scale
  scale_fill_viridis_c(
    option = "inferno",
    name = "Density\n(collection density)",
    direction = -1
  ) +
  # Map extent
  coord_sf(
    xlim = c(lon_range[1] - buf, lon_range[2] + buf),
    ylim = c(lat_range[1] - buf, lat_range[2] + buf),
    expand = FALSE
  ) +
  # Labels
  labs(
    title = "Herbarium Collection Density",
    subtitle = sprintf("n = %d unique collection points", nrow(map_data)),
    x = "Longitude",
    y = "Latitude",
    caption = "Data source: Herbarium collection dataset"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey40"),
    legend.position = "right",
    panel.grid = element_line(colour = "grey80", linewidth = 0.3)
  )

# Save the map
ggsave(
  "herbarium_density_map.png",
  plot = density_map,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

print(density_map)
