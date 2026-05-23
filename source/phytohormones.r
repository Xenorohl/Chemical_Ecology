##### Packages 

library(styler)
tidyverse_style()
library(tidyverse)
library(dplyr)
library(tidyr)



##### result extraction made with Claude AI

txt = readLines("./data/phytohormones_results.txt")

compound_idx = grep("^Compound", txt)

compound_idx_end = c(compound_idx[-1] - 1, length(txt))

# Fonction pour parser chaque compound
parse_compound <- function(start, end, txt){

  block <- txt[start:end]

  # Nom du composé
  compound_name <- sub("^Compound [0-9]+:\\s*", "", block[1])

  # Garder uniquement les lignes de données
  data_lines <- block[stringr::str_detect(block, "^\\d+\\s+\\d+")]

  if(length(data_lines) == 0) return(NULL)

  # Lire le tableau
  df <- read.delim(
    textConnection(data_lines),
    header = FALSE,
    sep = "\t",
    fill = TRUE,
    stringsAsFactors = FALSE
  )

  # Noms attendus
  expected_names <- c(
    "row",
    "id",
    "Name",
    "Type",
    "Std_Conc",
    "RT",
    "Height",
    "Area",
    "IS_Height",
    "IS_Area",
    "Response",
    "Primary_Flags",
    "ng_ml",
    "PercentDev",
    "PercentRec"
  )

  # Ajouter colonnes manquantes si besoin
  if(ncol(df) < length(expected_names)){
    missing_cols <- length(expected_names) - ncol(df)

    for(i in 1:missing_cols){
      df[[paste0("tmp", i)]] <- NA
    }
  }

  # Garder seulement 15 colonnes
  df <- df[, 1:15]

  # Renommer
  colnames(df) <- expected_names

  # Ajouter nom du composé
  df$Compound <- compound_name

  return(df)
}

# Parser tous les composés
all_compounds <- map2_dfr(
  compound_idx,
  compound_idx_end,
  ~parse_compound(.x, .y, txt)
)

# Garder seulement MT1-MT4 et MC1-MC5
samples_to_keep <- c(
  paste0("260508_phytohormones_AKirchhofer_MT", 1:4),
  paste0("260508_phytohormones_AKirchhofer_MC", c(1:5))
)

dataset_final <- all_compounds %>%
  filter(Name %in% samples_to_keep)

# Voir le résultat
head(dataset_final)
# View(dataset_final)


df = dataset_final




##### Keeping only the column of interest


df_clear = df%>%
    select(-c("row", "id", "Type", "Std_Conc", "Height", "Area", "IS_Height", "IS_Area", "Response", "Primary_Flags", "PercentDev", "PercentRec"))
df_clear

df_name = df_clear$Sample <- sub(".*_", "", df$Name)
df_clear$Sample = tolower(df_clear$Sample)
df_clear %>%
  select(-Name)





##### Converting to wide format 

df_wide = df_clear %>%
    select(Sample, Compound, ng_ml) %>%
    pivot_wider(
      names_from = Compound, 
      values_from = ng_ml
    )

df_wide = as.data.frame(df_wide)

head(df_wide)

df_wide = df_wide %>%
    rename(
      species = "Sample",
      SA_d4 = "SA-d4",
      ABA_d6 = "ABA-d6", 
      JA_Ile = "JA-Ile", 
      JA_Ile_13C6 = "JA-Ile-13C6", 
      IAA_d5 = "IAA-d5"
    )

head(df_wide)

df_wide = df_wide %>%
    add_row(
      species = c("mt5", "mc3")
    )

df_wide = df_wide %>%
    arrange(species)

phyto = df_wide 
phyto

