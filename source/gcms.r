##### Packages 

library(styler)
tidyverse_style()
library(readxl)




##### data table 

gcms = read_xlsx("./data/GCMS_results.xlsx")

gcms = as.data.frame(gcms)
head(gcms)

colnames(gcms) = 1:213

gcms = gcms %>%
    select(-c(1,2,4,5,6))
head(gcms[, 1:6])

gcms[ ,1:6]
gcms = gcms %>%
    slice(-c(2:11))

gcms[, 1:6]

gcms[, 1] = c(
    "Data_File_Name",
    "mt3",
    "mc3",
    "unknown",
    "mc4",
    "unknown2",
    "mt4"
)


# write.csv(
#     gcms,
#     "./data/gcms_new.csv"
# )

colnames(gcms) = gcms[1,]
gcms = gcms[-1,]

##### Claude part to reshape the dataframe 

# ── GCMS reshape ──────────────────────────────────────────────────────────────
# Objectif :
#   1. Lire le fichier brut (2 lignes d'en-tête imbriquées)
#   2. Supprimer les colonnes Ret Time (colonnes # 1:70 RT)
#   3. Produire un tableau wide :
#        - lignes   = Data_File_Name
#        - colonnes = nom du composé (Name extrait du fichier)
#        - valeurs  = Target Response
# ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)   # readr + dplyr + tidyr + stringr

# Renommer la 1re colonne (index numérique) et garder Data_File_Name
raw <- gcms              

# fix(raw)
# fix(gcms)
# ── 2. Identifier les triplets Name / Ret Time / Target Response ──────────────
# Les colonnes suivent le pattern :  "(# N) Name"  "(# N) Ret Time"  "(# N) Target Response"
all_cols <- colnames(raw)

# Colonnes à garder : Data_File_Name  +  toutes les "(# N) Name"  +  "(# N) Target Response"
# Colonnes à supprimer : "(# N) Ret Time"
cols_rt <- str_subset(all_cols, "\\(# \\d+\\) Ret Time")   # RT à exclure
raw_no_rt <- raw %>% select(-all_of(cols_rt))

# ── 3. Pivot vers le format long, puis wide ────────────────────────────────────
# Pour chaque échantillon (Data_File_Name) et chaque composé (# N) :
#   - extraire le nom du composé  →  colonne "compound"
#   - extraire la Target Response →  valeur

# Construire un petit référentiel composé N → nom du composé
# Les noms des composés sont dans les colonnes "(# N) Name"
# Mais ils varient par ligne (le GCMS peut identifier un composé différent) ;
# on utilise donc la valeur de la colonne Name comme identifiant de composé.

# Approche : pivot_longer sur les colonnes "Target Response", en gardant le numéro # N,
# puis joindre avec le pivot_longer correspondant sur "Name"

df_response <- raw_no_rt %>%
  pivot_longer(
    cols      = matches("\\(# \\d+\\) Target Response"),
    names_to  = "compound_num",
    values_to = "target_response"
  ) %>%
  mutate(compound_num = str_extract(compound_num, "\\d+"))  # garde juste le numéro

df_name <- raw %>%
  select(Data_File_Name, matches("\\(# \\d+\\) Name")) %>%
  pivot_longer(
    cols      = -Data_File_Name,
    names_to  = "compound_num",
    values_to = "compound_name"
  ) %>%
  mutate(compound_num = str_extract(compound_num, "\\d+"))

df_long <- df_response %>%
  left_join(df_name, by = c("Data_File_Name", "compound_num")) %>%
  select(Data_File_Name, compound_name, target_response)

# ── 4. Format wide (lignes = samples, colonnes = composés) ───────────────────
df_wide <- df_long %>%
  pivot_wider(
    id_cols     = Data_File_Name,
    names_from  = compound_name,
    values_from = target_response
  )

# ── 5. Aperçu & export ────────────────────────────────────────────────────────
cat("Dimensions du tableau wide :", nrow(df_wide), "lignes x", ncol(df_wide), "colonnes\n")
print(df_wide[, 1:6])   # aperçu des 5 premières colonnes

# fix(df_wide)

# write_csv(df_wide, "./data/gcms_wide.csv")
# cat("\n✓ Fichier sauvegardé : gcms_wide.csv\n")

str(df_wide)
df_wide = as.data.frame(df_wide)

df_wide = df_wide %>%
  select(-c(
    "Hydroperoxide, hexyl", 
    "3-Hydroxymandelic acid, 3TMS derivative", 
    "(+)-alpha-Pinene", 
    "2H-Pyran-2-one, tetrahydro-3,6-dimethyl-", 
  ))

head(df_wide[, 1:3])

colnames(df_wide)[2] = "Silane, methylvinyl(2,4-dimethylpent-3-yloxy)(methylvinyldodecyloxysilyloxy)-"
head(df_wide[, 1:3])

str(df_wide)
ncol(df_wide)

for(i in 2:61){
  df_wide[, i] = as.numeric(df_wide[, i])
}

str(df_wide)

gcms_clean <- df_wide %>%
  select(
    Data_File_Name,
    where(~ any(. != 0))
  )

gcms_clean[, 15] = c(1.434435e+09, 9.789104e+03, 2.870959e+08, 0.000000e+00, 2.639733e+05, 0) # because there was some negative value (how is that even possible my guy ?!)

gcms_log <- gcms_clean
gcms_log[-1] <- log10(gcms_log[-1] + 1)


# which(is.na(mat))
# mat[, 14]

