######################################################################################

##### Packages 

######################################################################################

library(ggplot2)
library(paletteer)
library(patchwork)
library(ggthemes)
library(extrafont)
library(systemfonts)
tidyverse_style()
library(showtext)





######################################################################################

##### Plotting the bioassay results and doing the statistics on it 

######################################################################################

plot_a = ggplot(
    data = bioassay,
    aes(
        x = choice,
        y = count,
        fill = choice,
    )) + 
    geom_col(
        color = "#dcdcdc"
    ) + 
    scale_fill_manual(
        values = c(
             "#B7E6A5FF",
             "#46AEA0FF",
             "#00718BFF",
             "#B7E6A5FF",
             "#46AEA0FF",
             "#00718BFF",
             "#F7FEAEFF"
        )
    ) + 
    theme_fivethirtyeight() + 
    labs(
        title = "Larval choice bioassay",
        subtitle = "by color pairs",
        y = "Number of larvae"
    ) +
    theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.position = "none",
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 60, face = "bold"),
        plot.subtitle = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 20),
        text = element_text(family = ".SF Compact Rounded")
    ) 
# x11()
# plot_a

# quartz.save(
#     "./plots/bioassay_plot.png",
#     type = "png",
#     dpi = 300,
#     width = 11,
#     height = 11
#     )

### statistical part 

test1 = binom.test(
    x = 9,
    n = 9 + 6,
    p = 0.5
)
test1
    # exact binomial test 
    # there is no statistical difference from a 50/50 choice (p = 0.6072)

test2 = binom.test(
    x = 13,
    n = 13 + 3,
    p = 0.5
)
test2
    # success rate is statistically different from 0.5 (p = 0.02127)

test3 = binom.test(
    x = 12,
    n = 12 + 7,
    p = 0.5
)
test3
    # success rate isn't statiscally different from 0.5 (p = 0.3593)





######################################################################################

##### Leaf damage plotting and statistical differences 

######################################################################################

#    species leaf_damage
# 1      mc1    1.779000
# 2      mc2    0.893750
# 3      mc3    1.065333
# 4      mc4    4.208500
# 5      mc5    1.467000
# 6      mt1    1.493500
# 7      mt2    1.382000
# 8      mt3    0.792250
# 9      mt4    1.198600
# 10     mt5    1.522000

plot_b = ggplot(
    data = leaf,
    aes(
        x = treatment,
        y = leaf_damage,
        fill = treatment
    )) + 
    geom_boxplot(
        color = "#373737"
    ) + 
    scale_fill_manual(
        values = c(
            "#FDFBE4FF",
            "#F0C6C3FF"
        )
    ) + 
    theme_fivethirtyeight() + 
    labs(
        y = "Leaf damage [cm2]",
        title = "Leave's cover ate by larvae",
        subtitle = "control against treatment"
    ) + 
    theme(
        axis.title.y = element_text(size = 20),
        panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_line(color = "grey90"),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 60),
        plot.subtitle = element_text(size = 25, face = "bold"),
        text = element_text(family = ".SF Compact Rounded")
    ) 

# x11()
# plot_b

# quartz.save(
#     "./plots/leaf_plot.png",
#     type = "png",
#     dpi = 300,
#     width = 12,
#     height = 12
#     )


### statistical test 

control = leaf$leaf_damage[leaf$treatment == "control"]
control

treat = leaf$leaf_damage[leaf$treatment == "treatment"]
treat

# verification of normality 
shapiro.test(control)
    # it is (p = 0.57)
shapiro.test(treat)
    # it is also (p = 0.23)

# equality of variances 
var.test(
    control,
    treat
)
    # variances are equals (p = 0.47)

# two sample t.test 

t.test(
    control,
    treat,
    var.equal = T
)
    # means are statistically the same (p = 0.55)





######################################################################################

##### phytohormones plot and statistics

######################################################################################

phyto$treatment = c(
    "control",
    "control",
    "control",
    "control",
    "control",
    "treatment",
    "treatment",
    "treatment",
    "treatment",
    "treatment"
)

phyto = phyto %>% 
    filter(
        is.na(SA) == 0 
    )
phyto


plot_c = ggplot(
    data = phyto,
    aes(
        x = treatment,
        y = SA,
        fill = treatment
    )) +
    geom_boxplot() + 
    scale_fill_manual(
        values = c(
            "#FED789FF",
            "#A4BED5FF"
        )
    ) +
    theme_fivethirtyeight() +
    labs(
        title = "Phytohormones content",
        subtitle = "A. Salicylic acid",
        y = "Relative content [ng/mL]"
    )  + 
    theme(
        axis.title.y = element_text(size = 15),
        panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_line(color = "grey90"),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 80, face = "bold"),
        plot.subtitle = element_text(size = 25, face = "bold"),
        text = element_text(family = ".SF Compact Rounded"),
        axis.text.x = element_blank()
    ) 
# plot_c

plot_d = ggplot(
    data = phyto,
    aes(
        x = treatment,
        y = ABA,
        fill = treatment
    )) +
    geom_boxplot() + 
    scale_fill_manual(
        values = c(
            "#FED789FF",
            "#A4BED5FF"
        )
    ) +
    theme_fivethirtyeight() +
    labs(
        subtitle = "B. Absisic acid"
    )  + 
    theme(
        panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_line(color = "grey90"),
        axis.text = element_text(size = 20),
        plot.subtitle = element_text(size = 25, face = "bold"),
        text = element_text(family = ".SF Compact Rounded"),
        axis.text.x = element_blank()
    )

plot_e = ggplot(
    data = phyto,
    aes(
        x = treatment,
        y = JA_Ile,
        fill = treatment
    )) +
    geom_boxplot() + 
    scale_fill_manual(
        values = c(
            "#FED789FF",
            "#A4BED5FF"
        )
    ) +
    theme_fivethirtyeight() +
    labs(
        subtitle = "C. Jasmonic acid isoleucine"
    )  + 
    theme(
        panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_line(color = "grey90"),
        axis.text = element_text(size = 20),
        plot.subtitle = element_text(size = 25, face = "bold"),
        text = element_text(family = ".SF Compact Rounded"),
        axis.text.x = element_blank()
    )

plot_f = ggplot(
    data = phyto,
    aes(
        x = treatment,
        y = JA,
        fill = treatment
    )) +
    geom_boxplot() + 
    scale_fill_manual(
        values = c(
            "#FED789FF",
            "#A4BED5FF"
        )
    ) +
    theme_fivethirtyeight() +
    labs(
        subtitle = "D. Jasmonic acid",
        y = "Relative content [ng/mL]"
    )  + 
    theme(
        axis.title.y = element_text(size = 15),
        panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_line(color = "grey90"),
        axis.text = element_text(size = 20),
        plot.subtitle = element_text(size = 25, face = "bold"),
        text = element_text(family = ".SF Compact Rounded"),
        axis.text.x = element_blank()
    )

plot_g = ggplot(
    data = phyto,
    aes(
        x = treatment,
        y = OPDA,
        fill = treatment
    )) +
    geom_boxplot() + 
    scale_fill_manual(
        values = c(
            "#FED789FF",
            "#A4BED5FF"
        )
    ) +
    theme_fivethirtyeight() +
    labs(
        subtitle = "E. 12-oxo-phytodienoic acid"
    )  + 
    theme(
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 30, face = "bold"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_line(color = "grey90"),
        axis.text = element_text(size = 20),
        plot.subtitle = element_text(size = 25, face = "bold"),
        text = element_text(family = ".SF Compact Rounded"),
        axis.text.x = element_blank()
    ) 

plot_h = ggplot(
    data = phyto,
    aes(
        x = treatment,
        y = IAA,
        fill = treatment
    )) +
    geom_boxplot() + 
    scale_fill_manual(
        values = c(
            "#FED789FF",
            "#A4BED5FF"
        )
    ) +
    theme_fivethirtyeight() +
    labs(
        subtitle = "F. Indole-3-acetic acid"
    )  + 
    theme(
        panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_line(color = "grey90"),
        axis.text = element_text(size = 20),
        plot.subtitle = element_text(size = 25, face = "bold"),
        text = element_text(family = ".SF Compact Rounded"),
        axis.text.x = element_blank()
    ) 

plot_h

# x11()
# wrap_plots(
#     plot_c,
#     plot_d,
#     plot_e,
#     plot_f,
#     plot_g,
#     plot_h,
#     ncol = 3)

# quartz.save(
#     "./plots/phyto_plot.png",
#     type = "png",
#     dpi = 300,
#     width = 20,
#     height = 12
#     )



### statistical part

SA_control = phyto$SA[phyto$treatment == "control"]
SA_treat = phyto$SA[phyto$treatment == "treatment"]

BA_control = phyto$ABA[phyto$treatment == "control"]
BA_treat = phyto$ABA[phyto$treatment == "treatment"]

JA_Ile_control = phyto$JA_Ile[phyto$treatment == "control"]
JA_Ile_treat = phyto$JA_Ile[phyto$treatment == "treatment"]

JA_control = phyto$JA[phyto$treatment == "control"]
JA_treat = phyto$JA[phyto$treatment == "treatment"]

OPDA_control = phyto$OPDA[phyto$treatment == "control"]
OPDA_treat = phyto$OPDA[phyto$treatment == "treatment"]

IAA_control = phyto$IAA[phyto$treatment == "control"]
IAA_treat = phyto$IAA[phyto$treatment == "treatment"]

treat = list(
    SA_control,
    SA_treat,
    BA_control,
    BA_treat,
    JA_Ile_control,
    JA_Ile_treat,
    JA_control,
    JA_treat,
    OPDA_control,
    OPDA_treat,
    IAA_control,
    IAA_treat
)

for(i in treat){
    print(shapiro.test(i))
}
    # all the groups are normally distributed (p > 0.05) except for BA_treat (p = 0.025)

# equality of variances

var.test(
    SA_control,
    SA_treat
)
    # variances are equal (p = 0.58)

var.test(
    BA_control,
    BA_treat
)
    # variances are equal (p = 0.69)

var.test(
    JA_Ile_control,
    JA_Ile_treat
)
    # variances are not equal (p = 0.001)

var.test(
    JA_control,
    JA_treat
)
    # variances are equal (p = 0.09)

var.test(
    OPDA_control,
    OPDA_treat
)
    # variances are equal (p = 0.18)

var.test(
    IAA_control,
    IAA_treat
)
    # variances are equal (p = 0.82)

# t.test

t.test(
    log(SA_control),
    log(SA_treat),
    var.equal = T
)
    # means are statistically indifferent (df = 6, p = 0.5379)

t.test(
    log(BA_control),
    log(BA_treat),
    var.equal = T
)
    # means are statistically indifferent (df = 6, p = 0.076)

t.test(
    log(JA_Ile_control),
    log(JA_Ile_treat),
    var.equal = F
)
    # means are statistically indifferent (df = 2.017, p = 0.2593)

t.test(
    log(JA_control),
    log(JA_treat),
    var.equal = T
)
    # means are statistically indifferent (df = 6, p = 0.79)

t.test(
    log(OPDA_control),
    log(OPDA_treat),
    var.equal = T
)
    # means are statistically indifferent (df = 6, p = 0.36)

t.test(
    log(IAA_control),
    log(IAA_treat),
    var.equal = T
)
    # means are statistically indifferent (df = 5, p = 0.28)





######################################################################################

##### gcms (chatgpt helped me a lot for this part ALSO) comparison between groups treatment/control

######################################################################################

library(tidyverse)
library(pheatmap)
library(FactoMineR)
library(factoextra)
library(RColorBrewer)
library(reshape2)
library(corrplot)
library(viridis)



## importation des données 

# Lire le fichier CSV
raw_data <- read.csv("./data/gcms_clean.csv", check.names = FALSE)

# Aperçu
head(raw_data)
dim(raw_data)



## préparation des données

# Conserver les noms des échantillons
sample_names <- raw_data$Data_File_Name

# Retirer les colonnes non numériques
clean_data = raw_data[, -1:-2]

# Convertir en matrice numérique
clean_data <- as.data.frame(lapply(clean_data, as.numeric))

# Ajouter les noms de lignes
rownames(clean_data) <- sample_names

# Vérification
str(clean_data)



## normalisation des données

# Transformation log10
log_data <- log10(clean_data + 1)

# Standardisation (centrage-réduction)
scaled_data <- scale(log_data)




## statistique descriptive

# Moyennes par composé
compound_means <- colMeans(clean_data)

# Ecart-type par composé
compound_sd <- apply(clean_data, 2, sd)

# Tableau récapitulatif
stats_table <- data.frame(
  Compound = names(compound_means),
  Mean = compound_means,
  SD = compound_sd
)

# Trier par abondance moyenne
stats_table <- stats_table %>%
  arrange(desc(Mean))

head(stats_table, 10)



## visualisation des composés les plus abondants

# Top 15 composés
Top15 <- stats_table %>%
  slice_max(order_by = Mean, n = 15)

# Graphique
library(ggplot2)

ggplot(Top15, aes(x = reorder(Compound, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Top 15 des composés les plus abondants",
    x = "Composé",
    y = "Abondance moyenne"
  )



## 

# Heatmap des données normalisées
pheatmap(
  scaled_data,
  color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "complete",
  fontsize_row = 10,
  fontsize_col = 6,
  main = "Heatmap GC-MS des échantillons"
)



## pca 

# PCA
pca_res <- PCA(log_data, graph = FALSE)

# Variance expliquée
fviz_eig(pca_res,
         addlabels = TRUE,
         ylim = c(0, 60),
         barfill = "steelblue",
         barcolor = "black") +
  theme_minimal()



## projection des échantillons dans l'espace 

fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 5,
  fill.ind = "darkred",
  col.ind = "black",
  repel = TRUE
) +
  theme_minimal() +
  labs(title = "PCA des échantillons GC-MS")



## contribution des composés à la pca

fviz_pca_var(
  pca_res,
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  repel = TRUE
) +
  theme_minimal() +
  labs(title = "Compounds contribution to PCA") +
    theme(
        plot.title = element_text(size = 30, face = "bold"),
        text = element_text(family = ".SF Compact Rounded")
    )

# quartz.save(
#   "./plots/pca_contrib_gcms.png",
#   type = "png",
#   dpi = 300,
#   width = 12,
#   height = 12
# )


## comparaison entre contrôles et traitements 

# Définition des groupes
sample_group <- data.frame(
  Sample = rownames(log_data),
  Group = c(
    "Treatment",
    "Control",
    "Supposed_treatment",
    "Control",
    "Supposed_control",
    "Treatment"
  )
)

sample_group

# PCA colorée par groupe
x11()
fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  pointsize = 5,
  habillage = sample_group$Group,
  palette = c(
    "#88A0DCFF",
    "#7C4B73FF",
    "#AB3329FF",
    "#7C4B73FF",
    "#F9D14AFF",
    "#88A0DCFF"
   ),
  addEllipses = FALSE,
  ellipse.level = 0.95,
  repel = TRUE
) +
  theme_minimal(
    base_size = 14
) +
  labs(
    title = "PCA",
    subtitle = "Control vs Treatment"
) +
theme_fivethirtyeight() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold"),
    legend.background = element_rect(fill = "white"),
    plot.title = element_text(size = 40, face = "bold"),
    plot.subtitle = element_text(size = 20, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

# quartz.save(
#   "./plots/pca_gcms.png",
#   type = "png",
#   dpi = 300,
#   width = 12,
#   height = 12
# )

# #88A0DCFF, #381A61FF, #7C4B73FF, #ED968CFF, #AB3329FF, #E78429FF, #F9D14AFF

# Annotation des lignes
annotation_row <- data.frame(
  Group = sample_group$Group
)
rownames(annotation_row) <- sample_group$Sample

# Heatmap annotée
pheatmap(
  scaled_data,
  annotation_row = annotation_row,
  color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "complete",
  fontsize_row = 10,
  fontsize_col = 6,
  main = "Heatmap GC-MS : Contrôles vs Traitements"
)



# Calcul des moyennes par groupe
control_idx <- sample_group$Group == "Control"
treatment_idx <- sample_group$Group == "Treatment"

control_mean <- colMeans(log_data[control_idx, ])
treatment_mean <- colMeans(log_data[treatment_idx, ])

# Fold-change
fold_change <- treatment_mean - control_mean

# Test t pour chaque composé
p_values <- sapply(colnames(log_data), function(compound) {
  t.test(
    log_data[treatment_idx, compound],
    log_data[control_idx, compound]
  )$p.value
})

# Correction multiple testing
adj_pvalues <- p.adjust(p_values, method = "fdr")

# Tableau résultats
diff_results <- data.frame(
  Compound = colnames(log_data),
  ControlMean = control_mean,
  TreatmentMean = treatment_mean,
  Log2FC = fold_change,
  Pvalue = p_values,
  FDR = adj_pvalues
)

# Tri
diff_results <- diff_results %>%
  arrange(FDR)

head(diff_results)



# Préparer les données
volcano_data <- diff_results %>%
  mutate(
    Significant = ifelse(FDR < 0.05 & abs(Log2FC) > 1,
                         "Yes",
                         "No")
  )

# Volcano plot
ggplot(volcano_data,
       aes(x = Log2FC,
           y = -log10(FDR),
           color = Significant)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("grey70", "red")) +
  labs(
    title = "Volcano Plot : Traitements vs Contrôles",
    x = "Différence moyenne (log)",
    y = "-log10(FDR)"
  )

# Top composés significatifs
TopMarkers <- volcano_data %>%
  filter(FDR < 0.05) %>%
  arrange(desc(abs(Log2FC))) %>%
  slice(1:20)

# Graphique
ggplot(TopMarkers,
       aes(x = reorder(Compound, Log2FC),
           y = Log2FC,
           fill = Log2FC > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("steelblue", "darkred")) +
  labs(
    title = "Top composés discriminants",
    x = "Composé",
    y = "Différence traitement - contrôle"
  )



## clustering hiérarchique des échantillons

# Distance euclidienne
sample_dist <- dist(scaled_data)

# Clustering
hc <- hclust(sample_dist, method = "ward.D2")

# Dendrogramme
x11()
plot(
  hc,
  main = "Hierarchical Clustering of Samples",
  xlab = "Echantillons",
  sub = "",
  cex = 1.2
)

library(ggdendro)
dhc <- dendro_data(hc, type = "rectangle")

ggplot() +
  geom_segment(
    data = dhc$segments,
    aes(x = x, y = y, xend = xend, yend = yend)
  ) +
  geom_text(
    data = dhc$labels,
    aes(x = x, y = y, label = label),
    angle = 90,
    vjust = -0.5,
    hjust = 0,
    size = 8,
    family = ".SF Compact Rounded"
  ) +
  labs(
    title = "Hierarchical clustering of samples",
    subtitle = "Dendrogram based on Euclidean distance",
    x = "Samples",
    y = "Distance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 35, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid = element_blank(),
    text = element_text(family = ".SF Compact Rounded")
  )

# quartz.save(
#   "./plots/dendrogram_gcms.png",
#   type = "png",
#   dpi = 300,
#   width = 9,
#   height = 12
# )


## corrélation entre les échantillons

# Matrice de corrélation
corr_matrix <- cor(t(log_data))

# Visualisation
corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 0.7,
  col = colorRampPalette(c("blue", "white", "red"))(200)
)



## analyses des composés les plus variables

# Variance par composé
compound_var <- apply(log_data, 2, var)

var_table <- data.frame(
  Compound = names(compound_var),
  Variance = compound_var
)

# Top 20 variables
TopVar <- var_table %>%
  arrange(desc(Variance)) %>%
  slice(1:20)

# Graphique
ggplot(TopVar, aes(x = reorder(Compound, Variance), y = Variance)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Top 20 des composés les plus variables",
    x = "Composé",
    y = "Variance"
  )




## Comparaison globale des abondances entre contrôles et traitements

sample_group <- data.frame(
  Sample = rownames(log_data),
  Group = c(
    "Treatment",
    "Control",
    "Treatment",
    "Control",
    "Control",
    "Treatment"
  )
)

clean_data

# Passage en format long
abundance_long <- log_data %>%
  as.data.frame() %>%
  rownames_to_column("Sample") %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Compound",
    values_to = "Abundance"
  )

# Ajouter les groupes
abundance_long <- abundance_long %>%
  left_join(sample_group, by = c("Sample" = "Sample"))

head(abundance_long)

abundance_long = abundance_long %>%
    filter(
        Abundance > 0
    )

ggplot(abundance_long,
       aes(x = Group,
           y = Abundance,
           fill = Group)) +
  geom_boxplot(alpha = 0.8,
               width = 0.6,
               outlier.shape = 16) +
  geom_jitter(width = 0.15,
              alpha = 0.4,
              size = 1.5) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c(
    "#FED789FF",
    "#A4BED5FF"
  )) +
  labs(
    title = "Abundances of volatile compounds",
    subtitle = "Control vs Treatment",
    x = "Groupe",
    y = "log10(Abondance + 1)"
  )  + 
  theme(
    plot.title = element_text(size = 40, face = "bold"),
    plot.subtitle = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 15),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    plot.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text = element_text(size = 20),
    text = element_text(family = ".SF Compact Rounded")
  )

# quartz.save(
#   "./plots/abundance_gcms.png",
#   type = "png",
#   dpi = 300,
#   width = 12,
#   height = 12
# )

wilcox_test <- wilcox.test(
  Abundance ~ Group,
  data = abundance_long
)
wilcox_test


gcms_control = abundance_long$Abundance[abundance_long$Group == "Control"]
gcms_treat = abundance_long$Abundance[abundance_long$Group == "Treatment"]

shapiro.test(gcms_control)
    # not normal 
shapiro.test(gcms_treat)
    # not normal 

wilcox.test(
  Abundance ~ Group,
  data = abundance_long
)  
    # there is a significant difference between the two groups (p = 0.008)



