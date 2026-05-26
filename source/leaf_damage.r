##### Packages

tidyverse_style()
library(dplyr)


##### files import 

for(i in 1:5){

    mc = paste0("./data/leaf_damage/mc", i ,".csv")
    mt = paste0("./data/leaf_damage/mt", i ,".csv")

    assign(
        paste0("mc",i),
        read.csv(mc)
    )

    assign(
        paste0("mt", i),
        read.csv(mt)
    )


    mc = get(paste0("mc", i))
    mt = get(paste0("mt", i))

    assign(
        paste0("mc", i , "_area"),
        mc %>%
            select(Area)
    )

      assign(
        paste0("mt", i , "_area"),
        mt %>%
            select(Area)
    )


    mc = get(paste0("mc", i ,"_area"))
    mt = get(paste0("mt", i ,"_area"))

    assign(
        paste0("mc", i ,"_total"),
        mean(mc$Area)
    )
    assign(
        paste0("mt", i ,"_total"),
        mean(mt$Area)
    )

}

leaf = data.frame(
    species = c(
        "mc1", "mc2", "mc3", "mc4", "mc5",
        "mt1", "mt2", "mt3", "mt4", "mt5"
    ),
    leaf_damage = c(
        mc1_total, mc2_total, mc3_total, mc4_total, mc5_total,
        mt1_total, mt2_total, mt3_total, mt4_total, mt5_total
    ),
    treatment = c(
        "control", "control","control","control","control",
        "treatment", "treatment", "treatment", "treatment", "treatment"
    )
)

head(leaf)


