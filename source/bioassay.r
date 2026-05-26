##### Packages 

library(styler)
tidyverse_style()


bioassay_1 = data.frame(
    choice = c(
        "mc1",
        "mt5",
        "no_choice"
    ),
    count = c(
        9,
        6,
        5
    )
)

bioassay_2 = data.frame(
    choice = c(
        "mc2",
        "mt4",
        "no_choice"
    ),
    count = c(
        13,
        3,
        4
    )
)

bioassay_3 = data.frame(
    choice = c(
        "mc3",
        "mt3",
        "no_choice"
    ),
    count = c(
        12,
        7,
        1
    )
)

bioassay = data.frame(
    choice = c(
        "mc1",
        "mc2",
        "mc3",
        "mt5",
        "mt4",
        "mt3",
        "no choice",
        "no choice",
        "no choice"
    ),
    count = c(
        9,
        13,
        12,
        6,
        3,
        7,
        5,
        4,
        1
    )
)
