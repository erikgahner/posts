# R script for "An overview of ggplot2 themes"
# URL: https://erikgahner.dk/2024/an-overview-of-ggplot2-themes/

library("ggplot2")
library("patchwork")

mapcan::mapcan(boundaries = province, type = standard) |>
    ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon() +
    coord_fixed() +
    labs(title = "Default ggplot2 theme")

fig_mapcan1 <- mapcan::mapcan(boundaries = province, type = standard) |>
    ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon() +
    coord_fixed() +
    labs(title = "Default ggplot2 theme")

fig_mapcan2 <- mapcan::mapcan(boundaries = province, type = standard) |>
    ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon() +
    coord_fixed() +
    mapcan::theme_mapcan() +
    labs(title = "mapcan::theme_mapcan()")

fig_mapcan1 + fig_mapcan2
ggsave("ggplot2_mapcan.png", width = 8, height = 4, bg = "white")

fig_tufte1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(title = "Default ggplot2 theme")

fig_tufte2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggthemes::theme_tufte() +
    labs(title = "ggthemes::theme_tufte()")

fig_tufte1 + fig_tufte2
ggsave("ggplot2_tufte.png", width = 8, height = 4, bg = "white")

fig_minimal1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    add2ggplot::theme_white() +
    labs(title = "add2ggplot::theme_white()")

fig_minimal2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggfun::theme_transparent() +
    labs(title = "ggfun::theme_transparent()")

fig_minimal3 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggiraphExtra::theme_clean2() +
    labs(title = "ggiraphExtra::theme_clean2()")

fig_minimal4 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggimage::theme_transparent() +
    labs(title = "ggimage::theme_transparent()")

(fig_minimal1 + fig_minimal2) / (fig_minimal3 + fig_minimal4)

ggsave("ggplot2_minimal.png", width = 8, height = 6, bg = "white")

fig_culture1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    tvthemes::theme_brooklyn99() +
    labs(title = "tvthemes::theme_brooklyn99()")

fig_culture2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    tvthemes::theme_simpsons() +
    labs(title = "tvthemes::theme_simpsons()")

fig_culture3 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    tvthemes::theme_rickAndMorty() +
    labs(title = "tvthemes::theme_rickAndMorty()")

fig_culture4 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    tvthemes::theme_parksAndRec() +
    labs(title = "tvthemes::theme_parksAndRec()")

(fig_culture1 + fig_culture2) / (fig_culture3 + fig_culture4)

ggsave("ggplot2_culture.png", width = 10, height = 6, bg = "white")

fig_media1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggthemes::theme_economist() +
    labs(title = "ggthemes::theme_economist()")

fig_media2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggthemes::theme_fivethirtyeight() +
    labs(title = "ggthemes::theme_fivethirtyeight()")

fig_media3 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggthemes::theme_wsj() +
    labs(title = "ggthemes::theme_wsj()")

fig_media4 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    wsjplot::theme_wsj() +
    labs(title = "wsjplot::theme_wsj()")

(fig_media1 + fig_media2) / (fig_media3 + fig_media4)

ggsave("ggplot2_media.png", width = 10, height = 6, bg = "white")

fig_software1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggthemes::theme_excel_new() +
    labs(title = "ggthemes::theme_excel_new()")

fig_software2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggthemes::theme_stata() +
    labs(title = "ggthemes::theme_stata()")

fig_software3 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    jmvcore::theme_spss() +
    labs(title = "jmvcore::theme_spss()")

fig_software4 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggnuplot::theme_gnuplot() +
    labs(title = "ggnuplot::theme_gnuplot()")

(fig_software1 + fig_software2) / (fig_software3 + fig_software4)

ggsave("ggplot2_software.png", width = 10, height = 6, bg = "white")

fig_models1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggsurvfit::theme_ggsurvfit_default() +
    labs(title = "ggsurvfit::theme_ggsurvfit_default()")

fig_models2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggsurvfit::theme_ggsurvfit_KMunicate() +
    labs(title = "ggsurvfit::theme_ggsurvfit_KMunicate()")

fig_models3 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    survminer::theme_survminer() +
    labs(title = "survminer::theme_survminer()")

fig_models4 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggdist::theme_tidybayes() +
    labs(title = "ggdist::theme_tidybayes()")

(fig_models1 + fig_models2) / (fig_models3 + fig_models4)

ggsave("ggplot2_models.png", width = 10, height = 6, bg = "white")

fig_lines1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    cowplot::theme_minimal_hgrid() +
    labs(title = "cowplot::theme_minimal_hgrid()")

fig_lines2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ezplot::theme_ez() +
    labs(title = "ezplot::theme_ez()")

fig_lines3 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    tinythemes::theme_ipsum_rc() +
    labs(title = "tinythemes::theme_ipsum_rc()")

fig_lines4 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggfun::theme_noxaxis() +
    labs(title = "ggfun::theme_noxaxis()")

(fig_lines1 + fig_lines2) / (fig_lines3 + fig_lines4)

ggsave("ggplot2_lines.png", width = 10, height = 6, bg = "white")

# Save for last because of {hrbrthemes}

fig_dark1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(colour = "white") +
    ggseg::theme_darkbrain() +
    labs(title = "ggseg::theme_darkbrain()")

fig_dark2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(colour = "white") +
    lcars::theme_lcars_dark() +
    labs(title = "lcars::theme_lcars_dark()")

fig_dark3 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(colour = "white") +
    hrbrthemes::theme_ft_rc() +
    labs(title = "hrbrthemes::theme_ft_rc()")

fig_dark4 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(colour = "white") +
    hrbrthemes::theme_modern_rc() +
    labs(title = "hrbrthemes::theme_modern_rc()")

(fig_dark1 + fig_dark2) / (fig_dark3 + fig_dark4)

ggsave("ggplot2_dark.png", width = 9, height = 6, bg = "white")
