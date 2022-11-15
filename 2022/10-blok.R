# R script for "Visualisering af politiske blokke i meningsmålingerne"
# URL: https://erikgahner.dk/2022/visualisering-af-politiske-blokke-i-meningsmalingerne/

library("tidyverse")
library("ggh4x")
library("colorspace")
library("gridExtra")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

blok_raw <- read_csv("blokModeraterne.csv")

blok_raw |> 
  filter(dato > as.Date("2022-08-01")) |> 
  select(-c("upper", "lower")) |> 
  filter(blok != "Moderaterne") |> 
  pivot_wider(names_from = "blok", values_from = "est") |> 
  ggplot(aes(x = dato)) +
  stat_difference(aes(ymin = Roed, ymax = Blaa), alpha = 0.3) +
  geom_line(aes(y = Roed, color = "Rød blok")) +
  geom_line(aes(y = Blaa, color = "Blå blok")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("#3D85F7", "#C32E5A")) +
  scale_fill_manual(values = c(lighten("#3D85F7"), lighten("#C32E5A"), "white"), labels = c("Blåt flertal", "Rødt flertal", "")) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.12),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", color = "grey20"),
    axis.text = element_text(color = "grey40"),
    plot.margin = margin(20, 30, 20, 30),
    plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, face = "bold", vjust = 0, color = "grey25"),
    plot.caption = element_text(size = 11)
  )

ggsave("blok_difference.png", width = 8, height = 4, bg = "white")

begynd_dato <- as.Date("2022-10-01")
idag_dato <- as.Date(max(df_raw$dato))
begynd_dag <- "1. oktober"
idag_dag <- "24. oktober"

df_raw <- read_csv("blok.csv")
prognose_raw <- read_csv("prognose.csv")
mandater_raw <- read_csv("mandater.csv")
mandater <- mandater_raw |> 
  mutate(blok = case_when(
    party_name %in% c("Socialdemokraterne", "Alternativet", "Radikale Venstre", "SF", "Veganerpartiet", "Enhedslisten", "Frie Grønne") ~ "Rød blok",
    party_name %in% c("Konservative", "Nye Borgerlige", "Liberal Alliance", "Kristendemokraterne", "Danmarksdemokraterne", "Dansk Folkeparti", "Venstre") ~ "Blå blok",
    party_name %in% c("Moderaterne") ~ "Moderaterne"
  )) |> 
  filter(dato == max(dato)) |> 
  group_by(blok) |> 
  summarise(mandater = sum(mandates))

moderaterne <- prognose_raw |> 
  filter(dato >= begynd_dato, party_name == "Moderaterne") |> 
  select(dato, Moderaterne = est)

moderaterne_full <- prognose_raw |> 
  filter(dato >= begynd_dato, party_name == "Moderaterne")

blok <- df_raw |> 
  filter(dato >= begynd_dato) |> 
  left_join(moderaterne, by = "dato") |> 
  mutate(est = ifelse(blok == "Blaa", est - Moderaterne, est)) |> 
  mutate(upper = ifelse(blok == "Blaa", upper - Moderaterne, upper)) |> 
  mutate(lower = ifelse(blok == "Blaa", lower - Moderaterne, lower))


fig1 <- blok |>  
  ggplot(aes(dato, est, colour = blok, fill = blok, ymin = lower, ymax = upper)) +
  geom_line() +
  geom_ribbon(alpha = 0.3) +
  geom_text(x=max(blok$dato) + 2, y = blok$est[blok$dato == max(blok$dato) & blok$blok == "Roed"], label= paste0(round(blok$est[blok$dato == max(blok$dato) & blok$blok == "Roed"], 3) * 100, "%"), color="#FF4136", size=3.5) +
  geom_text(x=max(blok$dato) + 2, y = blok$est[blok$dato == max(blok$dato) & blok$blok == "Blaa"], label= paste0(round(blok$est[blok$dato == max(blok$dato) & blok$blok == "Blaa"], 3) * 100, "%"), color="#0074D9", size=3.5) +
  scale_fill_manual(values = c("blue", "red")) +
  scale_colour_manual(values = c("blue", "red")) +
  geom_hline(yintercept = 0.5, colour = "gray") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA),
        axis.title.x = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0),
        legend.title = element_text(face = "bold"),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(limits = c(begynd_dato, as.Date("2022-11-01")),
               breaks = c(begynd_dato, idag_dato, as.Date("2022-11-01")),
               labels = NULL) + #c(begynd_dag, idag_dag, "1. november")) +
  labs(y = NULL,
       x = NULL,
       title = "Opbakning til rød og blå blok (%)")

fig2 <- moderaterne_full |> 
  ggplot(aes(dato, est, fill = party_name, colour = party_name, ymin = lower, ymax = upper)) +
  geom_line(colour = "#5bc3f5") +
  geom_ribbon(alpha = 0.3) +
  geom_text(x=max(moderaterne_full$dato) + 2, y = moderaterne_full$est[moderaterne_full$dato == max(moderaterne_full$dato) & moderaterne_full$party_name == "Moderaterne"], label= paste0(round(moderaterne_full$est[moderaterne_full$dato == max(moderaterne_full$dato) & moderaterne_full$party_name == "Moderaterne"], 3) * 100, "%"), color="#5bc3f5", size=3.5) +
  scale_fill_manual(values = c("#5bc3f5")) +
  scale_colour_manual(values = c("#5bc3f5")) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA),
        axis.title.x = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0),
        legend.title = element_text(face = "bold"),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(limits = c(begynd_dato, as.Date("2022-11-01")),
               breaks = c(begynd_dato, idag_dato, as.Date("2022-11-01")),
               labels = c(begynd_dag, idag_dag, "1. november")) +
  labs(y = NULL,
       x = NULL,
       title = "Opbakning til Moderaterne (%)")

mandater$fraction = mandater$mandater / sum(mandater$mandater)
mandater$ymax = cumsum(mandater$fraction)
mandater$ymin = c(0, head(mandater$ymax, n=-1))
mandater$labelPosition <- (mandater$ymax + mandater$ymin) / 2
mandater <- mandater |> 
  mutate(label = paste0(blok, "\n(", mandater, ")"))

fig3 <- mandater |> 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=blok)) +
  geom_rect() +
  geom_text(x=1.3, aes(y=labelPosition, label=label, color=blok), size=3.5) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void(base_family = "Roboto Condensed") +
  scale_fill_manual(values = c("#0074D9", "#5bc3f5", "#FF4136")) +
  scale_colour_manual(values = c("#0074D9", "#5bc3f5", "#FF4136")) +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "none") 

fig4 <- data.frame(placering_x = c(0, 1), placering_y = c(0.8, 0), tekst = c(str_wrap("Analysen bygger på et vægtet gennemsnit af meningsmålingerne fra JP.dk/snittet og Politologi.dk. Figurerne til venstre viser opbakningen til hhv. rød blok, blå blok og Moderaterne (med 95% troværdighedsintervaller). Figuren ovenfor viser det estimerede antal mandater blokkene ville få, hvis der var valg i dag. Da der især kan være udfordringer med at vurdere opbakningen til nye partier, herunder Moderaterne, anbefales det at man er ekstra opmærksom på den statistiske usikkerhed og holder sig bevidst omkring eventuelle fejlkilder.", width = 50),
                                                                             "")) |> 
  ggplot(aes(x = placering_x, y = placering_y, label = tekst)) +
  geom_label(label.size	= 0, size = 2.5, hjust = 0, colour = "gray40") +geom_label(label.size	= 0, size = 2.5, hjust = 0, colour = "gray40") +
  labs(title = "Det med småt") +
  theme_void(base_family = "Roboto Condensed")  +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", colour = "gray60"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "none") +
  ylim(0.4, 1.15)

png('blok_twitter.png', width = 8, height = 6, units = "in", res = 250)
grid.arrange(fig1, fig3, fig2, fig4, nrow = 2, heights = c(0.6, 0.4), widths = c(0.65, 0.35))
dev.off()

