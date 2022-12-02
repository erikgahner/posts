# R script for "Tabel med meningsmåling i R"
# URL: https://erikgahner.dk/2022/tabel-med-meningsmaling-i-r/

library("tidyverse")
library("gt")

maaned <- Vectorize(function(n) c("januar", "februar", "marts", 
                                  "april", "maj", "juni", "juli", 
                                  "august", "september", "oktober",
                                  "november", "december")[n])

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

polls <- read_csv("polls.csv",
                  col_types = cols(
                    party_p = col_double(),
                    party_e = col_double(),
                    party_g = col_double()
                  ))

aktuel <- polls |> 
  mutate(n = ifelse(is.na(n), 1000, n)) |> 
  #filter(id == max(id)) |> 
  slice(1647)

poll_df <- aktuel |> 
   pivot_longer(starts_with("party")) |> 
   select(name, value) |> 
   drop_na(value) |> 
   mutate(parti = case_when(
      name == "party_a" ~ "Socialdemokratiet",
      name == "party_v" ~ "Venstre",
      name == "party_o" ~ "Dansk Folkeparti",
      name == "party_f" ~ "SF",
      name == "party_g" ~ "Grøn Alliance",
      name == "party_c" ~ "Konservative",
      name == "party_b" ~ "Radikale Venstre",
      name == "party_oe" ~ "Enhedslisten",
      name == "party_i" ~ "Liberal Alliance",
      name == "party_d" ~ "Nye Borgerlige",
      name == "party_aa" ~ "Alternativet",
      name == "party_k" ~ "Kristendemokraterne",
      name == "party_e" ~ "Borgerlisten",
      name == "party_p" ~ "Stram Kurs",
      name == "party_q" ~ "Frie Grønne",
      name == "party_m" ~ "Moderaterne",
      name == "party_ae" ~ "Danmarksdemokraterne",
      TRUE ~ "Andre"
   )) |> 
   mutate(image = case_when(
      parti == "Venstre" ~ "http://politologi.dk/img/partilogoer/ico/v_24.png", 
      parti == "Socialdemokratiet" ~ "http://politologi.dk/img/partilogoer/ico/a_24.png", 
      parti == "Dansk Folkeparti" ~ "http://politologi.dk/img/partilogoer/ico/o_24.png", 
      parti == "SF" ~ "http://politologi.dk/img/partilogoer/ico/f_24.png", 
      parti == "Grøn Alliance" ~ "http://politologi.dk/img/partilogoer/ico/g_24.png", 
      parti == "Konservative" ~ "http://politologi.dk/img/partilogoer/ico/c_24.png", 
      parti == "Radikale Venstre" ~ "http://politologi.dk/img/partilogoer/ico/b_24.png", 
      parti == "Enhedslisten" ~ "http://politologi.dk/img/partilogoer/ico/oe_24.png", 
      parti == "Liberal Alliance" ~ "http://politologi.dk/img/partilogoer/ico/i_24.png", 
      parti == "Nye Borgerlige" ~ "http://politologi.dk/img/partilogoer/ico/d_24.png", 
      parti == "Alternativet" ~ "http://politologi.dk/img/partilogoer/ico/aa_24.png", 
      parti == "Kristendemokraterne" ~ "http://politologi.dk/img/partilogoer/ico/k_24.png", 
      parti == "Borgerlisten" ~ "http://politologi.dk/img/partilogoer/ico/e_24.png",
      parti == "Stram Kurs" ~ "http://politologi.dk/img/partilogoer/ico/p_24.png",
      parti == "Frie Grønne" ~ "http://politologi.dk/img/partilogoer/ico/q_24.png",
      parti == "Moderaterne" ~ "http://politologi.dk/img/partilogoer/ico/moderaterne_24.jpg",
      parti == "Danmarksdemokraterne" ~ "http://politologi.dk/img/partilogoer/ico/ae_24.png"
   )) |> 
   mutate(election2019 = case_when(
      parti == "Venstre" ~ 23.4, 
      parti == "Socialdemokratiet" ~ 25.9, 
      parti == "Dansk Folkeparti" ~ 8.7, 
      parti == "SF" ~ 7.7, 
      parti == "Grøn Alliance" ~ NA_real_, 
      parti == "Konservative" ~ 6.6, 
      parti == "Radikale Venstre" ~ 8.6, 
      parti == "Enhedslisten" ~ 6.9, 
      parti == "Liberal Alliance" ~ 2.3, 
      parti == "Nye Borgerlige" ~ 2.4, 
      parti == "Alternativet" ~ 3.0, 
      parti == "Kristendemokraterne" ~ 1.7, 
      parti == "Stram Kurs" ~ 1.8,
      parti == "Moderaterne" ~ NA_real_,
      parti == "Danmarksdemokraterne" ~ NA_real_
   )) |> 
   arrange(desc(value)) |> 
   transmute(image, Parti = parti, Opbakning = value, election2019) |> 
   mutate(Usikkerhed = round(1.96 * sqrt(( Opbakning * (100 - Opbakning)) / aktuel$n), 1)) |> 
   mutate(Forskel = Opbakning - election2019) |> 
   mutate(trend = case_when(
      Forskel > 0 ~ "↑",
      Forskel < 0 ~ "↓",
      TRUE ~ "–"
   ))


poll_table <- poll_df |> 
   select(image, Parti, Opbakning, Usikkerhed, election2019, Forskel, trend) |>  
   gt() |> 
   text_transform(
      locations = cells_body(columns = trend),
      fn = function(x){ glue::glue("<div><span style='font-weight:bold;font-variant:small-caps;font-size:20px'>{x}</div>") } ) |> 
   text_transform(
      locations = cells_body(columns = image),
      fn = function(x){ glue::glue("<img src='{x}' width = '16px'> ") } ) |> 
   text_transform(
      locations = cells_body(columns = Parti), 
      fn = function(x){ glue::glue("<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{x}</div>") }) |> 
   cols_label(image = "", 
              Parti = "",
              Opbakning = html(paste0("<div><span style='font-variant:small-caps;font-size:14px'>Opbakning</div>", glue::glue("<div><span style='color:gray;font-size:12px'>{aktuel$day}. {maaned(aktuel$month)}</div>"))),
              Usikkerhed = html(paste0("<div><span style='font-variant:small-caps;font-size:14px'>Usikkerhed</div>",
                                       "<div><span style='color:gray;font-size:12px'>95% KI</div>")),
              election2019 = html("<div><span style='font-variant:small-caps;font-size:14px'>FV '19</div><div><span style='color:gray;font-size:12px'>Resultat</div>"),
              Forskel = html("<div><span style='font-variant:small-caps;font-size:14px'>Forskel</div><div><span style='color:gray;font-size:12px'>%-point</div>"),
              trend = ""
              ) |> 
   tab_style(
      style = list(
         cell_text(color = "#FF4136")
      ),
      locations = cells_body(
         columns = trend,
         rows = Forskel < 0
      )
   ) |> 
   tab_style(
      style = list(
         cell_text(color = "#3D9970")
      ),
      locations = cells_body(
         columns = trend,
         rows = Forskel > 0
      )
   ) |> 
   sub_missing(missing_text = "–") |> 
   fmt_number(columns = c(Opbakning, election2019, Forskel),
              pattern = "{x}%",
              decimals = 1,
              dec_mark = ",",
              sep_mark = ".") |> 
   fmt_number(columns = c(Usikkerhed),
              pattern = "±{x}%",
              decimals = 1,
              dec_mark = ",",
              sep_mark = ".") |> 
   cols_align(
      align = "center",
      columns = c(election2019, Usikkerhed, Forskel, trend)
   ) |> 
   cols_align(
      align = "right",
      columns = c(Opbakning)
   ) |> 
   data_color(
      columns = c(Opbakning, Usikkerhed),
      colors = "gray96"
   ) 

poll_table

poll_table |> 
   as_raw_html()

glue::glue("<h4>Metode og kilde</h4>
            <b>Institut:</b> {aktuel$pollingfirm}<br>
            <b>Stikprøvestørrelse:</b> {aktuel$n}<br>
            <b>Dato:</b> {aktuel$day}. {maaned(aktuel$month)} {aktuel$year}<br>
            <b>Kilde:</b> <a href='{aktuel$source}'>{aktuel$pollingfirm}</a>")


