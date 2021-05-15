library(tidyverse)

atu_covid19 <- read_csv(
  "datos/2021-05-15-lima-atu-covid19-paraderos.csv",
  col_types = cols(
    partipcod = col_character(),
    parnom = col_character(),
    disnom = col_character(),
    cornom = col_character(),
    nivel = col_double(),
    latitud = col_double(),
    longitud = col_double(),
    tipodet = col_character(),
    ts = col_datetime(format = ""),
    nivel_lbl = col_character()
  )
) %>%
  mutate_at (
    vars(partipcod, disnom, cornom, tipodet),
    factor
  ) %>%
  mutate(
    nivel_lbl = factor(
      nivel_lbl,
      levels = c("Moderado", "Alto", "Muy Alto", "Extremo"),
      ordered = TRUE
    )
  )


por_tipo <- atu_covid19 %>%
  group_by(tipodet) %>%
  tally() %>%
  ungroup() %>%
  mutate(
    tipodet_n = glue::glue("{tipodet}\nN: {n}")
  ) %>%
  select(-n)

por_tipo_nivel <- atu_covid19 %>%
  group_by(nivel_lbl, tipodet) %>%
  tally() %>%
  left_join(
    por_tipo,
    by = "tipodet"
  ) %>%
  ungroup() %>%
  group_by(tipodet_n) %>%
  mutate(
    pct = n / sum(n)
  )

p1 <- ggplot(por_tipo_nivel,
       aes(x = tipodet_n, y = pct, fill = nivel_lbl, group = tipodet_n)) +
  geom_col(color = "black", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", 100*pct)),
                position = position_stack(vjust = .5),
            size = 9) +
  scale_fill_brewer(palette = "Reds") +
  theme_void(base_size = 16) +
  theme(
    axis.text.x = element_text(),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(family = "Inconsolata", size = 16),
    plot.margin = unit(rep(.5, 4), "cm"),
    legend.position = "top"
  ) +
  labs(
    title = "Distribución de paraderos por tipo y nivel de riesgo COVID-19",
    subtitle = "Fuente: Mapa de riesgo COVID-19 de ATU - Lima Metropolitana",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-05-15",
    fill = ""
  )

ggsave(
  p1,
  filename = "pct-paraderos-tipos-riesgo-covid19.png",
  width = 9,
  height = 9
)

por_distrito <- atu_covid19 %>%
  group_by(disnom, nivel_lbl) %>%
  tally() %>%
  mutate(
    disnom = fct_rev(disnom)
  )


p2 <- ggplot(
  por_distrito,
  aes(y = disnom, x = n, fill = nivel_lbl)
) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(~nivel_lbl, ncol = 4) +
  ggdark::dark_theme_light(base_size = 16) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(color = "grey80"),
    plot.caption = element_text(family = "Inconsolata", size = 16),
    plot.margin = unit(rep(.5, 4), "cm")
  ) +
  labs(
    title = "Número de paraderos en cada Distrito por nivel de riesgo COVID-19",
    subtitle = "Fuente: Mapa de riesgo COVID-19 de ATU - Lima Metropolitana",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-05-15",
    x = "Número de paraderos",
    y = "",
    fill = ""
  )

ggsave(
  p2,
  filename = "pct-paraderos-distrito-riesgo-covid19.png",
  width = 14,
  height = 14
)

