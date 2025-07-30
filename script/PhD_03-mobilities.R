# Homogamy

cli::cli_h2(col_blue("Análisis de Movilidad (MOV)"))

MOV <- list()

# load("data/derived/02_data.RData")
# source(here::here("scripts/SNA_00_fx.R"), encoding = "UTF-8")

# Cargar datos --------------------------------------

  if(!exists("CARTO", inherits = F)){
    f <- fs::path("data-workflow", "final-data", "CARTO", ext = "rda")
    cli_alert_info(
      col_silver("Cargando cartografía (`CARTO`): {.path {fs::path_rel(f)}}")
    )

    load(file = f)
    rm(f)
  }

  if(!exists("AUX", inherits = F)){
    f <- fs::path("data-workflow", "final-data", "AUX-data", ext = "rda")

    cli_alert_info(
      col_silver("Cargando datos auxiliares (`AUX`): {.path {fs::path_rel(f)}}")
    )

    load(file = f)
    rm(f)
  }

  if(!exists("ENMODO", inherits = F)){
    f <- fs::path("data-workflow", "final-data", "ENMODO", ext = "rda")

    cli_alert_info(
      col_silver("Cargando datos de hogares (`ENMODO`): {.path {fs::path_rel(f)}}")
    )

    load(file = f)
    rm(f)
  }

  replic <- 1000
  conf   <- 0.95

# Análisis de movilidades --------------------------------------
### ╠ Descripción de viajes ---------------------------------

TH_aux <- CARTO$RADIO |>
  filter(!is.na(TH)) |>
  group_by(TH) |>
  summarise() |>
  mutate(sup = st_area(geom),
         sup = set_units(sup, value = "km2"),
         sup = drop_units(sup),
         sup_p = round(sup / sum(sup) * 100, 1) ) |>
  st_drop_geometry()

VIA_TOT <- db_mim |>
  group_by(TH = in_TH) |>
  summarise(N = sum(in_time, na.rm = T))

VIA_MOT <- db_mim |>
  group_by(TH = in_TH, v_MOT) |>
  summarise(N = sum(in_time, na.rm = T)) |>
  pivot_wider(id_cols = TH, names_from = v_MOT, values_from = "N", values_fill = 0) |>
  ungroup()

VIA <- TH_aux |>
  left_join(VIA_TOT, by = "TH") |>
  left_join(VIA_MOT, by = "TH") |>
  mutate(across(.cols = N:Recreación,
                .fns = ~round(.x/sum(.x) * 100, 1)),
         sup = round(sup, 1) )

rm(VIA_TOT, VIA_MOT, TH_aux)

TIE <- rbind(
  db_mim |> summarise(v_MOT = "N", Dur = round(weighted.mean(v_TVIA, w, na.rm = T), 1)),
  db_mim |> group_by(v_MOT) |> summarise(Dur = round(weighted.mean(v_TVIA, w, na.rm = T), 1))
) |>
  pivot_wider(names_from = v_MOT, values_from = "Dur")

MOV$VIA <- rbind(VIA, c(TH = NA, sup = NA,
                        sup_p = NA,
                        TIE) ) |>
  mutate(TH = fct_na_value_to_level(TH, "Tiempo promedio de viaje (minutos)"),
         Ratio = N/sup_p)

rm(VIA, TIE)

### ╠ Análisis de Segregación: Diferencia residencial-destino -----------------

# Residencial
## UA: Hogar (considera a inmóviles)
## Escala: Fracción
## Variable: Máximo nivel educativo del hogar

db_hog <- ENMODO$HOG |>
  left_join(CARTO$RADIO |> select(ID_RADIO, TH), by = "ID_RADIO") |>
  filter(!is.na(TH)) |>
  mutate(ID_FRAC = str_sub(ID_RADIO, start = 1, end = 7 ) ) |>
  group_by(ID_FRAC) |>
  summarise(PRI = sum(PONDERA_HOG * (EDU %in% c("Sin estudios", "Primario")), na.rm = T),
            SEC = sum(PONDERA_HOG * (EDU %in% c("Secundario")), na.rm = T),
            SUP = sum(PONDERA_HOG * (EDU %in% c("SNU", "Universitario", "Post-Universitario")), na.rm = T)) |>
  pivot_longer(cols = PRI:SUP)

# Destinos
## UA: Destinos
## Escala: Fracción
## Variable: Máximo nivel educativo del hogar

db_dest <- db_mim |>
  group_by(DES, MOT = v_MOT) |>
  summarise(PRI = sum(w_time * (p_EDU == "pEDU_Primario"), na.rm = T),
            SEC = sum(w_time * (p_EDU == "pEDU_Secundario"), na.rm = T),
            SUP = sum(w_time * (p_EDU == "pEDU_Superior"), na.rm = T))  |>
  ungroup() |>
  pivot_longer(cols = PRI:SUP) |>
  mutate(value = round(value / 60, 1)) # Corrección para achicar números

# Comparación de segregación
MOV$SEG <- mutual_difference(db_dest |> rename(ID_FRAC = DES), db_hog,
                             unit = "ID_FRAC", group = "name", weight = "value",
                             se = T, CI = conf, n_bootstrap = replic) |>
  mutate(est = round(est, 3),
         CI2 = map(CI, ~paste0(format(round(.x, 3), nsmall = 3), collapse = ";")),
         CI2 = paste0("(", CI2, ")")) |>
  select(stat, est, CI2)

rm(db_dest, db_hog)

### ╚ Análisis de red Bipartita (MCA) -------------------------------------------

var_act <- AUX$VAR_mca$Variable[AUX$VAR_mca$MCA == "x" & !is.na(AUX$VAR_mca$MCA)]
var_sup <- AUX$VAR_mca$Variable[AUX$VAR_mca$MCA == "Sup." & !is.na(AUX$VAR_mca$MCA)]
var_quanti <- AUX$VAR_mca$Variable[AUX$VAR_mca$MCA == "Cuanti" & !is.na(AUX$VAR_mca$MCA)]

res_mca <- db_mim |>
  select(all_of(c(var_act, var_sup, var_quanti))) |>
  MCA(graph = F, row.w = db_mim$w_time,
      quali.sup = var_sup,#[3:15],
      quanti.sup = var_quanti) |>
  eig_corrected()

MOV$res_mca <- res_mca

rm(var_act, var_sup, var_quanti)
rm(res_mca)

### TODO Análisis de secuencia de movilidades -------------------

# VIA <- ENMODO$VIA |>
#   filter(!is.na(DUR_VIA) & DUR_VIA > 0)

# a <- VIA[rep(1:nrow(VIA), times = VIA$DUR_VIA), ] |>
#   group_by(id_hogar, id_persona, INI, FIN) |>
#   mutate(N = 1:n(),
#          HORA_RETRO = INI + N - 1) |>
#   ungroup()

### Guardar datos --------------------------------------

# save(MOV, AUX, file = "data/derived/03_data.RData")

# Figuras ------------------------------------

# load("data/derived/02_data.RData")
# load("data/derived/03_data.RData")

# Descripción de viajes ---------------------------------------------------

### ╠ FIG 5.1: Mapeo de zonas más visitadas --------------------------------

  p1 <- CARTO$FRAC |>
    left_join(AUX$prop_in, by = c("ID_FRAC" = "DES")) |>
    mutate(in_time = ifelse(in_time_lab %in% c("inT_Q5", "inT_Q4", "inT_Q3"),
                            in_time, NA) ) |>
    ggplot() +
    geom_sf(aes(fill = in_time), color = NA, alpha = 0.8) +
    geom_sf(data = CARTO$DPTO, fill = NA, color = "grey70" ) +
    scale_fill_viridis_c(paste0("Tiempo total\nen el destino\n(miles de hs.)"),
                         direction = 1, option = "plasma",
                         na.value = "#08014e", limits = c(25, 800),
                         breaks = c(25, 50, 100, 200, 400, 800),
                         labels = c("25 (o menos)", 50, 100, 200, 400, 800),
                         trans = 'log10') +
    coord_sf(xlim = AUX$AMGBA_lim[c(1,3)],
             ylim = AUX$AMGBA_lim[c(2,4)]) +
    theme_void()

  ggsave(filename = "figures/Destino-tiempo-total.png", p1,
         width = 17, height = 15, units = "cm", dpi = 300)

  rm(p1)

### ╠ FIG 5.2: Destino de viaje según nivel educativo ---------------------------

prom <- db_mim |>
  summarise(across(ends_with("_t"),
                   ~weighted.mean(.x, w = in_time ) ) ) |>
  pivot_longer(cols = everything(), names_to = "var", values_to = "p")

db_edu <- CARTO$FRAC["ID_FRAC"] |>
  right_join(AUX$prop_in, by = c("ID_FRAC" = "DES" ) ) |>
  filter(n >= 4) |>
  select(ID_FRAC, in_PRI_t:in_SUP_t, in_ls)

p2 <- map2(.x = prom$var, .y = prom$p, ggmapa) %>%
  ggarrange(plotlist = ., ncol = 3,
            labels = c("Primario", "Secundario", "Superior") )

ggsave(filename = "figures/Destino-Nivel-educativo.png", p2,
       width = 35, height = 15, units = "cm", dpi = 300)

rm(p2, prom)

### ╠ FIG 5.3: Mapeo de segregación local de destino ---------------------------

p3 <- db_edu |>
  ggplot() +
  geom_sf(aes(fill =  in_ls), colour = NA) +
  geom_sf(data = CARTO$DPTO, fill = NA, color = "grey10" ) +
  scale_fill_gradient2("Segregación local\nen destino",
                       low = "#dedede", mid = "#dad9be", high = "#aa0808",
                       midpoint = median(db_edu$in_ls)
  ) +
  coord_sf(xlim = AUX$AMGBA_lim[c(1,3)],
           ylim = AUX$AMGBA_lim[c(2,4)]) +
  theme_void()

ggsave(filename = "figures/Destino-Segregacion-local.png", p3,
       width = 17, height = 15, units = "cm", dpi = 300)

rm(p3, db_edu)

### ╚ FIG 5.4: Relación entre TH destino y origen por nivel educativo -----------

p4 <- db_mim |>
  group_by(in_TH, p_TH, p_EDU) |>
  summarise(N = sum(in_time, na.rm = T)) |>
  group_by(p_TH, p_EDU) |>
  mutate(p = round(N/sum(N) * 100, 1)) |>
  ggplot(aes(x = in_TH, y = p_TH, fill = p, label = p)) +
  geom_tile() +
  geom_text(col = "black") +
  facet_wrap(~p_EDU) +
  theme_minimal() +
  scale_fill_gradient2(low = "white", mid = "#FFBE33", high = "tomato3",
                       midpoint = 30, na.value = "white")

rm(p4)

# MCA -----------------------------------------------------

### ╠ Carga de datos MCA -------------------------------------------------------------

  res_mca <- MOV$res_mca
  coor <- rbind(res_mca$var$coord, res_mca$quali.sup$coord)
  aux_coor <- coor |>
    as_tibble(rownames = "Categ", .name_repair = "universal") |>
    full_join(AUX$VAR_cat, by = "Categ") |>
    mutate(grupo = str_wrap(grupo, 20),
           Categ = str_replace_all(Categ, "_", " "),
           Categ = str_wrap(Categ, 20))

### ╠ FIG 5.5: Proyección de variables en MCA ---------------------------------

p5 <- fviz_mca_var(res_mca,  geom.var = "point", col.quali.sup = NA, alpha.var = 0, title = "") +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, color = grupo, label = Categ),
                  data = aux_coor |> filter(sel == 1)) +
  scale_color_d3() +
  labs(color= "Grupo de variables") +
  theme(legend.position = "bottom")

ggsave(filename = "figures/MCA-Variables-Categ.png", p5,
       width = 30, height = 20, units = "cm", dpi = 300)

rm(p5)

### ╠ FIG 5.6: Proyección de unidades por nivel educativo en MCA ----------------

aux_lab <- c("Primario", "Secundario", "Superior") |>
  set_names(levels(db_mim$p_EDU))

p6 <- fviz_mca_ind(res_mca, habillage = "p_EDU", title = "",
                   alpha.ind = 0.02, geom.ind = "point") +
  stat_density_2d(geom = "polygon", contour = TRUE,
                  alpha = 0.25, colour = "grey85", bins = 8,
                  aes(fill = p_EDU, group = p_EDU )) +
  geom_text(data = aux_coor |> filter(ilust == 1), size =3,
            aes(x = Dim.1, Dim.2, label = Categ)) +
  facet_wrap(~p_EDU, ncol = 3, labeller = as_labeller(aux_lab)) +
  xlim(c(-1.2, 1.5)) + ylim(c(-1, 1.9)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "grey85", colour = "black"))

ggsave(filename = "figures/MCA-Nivel-educativo.png", p6,
       width = 30, height = 15, units = "cm", dpi = 300)

rm(p6, aux_lab, aux_coor)

### ╠ Figura(Anexo): Porcentaje de varianza explicada MCA -----------------------------

p7a <- fviz_eig(res_mca,
                barcolor = "tomato3", barfill = "tomato3",
                linecolor = "grey30",
                addlabels = T, hjust = 0.5,
                main = "Varianza explicada (Ajuste: Greenacre)",
                xlab = "Dimensión",
                ylab = "Porcentaje de varianza explicada [%]")

p7b <- res_mca$eig |>
  as.data.frame() |>
  rename(Greenacre = "cumulative percentage of variance") |>
  mutate(Benzécri = eigenvalue / sum(eigenvalue) * 100,
         Benzécri = cumsum(Benzécri)) |>
  slice_head(n = 10) |>
  mutate(Indicator = res_mca$eig_original[1:10, 3],
         Dim = 1:10) |>
  pivot_longer(cols = c("Greenacre", "Benzécri", "Indicator")) |>
  # mutate(factor(name, levels = c("Greenacre", "Benzécri", "Indicator"), ordered = T)) |>
  ggplot(aes(x = Dim, y = value, color = name, group = name))+
  geom_point(shape = 4) +
  geom_line(aes(linetype = !(name == "Greenacre") )) +
  scale_x_continuous("Dimensión", breaks = 0:10 ) +
  scale_y_continuous("Porcentaje de varianza acumulada [%]", limits = c(0, 100)) +
  labs(title = "Varianza acumulada", color = "Método") +
  scale_color_igv() +
  guides(linetype = "none") +
  theme_minimal()

ggsave(filename = here::here("figures/anexo_B_eig.png"),
       plot = ggarrange(p7a, p7b),
       width = 30, height = 15, units = "cm")

rm(p7a, p7b)

### ╠ Figura(Anexo): Correlación entre variables y dimensiones de análisis ------------

var_exp <- paste0("Dim ", 1:4, " (",round(res_mca$eig[1:4, 2], 1), "%)")

coor_var <- list(Activas = res_mca$var$eta2, Suplementarias = res_mca$quali.sup$eta2) |>
  map(~as_tibble(.x, rownames = "Var", .name_repair = "universal")) |>
  bind_rows(.id = "Rol")

p8a <- coor_var |>
  filter(Dim.1 > mean(Dim.1) | Dim.2 > mean(Dim.2) ) |>
  ggplot(aes(x = Dim.1, y = Dim.2, label = Var, color = Rol)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_text_repel() +
  scale_color_uchicago() +
  coord_fixed() +
  labs(x = var_exp[1], y = var_exp[2]) +
  lims(x = c(0,1), y = c(0,1)) +
  theme_minimal()

p8b <- coor_var |>
  filter(Dim.3 > mean(Dim.3) | Dim.4 > mean(Dim.4) ) |>
  ggplot(aes(x = Dim.3, y = Dim.4, label = Var, color = Rol)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point() +
  geom_text_repel() +
  scale_color_uchicago() +
  coord_fixed() +
  labs(x = var_exp[3], y = var_exp[4]) +
  lims(x = c(0,1), y = c(0,1)) +
  theme_minimal()

ggarrange(p8a, p8b, common.legend = T, legend = "bottom",
          labels = paste("Dimensiones", c("1 y 2", "3 y 4")) ) |>
  ggsave(file = "figures/anexo_B_dist-var.png",
         width = 30, height = 15, units = "cm", dpi = 300)

rm(p8a, p8b, coor_var, var_exp)

### ╚ Figura(Anexo): Descripción de variables ordinales MCA -------------------------------------

VAR <- AUX$VAR_cat |>
  filter(Ordenada == 1) |>
  mutate(tit = paste(Nivel, var_lab, sep = ": ") )

coor_ord <- coor[VAR$Categ, ] |>
  as_tibble(rownames = "Variable", .name_repair = "universal") |>
  mutate(lab = str_split_i(Variable, pattern = "_", -1))

p9 <- unique(VAR$Variable) |>
  map(~fviz_variables(.x, MCA_obj = res_mca, V = VAR, d = coor_ord) ) |>
  set_names(unique(VAR$Variable))

ggarrange(plotlist = p9[c(5:7, 11:13)] ) |>
  ggsave(file = "figures/anexo_B_var1.png",
         width = 40, height = 20, units = "cm", dpi = 300)


ggarrange(plotlist = p9[c(9:10, 14:17)] ) |>
  ggsave(file = "figures/anexo_B_var2.png",
         width = 40, height = 20, units = "cm", dpi = 300)

# ggarrange(plotlist = p9[c(8, 18)] ) |>
#   ggsave(file = "figures/anexo_B_var2.png",
#          width = 20, height = 10, units = "cm", dpi = 300)
ggarrange(plotlist = p9[c(8, 4, 1, 18, 3, 2)] ) |>
  ggsave(file = "figures/anexo_B_var3.png",
         width = 40, height = 20, units = "cm", dpi = 300)

rm(VAR, coor, coor_ord, p9)
rm(res_mca)

