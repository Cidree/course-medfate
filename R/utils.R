
## Process pmayores to calculate stats by dclass 
process_pmayores <- function(data) {

  data |> 
  ## Remove NA
  filter(
    !is.na(Dn1), 
    !is.na(Dn2),
    Dn1 >= 75,
    Dn2 >= 75,
    Ht > 0
  ) |> 
  select(Estadillo, Especie, Dn1, Dn2, Ht) |> 
  ## Get diametric classes
  mutate(
    dn       = (Dn1 + Dn2) / 2 / 10,
    dclass = silv_diametric_class(dn)
  ) |>
  ## Calculate number of trees per plot and species
  summarise(
    # Ht  = mean(Ht, na.rm = TRUE),
    n   = n(),
    .by = c(Estadillo, Especie, dclass, Ht)
  ) |> 
  ## Calculate average height
  summarise(
    h_mean = weighted.mean(Ht, n),
    n   = sum(n),
    .by = c(Estadillo, Especie, dclass)
  ) |> 
  ## Calculate trees/ha
  ## Concentric plots are used
  mutate(
    n_ha = case_when(
      dclass < 12.5                  ~ n * 10000 / (pi * 5^2),
      dclass >= 12.5 & dclass < 22.5 ~ n * 10000 / (pi * 10^2),
      dclass >= 22.5 & dclass < 42.5 ~ n * 10000 / (pi * 15^2),
      dclass >= 42.5                 ~ n * 10000 / (pi * 25^2),
      .default = NA
    )
  ) |> 
  ## Dominant height by plot and species
  mutate(
    h0  = silv_dominant_height(dclass, h_mean, n_ha, which = "assman"),
    .by = c(Estadillo, Especie)
  ) |> 
  ## Basal area
  mutate(
    g_ha  = silv_basal_area(dclass, n_ha)
    # Especie = parse_number(Especie)
  ) |> 
  select(plot = Estadillo, species = Especie, dclass, n_parc = n,
          n_ha, everything())

}

process_regeneration <- function(data, codes) {

  ## Measured in a circular plot of 5meters radius
  data |> 
    mutate(
      # across(c(Estadillo, Especie, Densidad), parse_number),
      ntrees_ha = if_else(
        is.na(Densidad),
        NumPies * 10000 / (pi * 5^2),
        Densidad * 10000 / (pi * 5^2)
      ),
      h_m = Hm / 10
    ) |> 
    left_join(
      codes,
      by = join_by(Especie == IFNCODE)
    ) |> 
    select(
      species = IFNNAME, plot = Estadillo, 
      development = CatDes, h_m, ntrees_ha
    )

}

process_matorral <- function(data, codes) {

  data |> 
    # mutate(Especie = parse_number(Especie)) |> 
    left_join(
      codes,
      by = join_by(Especie == IFNCODE)
    ) |> 
    select(
      species = IFNNAME, plot = Estadillo, fcover = Fcc, h_m = Hm, agent = Agente
    ) |> 
    mutate(h_m = h_m / 10)


}
