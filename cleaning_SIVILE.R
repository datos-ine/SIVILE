### Limpieza dataset SIVILE 2024
### Autor: Tamara Ricardo
### Fecha modificación:
# 2025-10-17 13:18:15

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  epikit,
  skimr,
  geoAr,
  labelled,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Códigos provincias
prov <- show_arg_codes() |>
  filter(between(codprov, "01", "24"))

## Reportes SIVILE 2024
datos_raw <- read_delim(
  "raw/export_Sistema_de_Vigilancia_de_Lesiones_SIVILE_Completo_2024.txt"
)


## Explorar datos ----
names(datos_raw)

# Explorar NAs variables obligatorias
n_missing(datos_raw$DG1_CODIGO_INSTITUCION_ID) # NA = 0

n_missing(datos_raw$DL1_SEXO) # NA = 1

n_missing(datos_raw$DL3_EDAD) # NA = 7

n_missing(datos_raw$DE1_FECHA_LESION) # NA = 1

n_missing(datos_raw$CL1_MECANISMO_ID) # NA = 0

n_missing(datos_raw$CL14_INTENCIONALIDAD_ID) # NA = 2

n_missing(datos_raw$REM10_EGRESO_ID) # NA = 122


# Explorar datos desconocidos/mal cargados
tabyl(datos_raw, DL1_SEXO) # desconocido = 2

tabyl(datos_raw, CL1_MECANISMO_ID) # desconocido = 34 / mal cargado = 1

tabyl(datos_raw, CL14_INTENCIONALIDAD_ID) # desconocido = 113

tabyl(datos_raw, REM10_EGRESO_ID) # desconocido = 131

tabyl(datos_raw$DE6E_PROVINCIA_LESION_ID) # NA = 11000

tabyl(datos_raw, CL1_MECANISMO_ID, CL14_INTENCIONALIDAD_ID)


# Explorar niveles variables categóricas
tabyl(datos_raw, DL4_SITUACION_LABORAL_ID)

tabyl(datos_raw, DL5_NIVEL_INSTRUCCION_ID)

tabyl(datos_raw, CL7_LUGAR_LESION_ID)

tabyl(datos_raw, CL8_LUGAR_PUBLICO_ID) |> arrange(-valid_percent)

tabyl(datos_raw, CL9_LUGAR_VIVIENDA_ID) |> arrange(-valid_percent)

tabyl(datos_raw, CL1_MECANISMO_ID)

tabyl(datos_raw, CL14_INTENCIONALIDAD_ID)


# Limpiar datos ----------------------------------------------------------
datos_clean <- datos_raw |>
  # Estandarizar nombres de variables
  clean_names() |>

  # Quitar prefijos
  rename_with(.fn = ~ str_remove(.x, "^[^_]*_")) |>

  # Seleccionar variables de interés
  select(
    ucl_id = codigo_institucion_id,
    prov_ucl = provincia_origen,
    caso_id = numero_caso,
    sexo,
    edad,
    sit_laboral_id = situacion_laboral_id,
    nivel_instruccion_id,
    prov_resid = provincia_residencia_id,
    fecha_lesion,
    prov_lesion = provincia_lesion_id,
    lugar_lesion_id,
    lugar_publico_id,
    lugar_vivienda_id,
    evento_multiple,
    mecanismo_id,
    cond_lesionado = condicion_lesionado_id,
    modo_transporte = modo_transporte_id,
    contraparte = contraparte_id,
    cinturon_seguridad:asiento_ninos,
    intencionalidad_id,
    rel_agresor = relacion_victima_agresor_id,
    sexo_agresor,
    contexto_agresion = contexto_agresion_id,
    intento_previo,
    evid_alcohol = evidencia_alcohol_id,
    evid_drogas = evidencia_drogas_id,
    evid_med_psiq = evidencia_med_psi_id,
    spg = score_practico_gravedad_id,
    egreso = egreso_id,
    herida_excoriacion:naturaleza_otro,
    mecanismo_otro,
    observaciones
  ) |>

  # Filtrar registros cargados por error
  filter(!grepl("CARGADA POR ERROR", observaciones)) |>

  # Filtrar códigos UCL inválidos
  filter(ucl_id != 70140562334118) |>

  # Filtrar NAs en edad, fecha de lesión y egreso
  drop_na(edad, fecha_lesion, egreso) |>

  # Código de UCL como factor
  mutate(ucl_id = factor(ucl_id)) |>

  # Filtrar NA/desconocido en sexo
  filter(between(sexo, "F", "M")) |>

  # Filtrar datos desconocidos en mecanismo e intencionalidad
  filter(!(mecanismo_id == 99 & intencionalidad_id == 99)) |>

  # Cambiar etiquetas de sexo
  mutate(sexo = fct_relabel(sexo, ~ c("Femenino", "Masculino"))) |>

  # Categorizar edad
  mutate(
    grupo_edad = age_categories(
      edad,
      breakers = c(0, 2, 5, 10, 15, 25, 45, 65)
    ),
    .after = edad
  ) |>

  # Recategorizar situación laboral
  mutate(
    sit_laboral = case_when(
      between(sit_laboral_id, 1, 3) ~ "Trabajador/a",
      sit_laboral_id == 4 ~ "Desempleado/a",
      sit_laboral_id == 5 ~ "Estudiante",
      sit_laboral_id == 6 ~ "Tareas del hogar",
      sit_laboral_id == 6 ~ "Jubilado/a o pensionado/a",
      .default = "Otro/desconocido"
    ),
    .after = sit_laboral_id
  ) |>

  # Recategorizar nivel de instrucción
  mutate(
    nivel_instruccion = case_when(
      between(nivel_instruccion_id, 1, 2) ~ "Ninguno/primario incompleto",
      between(nivel_instruccion_id, 3, 4) ~ "Primario completo",
      between(nivel_instruccion_id, 5, 6) ~ "Secundario completo",
      nivel_instruccion_id == 7 ~ "Terciario/universitario completo",
      .default = "Desconocido"
    ),
    .after = nivel_instruccion_id
  ) |>

  # Recategorizar provincia de residencia, lesión y UCL
  mutate(across(
    .cols = contains("prov_"),
    .fns = ~ case_when(
      .x == 1 ~ "CABA",
      .x == 2 ~ "Buenos Aires",
      .x == 3 ~ "Catamarca",
      .x == 4 ~ "Chaco",
      .x == 5 ~ "Chubut",
      .x == 6 ~ "Córdoba",
      .x == 7 ~ "Corrientes",
      .x == 8 ~ "Entre Ríos",
      .x == 9 ~ "Formosa",
      .x == 10 ~ "Jujuy",
      .x == 11 ~ "La Pampa",
      .x == 12 ~ "La Rioja",
      .x == 13 ~ "Mendoza",
      .x == 14 ~ "Misiones",
      .x == 15 ~ "Neuquén",
      .x == 16 ~ "Río Negro",
      .x == 17 ~ "Salta",
      .x == 18 ~ "San Juan",
      .x == 19 ~ "San Luis",
      .x == 20 ~ "Santa Cruz",
      .x == 21 ~ "Santa Fe",
      .x == 22 ~ "Santiago del Estero",
      .x == 23 ~ "Tierra del Fuego",
      .x == 24 ~ "Tucumán"
    )
  )) |>

  # Si provincia de lesión es NA, asignar provincia de UCL
  mutate(prov_lesion = coalesce(prov_lesion, prov_ucl)) |>

  # Crear ID UCL por provincia
  mutate(
    id_prov_ucl = str_c(
      prov$codprov_iso[match(prov_ucl, prov$name_iso)],
      str_pad(dense_rank(ucl_id), width = 3, pad = "0"),
      sep = "-"
    ),
    .by = prov_ucl,
    .after = prov_ucl
  ) |>

  # Cambiar fecha de lesión a formato fecha
  mutate(fecha_lesion = dmy(fecha_lesion)) |>

  # Crear variables para mes y semana epidemiológica de lesión
  mutate(
    mes_anio_lesion = format(fecha_lesion, "%Y-%m"),
    semana_epi_lesion = epiweek(fecha_lesion),
    .after = fecha_lesion
  ) |>

  # Recategorizar lugar de lesión
  mutate(
    lugar_lesion = case_when(
      lugar_lesion_id == 1 ~ "Lugar público",
      lugar_lesion_id == 2 ~ "Vivienda",
      lugar_lesion_id == 3 ~ "Institución",
      .default = "Otro/desconocido"
    ),
    .after = lugar_lesion_id
  ) |>

  # Crear variable lugar lesión combinado
  mutate(
    lugar_lesion_cat = case_when(
      between(lugar_publico_id, 1, 2) ~ "Calle/avenida",
      between(lugar_publico_id, 3, 4) ~ "Autopista/ruta",
      lugar_publico_id == 5 ~ "Vereda",
      lugar_publico_id == 6 ~ "Parque/plaza",
      lugar_publico_id == 11 ~ "Área de deporte",
      lugar_vivienda_id == 1 ~ "Patio/jardín",
      lugar_vivienda_id == 2 ~ "Cocina",
      lugar_vivienda_id == 4 ~ "Baño",
      .default = lugar_lesion
    ),
    .after = lugar_vivienda_id
  ) |>

  # Recategorizar mecanismo de lesión
  mutate(
    mecanismo = case_when(
      # Transporte
      mecanismo_id == 1 | str_detect(mecanismo_otro, "(AUTO)") ~ "Transporte",
      # Envenenamiento o intoxicación
      mecanismo_id == 2 |
        str_detect(mecanismo_otro, "ALA|ARA|CAU|clo|fos|hum|NAF|VIVO") ~
        "Envenenamiento/intoxicación",
      # Golpe
      mecanismo_id == 3 |
        str_detect(mecanismo_otro, "CAB|DEP|TORO") |
        str_detect(observaciones, "BURRO") ~
        "Golpe",
      # Caídas
      mecanismo_id == 4 ~ "Caída",
      mecanismo_id == 5 ~ "Caída de un nivel a otro",
      # Objeto cortopunzante
      mecanismo_id == 6 | str_detect(mecanismo_otro, "BLA|AMO") ~
        "Objeto cortopunzante",
      # Arma de fuego
      mecanismo_id == 7 ~ "Arma de fuego",
      # Quemaduras
      between(mecanismo_id, 8, 11) |
        str_detect(mecanismo_otro, "fue|[Qq][Uu][Ee][Mm]|ULT") ~
        "Quemadura",
      # Electricidad
      mecanismo_id == 12 ~ "Electricidad",
      # Cuerpo extraño
      mecanismo_id == 13 ~ "Cuerpo extraño en ojo o cavidad",
      # Asfixia
      between(mecanismo_id, 14, 16) ~ "Asfixia",
      # Contacto con animal/planta
      mecanismo_id == 17 ~ "Contacto animal/planta",
      # Mordedura de perro
      mecanismo_id == 18 ~ "Mordedura de perro",
      # Aplastado o atrapado
      mecanismo_id == 20 ~ "Atrapado/aplastado/apretado",
      # Agresión sexual
      mecanismo_id == 21 ~ "Agresión sexual",
      .default = "Otro/desconocido"
    ),
    .after = mecanismo_id
  ) |>

  # Recategorizar condición lesionado por transporte
  mutate(
    cond_lesionado = case_when(
      cond_lesionado == 1 ~ "Peatón",
      cond_lesionado == 2 ~ "Pasajero",
      cond_lesionado == 3 ~ "Conductor",
      between(cond_lesionado, 4, 99) ~ "Otro/desconocido"
    )
  ) |>

  # Recategorizar y validar modo de transporte
  mutate(
    modo_transporte = case_when(
      cond_lesionado == "Peatón" ~ NA,
      modo_transporte == 1 ~ "Automóvil",
      modo_transporte == 2 ~ "Camioneta/furgoneta",
      modo_transporte == 3 ~ "Ómnibus",
      modo_transporte == 4 ~ "Camión",
      modo_transporte == 5 ~ "Bicicleta",
      modo_transporte == 6 ~ "Moto/ciclomotor",
      modo_transporte == 7 ~ "Tren",
      modo_transporte == 8 ~ "Tracción animal",
      between(modo_transporte, 9, 99) ~ "Otro/desconocido"
    )
  ) |>

  # Recategorizar contraparte
  mutate(
    contraparte = case_when(
      contraparte == 1 ~ "Peatón",
      contraparte == 2 ~ "Animal",
      contraparte == 3 ~ "Automóvil",
      contraparte == 4 ~ "Camioneta/furgoneta",
      contraparte == 5 ~ "Ómnibus",
      contraparte == 6 ~ "Camión",
      contraparte == 7 ~ "Bicicleta",
      contraparte == 8 ~ "Moto/ciclomotor",
      contraparte == 9 ~ "Tren",
      contraparte == 10 ~ "Tracción animal",
      contraparte == 11 ~ "Objeto fijo",
      contraparte == 12 ~ "Evento sin colisión",
      between(contraparte, 13, 99) ~ "Otro/desconocido"
    )
  ) |>

  # Recategorizar y validar uso de cinturón de seguridad y asiento para niños
  mutate(across(
    .cols = c(cinturon_seguridad, asiento_ninos),
    .fns = ~ case_when(
      !str_detect(modo_transporte, "Aut|Cam") ~ NA,
      .x == "S" ~ "Sí",
      .x == "N" ~ "No",
      .x == "NS" ~ "Desconocido",
      .default = NA
    )
  )) |>

  # Recategorizar y validar uso de casco y chaleco reflectante
  mutate(across(
    .cols = c(casco_transporte, chaleco_reflectante),
    .fns = ~ case_when(
      !str_detect(modo_transporte, "Bic|Mot") ~ NA,
      .x == "S" ~ "Sí",
      .x == "N" ~ "No",
      .x == "NS" ~ "Desconocido",
      .default = NA
    )
  )) |>

  # Recategorizar evento múltiple
  mutate(evento_multiple = if_else(evento_multiple == "NO", "No", "Sí")) |>

  # Recategorizar intencionalidad
  mutate(
    intencionalidad = case_when(
      intencionalidad_id == 1 ~ "Interpersonal",
      intencionalidad_id == 2 ~ "Autoinfligida",
      intencionalidad_id == 3 ~ "No intencional",
      intencionalidad_id == 4 ~ "Intervención legal",
      intencionalidad_id == 5 &
        str_detect(observaciones, "GATO|MORD|MUER|PERROS|VIV") ~
        "No intencional",
      .default = "Otro/desconocido"
    ),
    .after = intencionalidad_id
  ) |>

  # Recategorizar relación con agresor
  mutate(
    rel_agresor = fct_relabel(
      factor(rel_agresor),
      .fun = ~ c(
        "Pareja/ex pareja",
        "Padres/padrastros",
        "Familiar",
        "Amigos/conocidos",
        "Extraño",
        rep("Otro/desconocido", 2)
      )
    )
  ) |>

  # Recategorizar sexo agresor
  mutate(
    sexo_agresor = fct_relabel(
      sexo_agresor,
      .fun = ~ c(
        "Femenino",
        "Masculino",
        "Múltiples agresores",
        "Desconocido"
      )
    )
  ) |>

  # Recategorizar contexto de la agresión
  mutate(
    contexto_agresion = fct_relabel(
      factor(contexto_agresion),
      .fun = ~ c(
        "Violencia doméstica/intrafamiliar",
        "Robo/otros crímenes",
        "Riñas/peleas",
        rep("Otro/desconocido", 2)
      )
    )
  ) |>

  # Recategorizar intento previo
  mutate(
    intento_previo = fct_relabel(intento_previo, ~ c("No", "Desconocido", "Sí"))
  ) |>

  # Recategorizar evidencia de alcohol, drogas y medicación
  mutate(across(
    .cols = contains("evid_"),
    .fns = ~ case_when(
      .x == 1 ~ "Sí",
      .x == 2 ~ "No",
      between(.x, 3, 4) ~ "Dudoso/no evaluado"
    )
  )) |>

  # Recategorizar SPG
  mutate(
    spg = fct_relabel(
      factor(spg),
      .fun = ~ c("Leve", "Moderado", "Grave")
    )
  ) |>

  # Recategorizar egreso
  mutate(
    egreso = fct_relabel(
      factor(egreso),
      .fun = ~ c(
        "Ambulatorio/alta",
        "Internación",
        "Internación área crítica",
        "Derivación",
        "Alta voluntaria/fuga",
        "Fallecimiento",
        "Desconocido"
      )
    )
  ) |>

  # Convertir columnas tipo de lesión a binarias
  mutate(across(
    .cols = herida_excoriacion:naturaleza_otro,
    .fns = ~ if_else(.x == "SI", 1, 0)
  )) |>

  # Si no hay ningún tipo de lesión, marcar naturaleza_otro = 1
  mutate(
    naturaleza_otro = if_else(
      !if_any(herida_excoriacion:naturaleza_otro, ~ .x == 1),
      1,
      naturaleza_otro
    )
  ) |>

  # Pasar naturaleza a formato long
  pivot_longer(cols = herida_excoriacion:naturaleza_otro) |>

  # Filtrar lesiones presentes
  filter(value == 1) |>

  # Agrupar datos por filas originales
  group_by(across(-c(name, value))) |>

  # Crear columnas para número y tipo de lesiones
  summarise(
    tipo_lesion = if_else(
      n() > 1,
      "Lesiones múltiples",
      first(name, na_rm = TRUE)
    ),
    .groups = "drop"
  ) |>

  # Modificar etiquetas tipo lesión
  mutate(
    tipo_lesion = case_when(
      tipo_lesion == "cuerpo_extrano" ~ "Cuerpo extraño",
      tipo_lesion == "esguince_torcedura" ~ "Esguince/torcedura",
      tipo_lesion == "herida_excoriacion" ~ "Herida/excoriación",
      tipo_lesion == "intoxicacion" ~ "Intoxicación",
      tipo_lesion == "quemadura_corrosion" ~ "Quemadura/corrosión",
      tipo_lesion == "tec" ~ "Traumatismo encéfalo craneano",
      tipo_lesion == "trauma_hematoma" ~ "Trauma/hematoma",
      tipo_lesion %in% c("fractura", "politraumatismo") ~
        str_to_title(tipo_lesion),
      tipo_lesion %in% c("naturaleza_otro", "lesion_medular") ~
        "Otro/desconocido",
      .default = tipo_lesion
    )
  ) |>

  # Descartar columnas innecesarias
  select(-c(caso_id, fecha_lesion, observaciones, mecanismo_otro))


# Asignar etiquetas variables y orden niveles ----------------------------
datos <- datos_clean |>
  # Convertir variables caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # Reordenar niveles grupo etario
  mutate(grupo_edad = fct_relevel(grupo_edad, "2-4", "5-9", after = 1)) |>

  # Reordenar niveles educación
  mutate(
    nivel_instruccion = fct_relevel(
      nivel_instruccion,
      "Desconocido",
      after = Inf
    )
  ) |>

  # Reordenar niveles SPG
  mutate(spg = fct_relevel(spg, "Grave", after = Inf)) |>

  # Si la variable tiene valor "Otro/desconocido", ponerlo al final
  mutate(across(
    .cols = where(is.factor),
    .fns = ~ if ("Otro/desconocido" %in% levels(.x)) {
      fct_relevel(.x, "Otro/desconocido", after = Inf)
    } else {
      .x
    }
  )) |>

  # Asignar etiquetas de variables
  set_variable_labels(
    .labels = list(
      id_prov_ucl = "UCL",
      prov_ucl = "Provincia UCL",
      sexo = "Sexo",
      grupo_edad = "Grupo etario",
      sit_laboral = "Situación laboral",
      nivel_instruccion = "Nivel de instrucción",
      prov_resid = "Provincia residencia",
      prov_lesion = "Provincia lesión",
      lugar_lesion_cat = "Lugar de la lesión",
      evento_multiple = "Evento múltiple",
      mecanismo = "Mecanismo",
      cond_lesionado = "Condición del lesionado",
      modo_transporte = "Modo de transporte",
      contraparte = "Contraparte",
      cinturon_seguridad = "Cinturón de seguridad",
      casco_transporte = "Uso de casco",
      chaleco_reflectante = "Uso de chaleco reflectante",
      asiento_ninos = "Uso de asiento para niños",
      intencionalidad = "Intencionalidad",
      rel_agresor = "Relación con agresor",
      sexo_agresor = "Sexo agresor",
      contexto_agresion = "Contexto de agresión",
      intento_previo = "Intento previo",
      evid_alcohol = "Evidencia de alcohol",
      evid_drogas = "Evidencia de drogas",
      evid_med_psiq = "Evidencia de medicación psiquiátrica",
      tipo_lesion = "Tipo de lesión",
      spg = "SPG",
      egreso = "Egreso"
    )
  )


# Guardar datos limpios ---------------------------------------------------
export(datos, file = "clean/clean_SIVILE_2024.rds", na = NA)


## Crear objeto con filas y UCL
datos_raw |>
  summarise(
    n = n(),
    n_ucl = factor(DG1_CODIGO_INSTITUCION_ID) |> nlevels() - 1
  ) |>

  export(file = "clean/resumen.csv")

## Limpiar entorno
rm(list = ls())
