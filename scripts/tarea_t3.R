# Renato Cortez Zapata
# Octubre 2026
# Responde a la pregunta: ¿Se asocia la edad con la probabilidad de tener un empleo
# formal, y se mantiene esa asociación al comparar personas del mismo sexo y región?

library(dplyr)
library(broom)
library(modelsummary)

## Parte A — CASEN

### A1.

casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m1 <- lm(ingreso ~ educ, data = casen)
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)

### A2.

arrange(filter(tidy(m2, conf.int = TRUE), term != "(Intercept)")|>
         mutate(estimate      = round(estimate),
                significativo = p.value < 0.05), p.value)

### A3.

m3 <- lm(ingreso ~ educ + experiencia + I(experiencia^2)+ genero, data = casen)

modelos <- list(simple = m1, controles = m2, mio = m3)
lapply(modelos, glance) |> bind_rows(.id = "modelo")

# Viendo el R², aparentemente mejora la capacidad explicativa del modelo al añadir
# más variables, pero con el ajustado se ve que la capacidad explicativa está
# inflada por la cantidad de variables y con el modelo simple se explica mejor.

### A4.

bind_rows(simple    = tidy(modelos$simple),
          controles = tidy(modelos$controles),
          mio    = tidy(modelos$mio), .id = "modelo") |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

# Para que un control mueva un coeficiente, tiene que estar relacionado con el ingreso
# y la educación. El coeficiente cambia porque estoy usando experiencia al cuadrado 
# y la fórmula de experiencia contiene educación.

### A5.

# 1

augment(m2) |>
  select(ingreso, educ, genero, .fitted, .resid) |>
  mutate(across(c(.fitted, .resid), round)) |>
  arrange(desc(abs(.resid))) |>
  head(3)

# Tienen en común que todos son hombres

# 2

augment(m2) |>
  slice_max(.cooksd, n = 1) |>
  summarise(.rownames, .cooksd)  # fila 43

augment(m2) |>
  slice_max(.hat, n = 1) |>
  summarise(.rownames, .hat)     # fila 27

# 3

m2_sin <- casen |>
  filter(row_number() != 43) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)

bind_rows(todos = tidy(m2), sin_43 = tidy(m2_sin), .id = "muestra") |>
  filter(term == "educ") |>
  select(muestra, estimate, std.error, p.value)

# El coeficiente aumenta en 2972 pesos al quitar a la persona más influyente.

### A6.

# En el modelo con controles, cada año adicional de educación se asocia con
# $42246 más de ingreso mensual, en promedio, manteniendo experiencia y
# género constantes (IC 95 %: 29487 a 55004).

# El coeficiente de `generoM` es 55686. Esto significa que por cada año adicional
# de educación, el ingreso mensual de los hombres es de $55686 más que el de las
# mujeres, en promedio, manteniendo las demás variables constantes (IC 95 %: -31246 a  142618).

# El coeficiente de generoM es el mayor pero no es significativo y con una muestra
# de 55 personas no se puede saber si realmente existe esa diferencia.

## Parte B — ENE

### B1.

# ¿Se asocia la edad con la probabilidad de tener un empleo formal, y se mantiene
# esa asociación al comparar personas del mismo sexo y región?

### B2.

ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")
ene_limpio <- read.csv("data/raw/ene_a3.csv")

regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")

ene_limpio <- ene_limpio |> mutate(edad   = ene$edad,
                                   sexo   = ene$sexo,
                                   region_txt = regiones[ene$region],
                                   formal = ocup_form == 1,
                                   hombre = sexo == 1)


summary(ene_limpio$edad)
summary(ene_limpio$sexo)
summary(ene_limpio$region_txt)

sum(is.na(ene_limpio$edad))
sum(is.na(ene_limpio$sexo))
sum(is.na(ene_limpio$region_txt))

# No se observan irregularidades en las variables añadidas.

### B3.

m_base      <- lm(formal ~ edad, data = ene_limpio, weights = fact_cal)

m_controles <- lm(formal ~ edad + hombre + region_txt, data = ene_limpio, weights = fact_cal)

### B4.

#1

modelos_B <- list(base        = m_base,
                  controles_B = m_controles)

bind_rows(base        = tidy(modelos_B$base),
          controles_B = tidy(modelos_B$controles_B), .id = "modelo")

#2

lapply(modelos_B, glance) |>
  bind_rows(.id  = "modelo") 

#3

m_3 <- lm(formal ~ edad * hombre, data = ene_limpio, weights = fact_cal)

### B5.

m_base_18_65 <- lm(formal ~ edad, data = filter(ene_limpio, edad >= 18 & edad <= 65), weights = fact_cal)

bind_rows(base        = tidy(m_base, conf.int = TRUE),
          rango_18_65 = tidy(m_base_18_65, conf.int = TRUE), .id = "modelo")

# Restringir el rango de edad entre 18 y 65 años, hace que el coeficiente siga 
# siendo negativo pero menor con una diferencia de 0.00237. 

### B6.

# La edad se asocia negativamente con la formalidad, cada año adicional de edad se
# asocia con 0,39 puntos porcentuales (pp) menos de probabilidad de tener un empleo
# formal. El modelo base responde cuánta asociación bruta hay entre edad y formalidad,
# mientras que el modelo con controles responde si la asociación se mantiene al comparar
# personas del mismo sexo y región, y casi no cambia (−0,00394 a −0,00389). Con la variable
# hombreTRUE se ve que con cada año adicional de edad, los hombres tienen 3,7 pp más de
# probabilidad que las mujeres y en cuanto a regiones, todas excepto Magallanes presentan
# menos pp de probabilidad que la base, Antofagasta. Viendo el valor-p, la edad es una variable
# muy significativa pero el efecto que tiene no es muy grande, como limitaciones están, la falta
# de muchas variables de la base de datos que no estan consideradas y podrían tener una mayor
# implicancia en el resultado y que los errores estándar de `lm()` con `weights` no consideran el diseño muestral de la ENE.
# Además, los r cuadrado son muy pequeños (0.014-0.022), lo que las variables no explican bien el modelo.

## Parte C - Comunicar





dir.create("data/processed", showWarnings = FALSE)
modelsummary(list("base" = m_base, "controles" = m_controles, "rango_18_65" = m_base_18_65),
             stars   = TRUE,
             gof_map = c("nobs", "r.squared"), output = "data/processed/t3_tabla_ene.csv")
tidy(m2, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)

