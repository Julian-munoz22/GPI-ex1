# Cargar paquetes necesarios
library(ggplot2)


set.seed(123)


n <- 500  # Número de observaciones
datos <- data.frame(
  educacion = sample(5:20, n, replace = TRUE),  # Años de educación
  experiencia = sample(0:40, n, replace = TRUE) # Años de experiencia
)

# Crear variable de ingresos según la ecuación de Mincer
# ln(ingreso) = β0 + β1 * educación + β2 * experiencia + β3 * experiencia^2 + error
beta_0 <- 8
beta_1 <- 0.08  # Retorno a la educación
beta_2 <- 0.04  # Retorno a la experiencia
beta_3 <- -0.0008  # Penalización por experiencia cuadrática

# Generar ingresos logarítmicos con un poco de ruido aleatorio
datos$log_ingreso <- beta_0 + beta_1 * datos$educacion + 
  beta_2 * datos$experiencia + beta_3 * (datos$experiencia^2) + 
  rnorm(n, mean = 0, sd = 0.3)

# Transformar el ingreso de logaritmo a valor real
datos$ingreso <- exp(datos$log_ingreso)

# Output 1: Mostrar los primeros registros del dataframe
print(head(datos))

# Ajustar la regresión minceriana
modelo <- lm(log_ingreso ~ educacion + experiencia + I(experiencia^2), data = datos)


summary(modelo)


ggplot(datos, aes(x = educacion, y = ingreso)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  labs(title = "Relación entre Educación e Ingreso",
       x = "Años de Educación",
       y = "Ingreso") +
  theme_minimal()

