# Cargar paquetes necesarios
library(ggplot2)
library(lmtest)     # Para Breusch-Pagan test
library(car)        # Para VIF (Multicolinealidad)
library(tseries)    # Para Jarque-Bera test

# Establecer una semilla para reproducibilidad
set.seed(123)

# Generar datos sintéticos
n <- 500
datos <- data.frame(
  educacion = sample(5:20, n, replace = TRUE),
  experiencia = sample(0:40, n, replace = TRUE)
)

# Crear variable de ingresos según la ecuación de Mincer
beta_0 <- 8
beta_1 <- 0.08  # Retorno a la educación
beta_2 <- 0.04  # Retorno a la experiencia
beta_3 <- -0.0008  # Penalización por experiencia cuadrática

# Generar ingresos logarítmicos con ruido aleatorio
datos$log_ingreso <- beta_0 + beta_1 * datos$educacion + 
  beta_2 * datos$experiencia + beta_3 * (datos$experiencia^2) + 
  rnorm(n, mean = 0, sd = 0.3)

# Transformar ingreso de logaritmo a valor real
datos$ingreso <- exp(datos$log_ingreso)

# Ajustar la regresión minceriana
modelo <- lm(log_ingreso ~ educacion + experiencia + I(experiencia^2), data = datos)

# Resumen del modelo
summary(modelo)

# **Prueba de significancia individual (t-test en regresión)**
cat("\n### Prueba de significancia individual (t-test en regresión) ###\n")
print(summary(modelo)$coefficients)

# **Prueba de significancia global del modelo (F-test)**
cat("\n### Prueba de significancia global (F-test) ###\n")
print(summary(modelo)$fstatistic)

# **Prueba de heterocedasticidad (Breusch-Pagan Test)**
cat("\n### Prueba de heterocedasticidad (Breusch-Pagan Test) ###\n")
bp_test <- bptest(modelo)
print(bp_test)

# **Prueba de normalidad de residuos**
# Shapiro-Wilk Test
cat("\n### Prueba de normalidad de residuos (Shapiro-Wilk Test) ###\n")
shapiro_test <- shapiro.test(residuals(modelo))
print(shapiro_test)

# Jarque-Bera Test
cat("\n### Prueba de normalidad de residuos (Jarque-Bera Test) ###\n")
jb_test <- jarque.bera.test(residuals(modelo))
print(jb_test)

# **Prueba de multicolinealidad (VIF - Variance Inflation Factor)**
cat("\n### Prueba de multicolinealidad (VIF) ###\n")
vif_values <- vif(modelo)
print(vif_values)

# **Gráfico de residuos para visualizar heterocedasticidad**
ggplot(data = data.frame(residuos = residuals(modelo), fitted = fitted(modelo)), 
       aes(x = fitted, y = residuos)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Gráfico de Residuos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme_minimal()


