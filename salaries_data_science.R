# librerias a usar

library("readxl")
library("dplyr")
library("ggplot2")
library("tidyr")

# lectura de datos

df_salaries <- read.csv("jobs_in_data.csv")
head(df_salaries)
summary(df_salaries)

## valores faltantes
sum(is.na(df_salaries))

### ver valores unicos de algunas columnas
columns <- c("work_year","job_category", "salary_currency", "employee_residence", "experience_level", "company_location","company_size")
valores_unicos <- lapply(df_salaries[columns], unique)
valores_unicos

# value counts
table(df_salaries$work_year)
table(df_salaries$job_category)
table(df_salaries$salary_currency)
table(df_salaries$employee_residence)
table(df_salaries$experience_level)
table(df_salaries$company_location)
table(df_salaries$company_size)
table(df_salaries$work_setting)

# filtrar por pais colombia 
df_employye_colombian <- df_salaries %>% filter(employee_residence == "Colombia")
head(df_employye_colombian)

# filtrar donde el pais del empleado (nacionalidad) y el pais de la empresa sean diferentes
df_residence_location <- df_salaries %>% filter(employee_residence != company_location)
head(df_residence_location)

## df_residence_location cuales no son remotos
df_no_remote <- df_residence_location %>% filter(work_setting != "Remote")
head(df_no_remote)

# sueldos por job_title con salario medio en dolares con su respectiva desviacion estandar

df_jobs_salaries <- df_salaries %>% 
  group_by(job_title) %>% 
  summarise(
    job_count = n(),  # Número de filas en cada grupo
    mean_salary = mean(salary_in_usd), # Media de salario en cada grupo
    sd_salary = sd(salary_in_usd), # Desviación estándar de salario en cada grupo
    percent_sd = (sd_salary / mean_salary) * 100 # Porcentaje de desviación estándar
  ) %>%
  ungroup() %>%  # Desagrupar para evitar el mensaje de advertencia
  replace_na(list(sd_salary = 0, percent_sd = 0)) %>%  # Reemplazar NA con 0
  mutate(percent_sd = round(percent_sd, 2)) %>%  # Redondear a 2 decimales)
  arrange(desc(mean_salary)) # Ordenar por salario medio de mayor a menor


