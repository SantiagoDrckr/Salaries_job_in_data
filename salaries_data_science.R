# librerias a usar
library("readxl")
library("dplyr")
library("ggplot2")
library("tidyr")
library("corrplot")
library("sf")

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

# graficos

## histograma de sueldos en dolares

ggplot(df_salaries) + geom_histogram(aes(x = salary_in_usd),
                                     bins = 10, fill = "#8EE5EE", col ="black") +
  scale_x_continuous(breaks=c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000),
                     labels=c("0","50k","100k","150k","200k","250k","300k","350k","400k","450k","500k")) +
  labs(
    title = "Histograma de sueldos en dolares",
    x = "Sueldo promedio en dolares por año",
    y = "Frecuencia"
  )

## histograma por año

ggplot(df_salaries) + 
  geom_histogram(aes(x = salary_in_usd, fill = factor(work_year)),
                 bins = 10, col = "black", alpha = 0.15, position = "identity") +
  scale_x_continuous(breaks = seq(0, 500000, by = 50000),
                     labels = c("0", "50k", "100k", "150k", "200k", "250k", "300k", "350k", "400k", "450k", "500k")) +
  labs(
    title = "Histograma de sueldos en dólares",
    x = "Sueldo promedio en dólares por año",
    y = "Frecuencia"
  )

## histograma por nivel de experiencia junior

junior <- df_salaries %>% filter(experience_level == "Entry-level")
ggplot(junior) + geom_histogram(aes(x = salary_in_usd),
                                     bins = 14, fill = "#8EE5EE", col ="black") +
  scale_x_continuous(breaks=c(0,20000,40000,60000,80000,100000,120000,140000,160000,180000,200000,220000,240000,260000,280000),
                     labels=c("0","20k","40k","60k","80k","100k","120k","140k","160k","180k","200k","220k","240k","260k","280K")) +
  labs(
    title = "Histograma de sueldos en dolares para junior",
    x = "Sueldo promedio en dolares por año",
    y = "Frecuencia"
  )

## barras por job_category
df_jobs_category <- df_salaries %>% 
  group_by(job_category) %>% 
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

ggplot(head(df_jobs_category)) + 
  geom_bar(aes(x = job_category, y = mean_salary, fill = factor(job_category)),
           stat = "identity", width = 0.7) +
  coord_flip() +
  labs(
    title = "Sueldo promedio por trabajo",
    x = "Trabajo",
    y = "Sueldo promedio en dólares al año"
  ) + theme(legend.position = "none")

## grafico de barras por job_category para cada año

ggplot(df_salaries) + 
  geom_bar(aes(x = job_category, y = salary_in_usd, fill = factor(work_year)),
           stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k")) +
  labs(
    title = "Sueldo promedio por trabajo",
    x = "Trabajo",
    y = "Sueldo promedio en dólares al año"
  ) 

## grafico de barras por job_category para cada año para junior (entry-level)

ggplot(junior) + 
  geom_bar(aes(x = job_category, y = salary_in_usd, fill = factor(work_year)),
           stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k")) +
  labs(
    title = "Sueldo promedio junior por trabajo",
    x = "Trabajo",
    y = "Sueldo promedio en dólares al año"
)

# grafico de cajas

ggplot(df_salaries) + 
  geom_boxplot(aes(x = job_category, y = salary_in_usd, fill = factor(job_category))) +
  coord_flip() +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k")) +
  labs(
    title = "Sueldo promedio por trabajo",
    x = "Trabajo",
    y = "Sueldo promedio en dólares al año"
  ) + theme(legend.position = "none")

# grafico de cajas para junior

ggplot(junior) + 
  geom_boxplot(aes(x = job_category, y = salary_in_usd, fill = factor(job_category))) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "k"), limits = c(0,300000)) +
  labs(
    title = "Sueldo promedio junior por trabajo",
    x = "Trabajo",
    y = "Sueldo promedio en dólares al año"
  ) + theme(legend.position = "none")

# grafico coropleta de sueldos por pais

df_salaries %>%
  group_by(employee_residence) %>%
  summarise(
    mean_salary = mean(salary_in_usd)
  ) %>%
  ggplot() +
  geom_sf(aes(fill = mean_salary, geometry = geometry)) +
  scale_fill_gradient(low = "#FDE725FF", high = "#440154FF") +
  labs(
    title = "Sueldo promedio por país",
    fill = "Sueldo promedio en dólares al año"
  )
