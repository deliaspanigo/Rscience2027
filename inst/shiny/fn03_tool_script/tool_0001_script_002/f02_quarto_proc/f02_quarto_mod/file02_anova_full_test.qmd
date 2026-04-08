---
title: "Rscience - Anova - script02 - Anova - Full test"
format: 
  html:
#    css: custom_theme.css
    grid:
      body-width: 2000px
      margin-width: 250px
      gutter-width: 1.5rem
    toc: true
    toc-float: true
    toc-location: right
    self-contained: true
    link-citations: true # Util para ver referencias en el margen (ver `tufte.html` en la galería de Quarto)
knitr: 
    opts_chunk: 
        collapse: false
        comment: ""
---

```{r}
#| eval: true
#| echo: true
#| message: false
#| warning: false
library(dplyr)
library(plotly)
```

```{r eval=TRUE, echo=FALSE}
folder_fn_local_relative <- "../zzz_fn_local"
folder_fn_local_absolute <- normalizePath(folder_fn_local_relative, mustWork = T)
vector_fn <- list.files(
  path = folder_fn_local_absolute, 
  pattern = "^fn_local.*\\.R$", 
  full.names = TRUE
)
vector_fn <- normalizePath(vector_fn, mustWork = T)

invisible(lapply(vector_fn, source, local = FALSE, encoding = "UTF-8"))
#lapply(vector_fn, source, local = FALSE, encoding = "UTF-8")
``` 

```{r eval=TRUE, echo=TRUE} 
# load("R_obj_env_script01_anova_import_and_control.RData")
# vector_all_obj_init <- ls(envir = .GlobalEnv)

# 1. Definir la ruta del archivo
path_rdata_file01 <- "R_obj_env_file01_anova_import_and_control.RData"
list_obj_file01 <- fn_load_env_RData_hidden(path_rdata = path_rdata_file01)
```

```{r eval=TRUE, echo=TRUE} 
var_name_rv <- list_obj_file01$var_name_rv
var_name_factor <- list_obj_file01$var_name_factor
my_minidataset <-  list_obj_file01$my_minidataset 
df_factor_info <- list_obj_file01$df_factor_info
vector_color_levels_new <- list_obj_file01$vector_color_levels_new
alpha_value <- list_obj_file01$alpha_value
```

```{r eval=TRUE, echo=TRUE} 

rm(list_obj_file01)
vector_all_obj_init <- ls(envir = .GlobalEnv)
#vector_all_obj_init <- ""
```


```{r eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE}
# Libraries
  library("htmlwidgets")
  library("knitr")


# General config
options(width = 500)

```


## Section 01 - Libraries
Lo primero que haremos siempre será cargar las librerias necesarias para obtener 
la totalidad de las tablas, gráficos y test estadísticos.
```{r eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE}
# # # # # Section 01 - Libraries ---------------------------------------------
  library("stats")     # General Linear Models
  library("agricolae") # Tukey test
  library("dplyr")     # Developing with %>%
  library("stringr")   # Strings replacement
```




## Section 08 - Anova Test
Realizamos el test de Anova a 1 Factor.
Para quien es usuario de R pero no avanzado, esta es la manera en que se automatiza el análisis
tomando la información detallada en la sección 03.
Para recalcar que estamos en presencia de un modelo lineal general, hacemos explícita la utilización de 
la función lm() y luego aplicamos la función aov().
```{r eval=TRUE, echo=TRUE}
  # # # # # Section 08 - Anova Test ----------------------------------------------
  # # # Anova test
  the_formula <- paste0(var_name_rv,  " ~ " , var_name_factor)
  the_formula <- as.formula (the_formula)
  the_formula
```

```{r eval=TRUE, echo=TRUE}

  list_lm_anova <- lm(formula = the_formula, data = my_minidataset)               # Linear model
  list_aov_anova <- aov(list_lm_anova)                                 # R results for anova
  df_table_anova <- as.data.frame(summary(list_aov_anova)[[1]])   # Common anova table
  df_table_anova
  

```  


La tabla de Anova obtenida en R no es exactamente la tabla de Anova que uno espera según la bibliografía.
Podemos observar que la tabla obtenida no contiene la fila referida a la variación total, y que no detalla explícitamente una
columna como "fuentes de variación". Creamos entonces nuestra propia tabla según las especificaciones
de la bibliografía tradicional.
```{r eval=TRUE, echo=TRUE}  
  df_table_anova_classroom <- df_table_anova
  new_row_anova <- nrow(df_table_anova_classroom)+ 1
  df_table_anova_classroom[new_row_anova, ] <- rep(NA, ncol(df_table_anova_classroom))
  df_table_anova_classroom$"Df"[new_row_anova] <- sum(na.omit(df_table_anova_classroom$"Df"))
  df_table_anova_classroom$"Sum Sq"[new_row_anova] <- sum(na.omit(df_table_anova_classroom$"Sum Sq"))
  df_table_anova_classroom$"Mean Sq"[new_row_anova] <- df_table_anova_classroom$"Sum Sq"[new_row_anova]/df_table_anova_classroom$"Df"[new_row_anova]
  df_table_anova_classroom <- cbind.data.frame(c("Factor", "Error", "Total"), df_table_anova_classroom)
  colnames(df_table_anova_classroom)[1]  <- "Sources of Variation"
  rownames(df_table_anova_classroom)[new_row_anova]  <- "Total"
  df_table_anova_classroom
```

Por último, tomamos de la tabla de anova el error cuadrático medio del error para utilizarlo luego.
```{r eval=TRUE, echo=TRUE}

  # # # Standard error from model for each level
  model_error_var_MSE <- df_table_anova$`Mean Sq`[2]
  model_error_sd <- sqrt(model_error_var_MSE)
  
  df_model_error <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "model_error_var_MSE" = model_error_var_MSE,
    "model_error_sd" = model_error_sd
  )
  df_model_error["model_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$n)
  df_model_error
  
  
  
```

Recordamos al operador que aún no hemos realizado ningún tipo de verificación sobre
los requisitos del modelo, y por lo tanto aún no sabemos si los valores estadísticos obtenidos 
para el test de Anova son válidos para sacar conclusiones. En secciones posteriores
realizamos la verificaciòn de los requisitos y una automatización de la interpretación 
de los valores estadísticos.


## Section 09 - Tukey test
Realizar el test de Tukey en R es un poco más complicado de lo que uno espera.
El test de Tukey necesita:  
- Los resultados de Anova.  
- El valor de alfa.  
- Un valor lógico que define si hay o no desbalance en repeticiones entre los niveles del factor.  

Tenemos ya los resultados de Anova obtenidos en la sección anterior y el valor de alfa detallado en la
sección 03.

Se dice que hay desbalance en repeticiones cuando el 'n' entre los niveles del factor no es el mismo.
Si hay desbalance internamente el test de Tukey debe generar una corrección.  
Determinamos entonces si hay desbalance o no, y generamos a su vez una frase automatizada sobre esta
característica de nuestros datos.
```{r eval=TRUE, echo=TRUE}

# # # ## Section 09 - Tukey test

  # Step 01 - Check unbalanced repetitions on levels
  # # # Unbalanced reps for levels?
  # # # Important information for Tukey test.
  check_unbalanced_reps <- length(unique(df_factor_info$n)) > 1
  check_unbalanced_reps
  
  phrase_yes_unbalanced <- "The design is unbalanced in repetitions. A correction is applied to the Tukey test."
  phrase_no_unbalanced  <- "The design is not unbalanced in repetitions. A correction not be applied to the Tukey test."
  
  phrase_selected_check_unbalanced <- ifelse(test = check_unbalanced_reps, 
                                  yes = phrase_yes_unbalanced,
                                  no  = phrase_no_unbalanced)
  
  phrase_selected_check_unbalanced

```  

Ahora si estamos en condiciones de realizar el test de Tukey.
Lo obtendremos el test de Tukey en dos formatos.
El primer formato es como informe agrupado, en el cual obtenemos la información clásica
de los agrupameintos de Tukey con letras para los niveles del factor.
El segundo formato es el listado completo de todas las comparaciones dobles realizadas para generar el primer informe.

El formato clásico para Tukey:
```{r eval=TRUE, echo=TRUE}

  ##############################################################################
  tukey01_full_groups <- agricolae::HSD.test(y = list_lm_anova,
                                             trt = var_name_factor,
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = check_unbalanced_reps)
  
  
  tukey01_full_groups
```

El formato con todos los pares de comparaciones utilizados para generar el primer formato:
```{r eval=TRUE, echo=TRUE}

  # # # Tukey test - Tukey pairs comparation - Full version
  tukey02_full_pairs <- agricolae::HSD.test(y = list_lm_anova,
                                            trt = var_name_factor,
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = check_unbalanced_reps)
  
  tukey02_full_pairs
```
  
Podemos ahora simplemente obtener la famosa tabla de Tukey:  
```{r eval=TRUE, echo=TRUE}

  # # Original table from R about Tukey
  df_tukey_original_table <- tukey01_full_groups$groups
  df_tukey_original_table
```  
  
La tabla que ofrece R es ligeramente diferente a la detallada en bibliografía, por lo cual armamos
nuestra propia tabla:
```{r eval=TRUE, echo=TRUE}

  # # # New table about Tukey
  df_tukey_table_classroom <- data.frame(
    "order" = 1:nrow(tukey01_full_groups$groups),
    "level" = rownames(tukey01_full_groups$groups),
    "mean" = tukey01_full_groups$groups[,1],
    "group" = tukey01_full_groups$groups[,2]
  )
   df_tukey_table_classroom[,"level"] <- factor(
  x = df_tukey_table_classroom[,"level"],       # La variable original de factor
  levels = df_tukey_table_classroom[,"level"]  # El orden de los niveles que calculamos en el Paso 2
)
  df_tukey_table_classroom
```  
  
  
```{r eval=TRUE, echo=TRUE}

df_table_tukey_plot001 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(my_minidataset[,var_name_rv], my_minidataset[,var_name_factor], mean),
    "model_error_se" = df_model_error$model_error_se
  )
  df_table_tukey_plot001["lower_limit"] <- df_table_tukey_plot001$mean - df_table_tukey_plot001$model_error_se
  df_table_tukey_plot001["upper_limmit"] <- df_table_tukey_plot001$mean + df_table_tukey_plot001$model_error_se
  df_table_tukey_plot001["color"] <- df_factor_info$color

  df_table_tukey_plot001[,"level"] <- factor(
  x = df_table_tukey_plot001[,"level"],       # La variable original de factor
  levels = df_table_tukey_plot001[,"level"]  # El orden de los niveles que calculamos en el Paso 2
)
  

correct_pos_letters <- order(df_tukey_table_classroom$level)
vector_letters <- df_tukey_table_classroom$group[correct_pos_letters]
df_table_tukey_plot001["group"] <- vector_letters
df_table_tukey_plot001
```

```{r eval=TRUE, echo=TRUE}
  # # # Create a new plot...
  plot_tukey001 <- plotly::plot_ly()
  
  
  # # # Adding errors...
  plot_tukey001 <-   plotly::add_trace(p = plot_tukey001,
                                        type = "scatter",
                                        mode = "markers",
                                        x = df_table_tukey_plot001$level,
                                        y = df_table_tukey_plot001$mean,
                                        color = df_table_tukey_plot001$level,
                                        colors = df_table_tukey_plot001$color,
                                        marker = list(symbol = "line-ew-open",
                                                      size = 50,
                                                      opacity = 1,
                                                      line = list(width = 5)),
                                        error_y = list(type = "data", array = df_table_tukey_plot001$model_error_se)
  )
  
  
  
  plot_tukey001 <-  add_text(p = plot_tukey001,
                              x = df_table_tukey_plot001$level,
                              y = df_table_tukey_plot001$mean,
                              text = df_table_tukey_plot001$group, name = "Tukey Group",
                              size = 20)
  
  # # # Title and settings...
  plot_tukey001 <- plotly::layout(p = plot_tukey001,
                                   title = "Plot 007 - Mean y model standard error",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  # # # Without zerolines
  plot_tukey001 <-plotly::layout(p = plot_tukey001,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))
  
  
  plot_tukey001 <- plotly::plotly_build(plot_tukey001)
  
  # # # Plot output
  plot_tukey001
  

```  

   




## Section 10 - my_minidataset_ext
Hasta el momento nuestro objeto my_minidataset contiene la variable respuesta, el factor, el color correspondiente a 
cada nivel del factor, y el id tanto respecto a my_datasete y my_minidataset.
En la obtención del test de Anova han sido generado también los residuos de cada  dato, sera posible y necesario generar a partir de ellos los residuos studentizados con ciertas peculiaridades para el casod e Anova.
Los residuos serán utilizados en secciones posteriores para la comprobación de los requisitos del modelo.
Toda esta inforamción será recolectada en un nuevo dataframa llamado "my_minidataset_ext" con la idea de manifestar que es una extension de lo que ha sido el minidataset original.
```{r eval=TRUE, echo=TRUE}


  
  # Creation: Dataframe my_minidataset_ext
  my_minidataset_ext <- my_minidataset
  my_minidataset_ext$"residuals"      <- list_lm_anova$residuals
  my_minidataset_ext$"studres"        <- my_minidataset_ext$"residuals"/model_error_sd

  
  # First 5 rows from my_minidataset_ext
  head(x = my_minidataset_ext, n = 5)
```  
  
Algo importante de las sentencias anteriores es la manera en que se ha definido a los residuos estandarizados.  
El modelo de anova tiene su pripia estimación de varianza del error que no es la estimación de la varianza tomando todos los residuos, ya que difiere en los grados de libertad. La estandarización de los residuos se realiza entonces con el desvío estandard del error del modelo, y no con desvío estándard de todos los residuos ni con el desvio estandard de cada nivel del factor. Un error comun aqui es aplicar funciones de R para realizar la studentizacion de los residuos.
Dichas funciones no obtienen el desvio estadnard del modelo sino que aplican otras reglas que no son las correctas para este caso.
  
  
## Section 11 - Requeriment - Normality on residuals - Shapiro-Wilk test
Realizamos el test de normalidad de Shapiro-Wilk sobre los residuos.  
```{r eval=TRUE, echo=TRUE}
  
  
  
  # # # ## Section 11 - Requeriment - Normality on residuals - Shapiro-Wilk test
  # # # Normality test (Shapiro-Wilk)
  list_test_residuals_normality <- shapiro.test(x = my_minidataset_ext$"residuals")
  list_test_residuals_normality
```

De la salida original de R podemos armar la siguiente tabla.
```{r eval=TRUE, echo=TRUE}
  

df_normality <- data.frame(
  "variable" = "residuals", 
  "test" = list_test_residuals_normality$method,
  "statistic"  = list_test_residuals_normality$statistic,
  "p_value" = list_test_residuals_normality$p.value
)
str_new_name_shapiro <- paste0("statistic", "(", names(list_test_residuals_normality$statistic), ")")
colnames(df_normality)[3] <- str_new_name_shapiro
rownames(df_normality) <- NULL
df_normality

```  



## Section 12 - Requeriment - Homogeneity of variance from residuals - Bartlett test
Realizamos el test de homogeneidad de varianzas de Bartlett sobre los residuos.

```{r eval=TRUE, echo=TRUE}
  # # # # # Section 12 - Requeriment - Homogeneity of variance from residuals - Bartlett test 
  # # # Homogeinidy test (Bartlett)
  the_formula_bartlett <- paste0("residuals", " ~ ", var_name_factor)
  the_formula_bartlett <- as.formula(the_formula_bartlett)
  list_test_residuals_homogeneity <- bartlett.test(the_formula_bartlett, data = my_minidataset_ext)
  list_test_residuals_homogeneity
  
```  

```{r eval=TRUE, echo=TRUE}


df_homogeneity <- data.frame(
  "variable" = "residuals", 
  "test" = list_test_residuals_homogeneity$method,
  "statistic"  = list_test_residuals_homogeneity$statistic,
  "p_value" = list_test_residuals_homogeneity$p.value
)
str_new_name_bartlett <- paste0("statistic", "(", names(list_test_residuals_homogeneity$statistic), ")")
colnames(df_homogeneity)[3] <- str_new_name_bartlett
rownames(df_homogeneity) <- NULL
df_homogeneity

```  




```{r eval=TRUE, echo=TRUE}

  # # # Residuals variance from levels from original residuals
  df_raw_error <- data.frame(
    "order" = 1:nlevels(my_minidataset_ext[,var_name_factor]),
    "level" = levels(my_minidataset_ext[,var_name_factor]),
    "n" = tapply(my_minidataset_ext$"residuals", my_minidataset_ext[,var_name_factor], length),
    "raw_error_var" = tapply(my_minidataset_ext$"residuals", my_minidataset_ext[,var_name_factor], var),
    "raw_error_sd" = tapply(my_minidataset_ext$"residuals", my_minidataset_ext[,var_name_factor], sd)
  )
  df_model_error["raw_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$"n")
  rownames(df_raw_error) <- 1:nrow(df_raw_error)
  df_raw_error
  
  phrase_info_errors <- "
Anova and Tukey use MSE from model with 'n-k' degree of freedom.
Bartlett use variance from raw error on each level with 'n_i' defree of freedom on each level.
Only if there is homogeneity from raw error variances then is a good idea take decision from MSE in Anova and Tukey."
  
  cat(phrase_info_errors)

``` 


## Section 13 - Requeriment - Mean 0 for residuals
Uno de los requisitos del modelo es la media 0 para los residuos.  
Por suerte para nosotros esto no es necesario probar ya que ocurre si o si en cualquier contexto con o sin normalidad de  los reisduos e incluso con o sin homogeneidad de los residuos. Hay una prueba estadistica para realizar ya que ocurre como una caracteristica intrinseca de los residuos y hay una demostracion matematica que lo demuestra.
Hay otro detalle propio de los residuos que es  que la suma de l os residuos da cero.
En cualquier caos, podemos tomar los residuos y realizar  la  sumatoria y obtener la media.

```{r eval=TRUE, echo=TRUE}

  # # # Sum for residuals
  sum_residuals <- sum(my_minidataset_ext$"residuals")
  sum_residuals
``` 
Si bien la suma de los resiudos puede no ser cero, esto se debe a pequenios errores de calculo informatico. En caulquier caso debiera ocurrir obtener  una suma de los residuos cero o muy pequenia.  
  
```{r eval=TRUE, echo=TRUE}
  
  # # # Mean for residuals
  mean_residuals <- mean(my_minidataset_ext$"residuals")
  mean_residuals
  
``` 
La media de los residuos debe ser cero por demostracion matematica, y ocurre que los pequenios errores de calculo informatico nos llevan a que no sea exactamente cero.



```{r eval=TRUE, echo=TRUE} 

vector_all_obj_end <- ls(envir = .GlobalEnv)

# Excluir los que NO quieres guardar
vector_exclusion <- vector_all_obj_init
vector_save_obj  <- setdiff(vector_all_obj_end, vector_exclusion)

# Guardar todo excepto los excluidos
save(list = vector_save_obj, file = "R_obj_env_file02_anova_full_test.RData")

```



