# install.packages("renv")

renv::deactivate()

# Cargar renv
library(renv)

# Inicializa renv totalmente limpio
renv::init()

# Configura para que SOLO instale lo que pongas en el DESCRIPTION
renv::settings$snapshot.type("explicit")

# Inicializar renv en tu proyecto/paquete
renv::init()

renv::install()
renv::snapshot()

# Cuando quieras guardar el estado actual de dependencias
renv::snapshot()

# Restaurar el entorno exacto si trabajo en otro equipo!
# renv::restore()

# Limpiar paquetes no utilizados
# renv::clean() ocasionalmente para eliminar paquetes no utilizados de la biblioteca privada

# Actualiza los paquetes de manera controlada
# renv::update()
#############################################################
