# 1. Preparar y Guardar los cambios (incluyendo el DESCRIPTION limpio)
git add .
git commit -m "Release: Version 0.0.21"

# 2. Etiquetar la versión
# Borramos el tag local por si acaso ya se creó con error antes
git tag -d v0.0.21 2>/dev/null
git tag -a v0.0.21 -m "Versión estable 0.0.21 del paquete"

# 3. Subir cambios y etiquetas a GitHub
git push origin main --follow-tags
