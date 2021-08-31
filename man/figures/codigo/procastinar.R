#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curso IntRo
# Procastinar
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#------------
# Jugar
#------------
# Paquete : Rcade
# Disponible en GitHub

devtools::install_github('RLesur/Rcade')
Rcade::games # listado de juegos
Rcade::games$Pacman # jugar al pacman

#------------
# Ver Videos
#------------

# Paquete : tubeplayR
# Disponible en GitHub

# IMPORTANTE --> no funciona con windows!, funciona en RStudio Server y Mac

devtools::install_github("kazutan/tubeplayR")
tubeplayR::tubeplay(url = ...)





