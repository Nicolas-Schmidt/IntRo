#' @title Presentaciones del curso
#' @description Permite acceder a las presentaciones (slides) del docente en formato .pdf
#' @param modulo valor numerico que indica el la presentacion de que modulo se desea abrir.
#'     \describe{
#'              \item{0}{Preliminares}
#'              \item{1}{¿Que es R?}
#'              \item{2}{Fundamentos de programacion en R}
#'              \item{3}{Vectores}
#'              \item{4}{Matrices}
#'              \item{5}{Data Frame}
#'              \item{6}{Listas}
#'              \item{7}{Graficos}
#'              \item{8}{Sentencias de control y Funciones}
#'             }
#' @details Si se pone por valor del argumento \code{modulo} un numero fuera del rango se va a
#'     abrir la carpeta en la que estan las presentaciones y desde ahi poder seleccionar la que se desea.
#' @return Abre un archivo .pdf en el lector de pdf que se tenga instalado.
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#' @examples
#' # presentaciones()
#' # presentaciones(modulo = 1)
#' @export

presentaciones <- function(modulo = 0){

    sf <- system.file("presentaciones", package = "IntRo")
    files <- dir(path = sf)
    p <- files[grep(paste0("M", modulo), x = files)]
    system2('open', args = paste0(sf, "/", p), wait = FALSE)

}

#' @title Manual de envio de ejercicio y paquete
#' @description Permite acceder al manual del procedimiento de envio por mail desde R de los ejercicios
#'     una vez finalizados. Tambien permite acceder al manual del paquete y a la documentacion de todas las
#'     funciones del este.
#' @param manual valor de tipo \code{character} que por defecto es \code{enviar} y admite otro valor que es
#'     el de \code{paquete}.
#' @return Abre un archivo .pdf en el lector de pdf que se tenga instalado.
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#' @examples
#' # manual()
#' # manual(manual = "paquete")
#' @export

manual <- function(manual = "envio"){

    if(manual == "envio") m <- "/Instructivo_IntRo.pdf" else  m <- "/Manual_IntRo.pdf"
    sf <- system.file("manual", package = "IntRo")
    system2('open', args = paste0(sf, m), wait = FALSE)
}


#' @title Pauta de ejercicios del curso IntRo
#' @description Permite acceder a los repartidos de ejercicios de cada modulo del curso
#' @param modulo valor numerico que indica el repartido de ejercicios que se desea consultar.
#'     \describe{
#'              \item{1}{Vectores}
#'              \item{2}{Matrices}
#'              \item{3}{Data Frame}
#'              \item{4}{Listas}
#'             }
#' @details Si se pone por valor del argumento \code{modulo} un numero fuera del rango se va a
#'     abrir la carpeta en la que estan los ejercicios y desde ahi poder seleccionar la que se desea.
#' @return Abre un archivo .pdf en el lector de pdf que se tenga instalado.
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#' @examples
#' # ejercicios()
#' # ejercicios(modulo = 1)
#' @export

ejercicios <- function(modulo = 1){

    sf <- system.file("ejercicios", package = "IntRo")
    files <- dir(path = sf)
    e <- files[grep(paste0("M", modulo), x = files)]
    system2('open', args = paste0(sf, "/", e), wait = FALSE)

}
