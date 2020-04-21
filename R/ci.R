#' @title Coerciones implicitas
#'
#' @description Esta funcion devuelve el resultado del tipo de datos resultante
#'     de combinar en un vector atomico datos de distinto tipo. Los tipos posibles
#'     de datos que puede tener un vector atomico son:
#'
#'     \itemize{
#'     \item{\code{logical}}
#'     \item{\code{integer}}
#'     \item{\code{double}}
#'     \item{\code{raw}}
#'     \item{\code{complex}}
#'     \item{\code{character}}
#'     }
#'
#'     Cuando se combinan datos de distinto tipo R resuelve coercionar el vector
#'     hacia uno de los tipos que se estan combianndo. Este resultado no es aleatorio
#'     sino que es resultado de una regla clara en el lenguaje sobre 'jerarquia' de
#'     tipos de datos. La funcion con la que se verifica el tipo de dato de un
#'     vector atomico es \code{typeof()}
#'
#' @param type vector de caracteres mayor o igual a dos que indica el tipo de datos
#'     se desea testear.
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#'
#' @source http://bit.ly/Instructivo_package_IntRo
#'
#'
#' @examples
#' ci()
#' ci(c("character", "numeric"))
#' @export

ci <- function(type = c("logical", "integer", "double","raw", "complex", "character")){

        if(any(c(!is.vector(type), is.list(type), !is.character(type)))){
                stop("'type' must be a character vector.", call. = FALSE)
        }
        if(length(type) < 2){
                stop("The length of the vector must be > 1.", call. = FALSE)
        }
        dl <- length(type)
        tabla <- matrix(0, dl, dl, dimnames = list(type, type))
        for(i in 1:dl) for(j in 1:dl) {
                tabla[i, j] <- typeof(c(vector(type[i]), vector(type[j])))
        }
        tabla[upper.tri(tabla)] <- ""
        g <- paste(rep("-", max(nchar(type))), collapse = "")
        tabla <- cbind(c("", rep("|", nrow(tabla))),
                       rbind(rep(g, ncol(tabla)),tabla))
        noquote(tabla)
}





















