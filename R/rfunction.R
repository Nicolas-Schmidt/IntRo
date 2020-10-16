#' @title Busqueda aleatoria de funciones de uno o todos los paquetes
#'     instalados.
#'
#' @description Esta funcion permite obtener una funcion aleatoria de alguno de los
#'     paquete instalados  con fines de aprendizaje. Es decir,
#'     la idea es seleccionar al azar una funcion y la funcion que salga sorteada
#'     es con la que se debe experimentar para aprender sobre ella.
#'
#' @param package por defecto en la funcion esta cargado el paquete \code{base}.
#'      Pero se puede seleccionar cualquiera de los paquetes que fueron instalados
#'      en su computadora.
#'
#' @param ALL por defecto es \code{FALSE}. Si es \code{TRUE} significa que la busqueda
#'     aleatoria se va a buscar entre todos los paquetes que estan cargados en la
#'     sesion de trabajo actual. Si adicionalmente se selecciono un paquete en
#'     \code{package} este sera incorporado al muestreo.
#'
#' @param library por defecto es \code{TRUE}. Esto significa que el paquete seleccionado
#'    queda cargado en la rurta de busqueda de nombres de R. Caso contrario el paquete no
#'    va a quedar cargado.
#'
#' @return Esta funcion va a imprimir en pantalla la funcion sorteada, el paquete al
#'     que pertenece y la cantidad de funciones que ese paquqte tiene. Adicionalmente
#'     se va imprimir una descripcion del paquete y se va a abrir un device de la
#'     ayuda de la funcion sorteada.
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#'
#' @source http://bit.ly/Instructivo_package_IntRo
#'
#'
#' @details Si se selecciona un paquete que no esta cargado en la sesion
#'     de trabajo no se afecta la ruta de busqueda de nombres de R. El
#'     paquete seleccionado no va a quedar cargado en la ruta actual unicamente
#'     si selecciona  \code{library = FALSE}.Si se selecciona un paquete que
#'     no esta instalado la funcion le va a preguntar si desea instalarlo mediante
#'     una ventana emergente. Si no desea instalar o la instalacion es fallida por
#'     algun motivo la funcion va a debolver un muestreo del paquete 'base'.
#'
#'
#' @examples
#'
#' rfunction()
#' rfunction(ALL = TRUE)
#' rfunction(package = "stats")
#'
#'
#' @export

rfunction <- function(package = "base",
                      ALL = FALSE,
                      library = TRUE){

        if(length(package) > 1){
                stop("The argument 'package' must not have a length greater than one.", call. = FALSE)
        }
        if(!package %in% utils::installed.packages()[,1]){
                qu <- utils::askYesNo("You do not have the package installed: do you want to install it?")
                if(isTRUE(qu)){
                        suppressMessages(utils::install.packages(package))
                        pack <- package
                        package <- ifelse(package %in% utils::installed.packages()[,1], package, "base")
                        if(package=="base"){
                                cat(crayon::red$bold("\n ::As the package", pack, "could not be installed \n correctly, we continue with the 'base' package::\n\n"))
                        }
                }else{
                        package <- "base"
                }
        }
        p <- paste0("package:", package)
        r <- search()
        if(!p %in% r){
                suppressMessages(require(package, character.only = TRUE))
        }
        r. <-  search()[grepl("^package:", search())]
        if(ALL){
                package <- r.[sample(2:length(r.), 1)]
                FUN <- sample(ls(r.[r. == package]), 1)
                package <- stringr::str_extract(package,"(?<=:).*")
        }else{
                FUN <- sample(ls(r.[r.==p]), 1)
        }
        n <- length(ls(search()[search() == utils::find(FUN)]))
        if(!library){
                end <- replicate(length(search())-length(r), detach(pos = 2))
        }
        cat('\n - The function of the day is:', crayon::green$bold(FUN),
            '\n - Package:', crayon::blue$bold(package),'-->', n, 'functions',
            '\n',
            '\n - Package Description:', utils::packageDescription(package)$Description)
        utils::help(FUN, package = noquote(package))
}


