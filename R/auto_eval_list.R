#' @title Autoevaluacion de 'Ejercicios IV' (Modulo Listas).
#'
#' @description Es una funcion que permite autoevaluar los ejercicios del modulo
#'     de listas del curso Introduccion al software estadistico R.
#' @param nombre Una cadena de caracteres que indique el nombre del estudiante.
#' @param apellido Una cadena de caracteres que indique el apellido del estudiante.
#' @param mail Una cadena de caracteres que indique el mail del estudiante.
#' @param intentos Refiere a la cantidad de veces que se realizo la autoevaluacion
#'     hasta el momento en el que se resuelve que los ejercicios estan prontos para
#'     ser enviados para obtener la nota final. Por defeto es 1. La cantidad de
#'     intentos debe ser un numero entero. Si el numero no es entero la funcion
#'     va a forzar el numero a uno de tipo  \code{integer} redondeando hacia
#'     arriba.
#' @param enviar Valor logico que por defeto es \code{FALSE}. En el momento en el que el
#'     estudiante considera que los ejercicios estan completos debe enviar los
#'     resultados para obtener la nota final de los ejercicios. La nota final le va
#'     a llegar al mail que cargo en el argumento \code{mail} de esta funcion.
#' @param summary Valor logico que por defetco es \code{TRUE}. La utilidad de este
#'     argumento es contar con un resumen de la situacion de todos los ejercicios
#'     correspondientes al modulo de listas.
#'
#' @return
#'     \itemize{
#'
#'     \item{Si el argumento \code{summary} es \code{TRUE}}{ la funcion va a devolver
#'     un \code{print} del estado de los ejercicios. El resultado de los ejercicioes
#'     pueden estar en tres categorias: respuesta 'Correcta', ' Incorrecta' o '
#'     Incompleta'.}
#'     \item{Si el argumento \code{summary} es \code{FALSE}}{ la funcion no va a imprimir
#'     en la consola ninun resulatdos. Solo se van a ver los Errores y las Advertencias
#'     si son necesarias.}
#'     }
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#'
#' @source http://bit.ly/Instructivo_package_IntRo
#'
#'
#'
#' @examples
#'
#' lista1 <- list(datasets::iris)
#'
#' auto_eval_list(nombre = "Nicolas",
#'                  apellido = "Schmidt",
#'                  mail = NULL,
#'                  enviar = FALSE,
#'                  intentos = 3,
#'                  summary = TRUE)
#'
#' ## --------------------
#' ## ENVIAR EL EJERCICIO
#' ## --------------------
#'
#' # auto_eval_list(nombre   = "Nicolas",
#' #                apellido = "Schmidt",
#' #                mail     = "tu_mail.gmail.com",  ## VALOR NECESARIO!
#' #                enviar   = TRUE,                 ## VALOR NECESARIO!
#' #                intentos = 3,
#' #                summary  = FALSE)
#'
#' @export

auto_eval_list <- function(nombre = NULL,
                         apellido = NULL,
                         mail = NULL,
                         intentos = 1,
                         enviar = FALSE,
                         summary = TRUE){

        espacio <- ls(search()[1])
        cant_ej <- 6
        if(length(espacio)==0){
                stop("No hay ningun objeto creado en la sesion de trabajo. Puede consultarlo con la funcion 'objects()' o 'ls()'", call. = FALSE)
        }
        vector_objetos <- paste0("lista", 1:cant_ej)
        faltantes <- vector_objetos %in% espacio
        if(sum(faltantes)==0){
                stop("En el espacio de trabajo no hay ninguno de los objeto del ejercicio creados. Recuerde que debe utilizar los mombres que figuran en los ejercicios", call. = FALSE)
        }

        base <- as.list(stats::setNames(rep(NA, cant_ej), paste("Ejercicio", 1:cant_ej)))

        if(vector_objetos[1] %in% espacio){
                if(is.list(lista1)){
                        if(length(lista1)==3){
                                if(!all(unique(c(sapply(lista1, dim))) %in% c(50,5))){
                                        warning("Ejercicio 1: La dimension de los data.frame no son correctas.", call. = FALSE)
                                }
                                if(!all(unique(c(sapply(lista1, colnames))) == names(datasets::iris))){
                                        warning("Ejercicio 1: Las variables de alguno de los data.frame no son correctas.", call. = FALSE)
                                }
                                if(!all(c("setosa","versicolor", "virginica") == tolower(names(lista1)))){
                                        warning("Ejercicio 1: El nombre de las entradas de lista no son correctos.", call. = FALSE)
                                }
                                base[[1]] <- ifelse(all(unique(c(sapply(lista1, dim))) %in% c(50,5)) &&
                                                    all(unique(c(sapply(lista1, colnames))) == names(datasets::iris)) &&
                                                    all(c("setosa","versicolor", "virginica") == tolower(names(lista1))) &&
                                                    length(lista1)==3, 1, 0)
                        }else{
                                warning("Ejercicio 1: El largo de la lista no es correcto.", call. = FALSE)
                                base[[1]] <- 0
                        }
                } else{
                        warning("Ejercicio 1: El objeto 'lista1' no es de tipo lista.", call. = FALSE)
                        base[[1]] <- 0
                }
        }

        if(vector_objetos[2] %in% espacio){
                if(is.list(lista2)){
                        if(length(lista2)==6){
                                if(!all(unique(c(sapply(lista2[1:3], dim))) %in% c(50,5))){
                                        warning("Ejercicio 2: La dimension de los data.frame de las 3 primeras entradas de la lista no son correctas. Verifique no haber guardado una lista dentro de otra lista", call. = FALSE)
                                }
                                if(!all(unique(c(sapply(lista2[1:3], colnames))) == names(datasets::iris))){
                                        warning("Ejercicio 2: Las variables de alguno de los data.frame no son correctas.", call. = FALSE)
                                }
                                if(!all(c("setosa","versicolor", "virginica") == tolower(names(lista2[1:3])))){
                                        warning("Ejercicio 2: El nombre de las entradas de lista no son correctos.", call. = FALSE)
                                }
                                if(!all(c(lista2[[4]], lista2[[5]], lista2[[6]]) %in% c(5.01, 5.94, 6.59))){
                                        warning("Ejercicio 2: Alguno de los valores promedio no es correcto.", call. = FALSE)
                                }
                                base[[2]] <- ifelse(all(unique(c(sapply(lista2[1:3], dim))) %in% c(50,5)) &&
                                                            all(unique(c(sapply(lista2[1:3], colnames))) == names(datasets::iris)) &&
                                                            all(c("setosa","versicolor", "virginica") == tolower(names(lista2[1:3]))) &&
                                                            all(c(lista2[[4]], lista2[[5]], lista2[[6]]) %in% c(5.01, 5.94, 6.59)), 1, 0)
                        }else{
                                warning("Ejercicio 2: El largo de la lista no es correcto.", call. = FALSE)
                                base[[2]] <- 0
                        }
                } else{
                        warning("Ejercicio 2: El objeto 'lista2' debe ser de tipo 'list'.", call. = FALSE)
                        base[[2]] <- 0
                }
        }

        if(vector_objetos[3] %in% espacio){
                if(is.list(lista3)){
                        if(length(lista3)==4){
                                if(!all(lista3[[1]]==c(0:10))){
                                        warning("Ejercicio 3: La secuencia del primer vector no es correcta.", call. = FALSE)
                                }
                                if(!all(lista3[[2]]==c(10:20))){
                                        warning("Ejercicio 3: La secuencia del segundo vector no es correcta.", call. = FALSE)
                                }
                                if(!all(lista3[[3]]==c(30:40))){
                                        warning("Ejercicio 3: La secuencia del tercer vector no es correcta.", call. = FALSE)
                                }
                                if(!all(lista3[[4]]==c(40:50))){
                                        warning("Ejercicio 3: La secuencia del cuarto vector no es correcta.", call. = FALSE)
                                }
                                base[[3]] <- ifelse(all(unlist(lista3)==c(0:10, 10:20, 30:40, 40:50)), 1, 0)
                        }else{
                                warning("Ejercicio 3: El largo de la lista no es correcto.")
                                base[[3]] <- 0
                        }
                } else{
                        warning("Ejercicio 3: El objeto 'lista3' debe ser de tipo 'list'.", call. = FALSE)
                        base[[3]] <- 0
                }
        }

        if(vector_objetos[4] %in% espacio){
                if(is.list(lista4)){
                        if(length(lista4)==1){
                                if(is.matrix(lista4[[1]])){
                                        if(all(c(4,11) == dim(lista4[[1]]))){

                                                if(!all(rbind(0:10, 10:20, 30:40, 40:50) == lista4[[1]])){
                                                        warning("Ejercicio 4: Los vectores pegados no son los correctos.", call. = FALSE)
                                                }else{
                                                        base[[4]] <- 1
                                                }
                                        }else{
                                                warning("Ejercicio 4: La dimension de la matriz no es correcta.", call. = FALSE)
                                                base[[4]] <- 0
                                        }

                                }else{
                                        warning("Ejercicio 4. La lista debe tener una matriz", call. = FALSE)
                                        base[[4]] <- 0
                                }
                        }else{
                                warning("Ejercicio 4: El largo de la lista no es correcto.", call. = FALSE)
                                base[[4]] <- 0
                        }
                } else{
                        warning("Ejercicio 4: El objeto 'lista4' debe ser de tipo 'list'.", call. = FALSE)
                        base[[4]] <- 0
                }
        }

        if(vector_objetos[5] %in% espacio){
                 if(is.list(lista5)){
                         if(length(lista5)==11){
                                 if(!all(unlist(lista5)==c(20:30))){
                                         warning("Ejercicio 5: Alguno de los promedios no son correctos.", call. = FALSE)
                                 }
                                 base[[5]] <- ifelse(all(unlist(lista5)==c(20:30)), 1, 0)
                         }else{
                                 warning("Ejercicio 5: El largo de la lista no es correcto.", call. = FALSE)
                                 base[[5]] <- 0
                         }
                 } else{
                         warning("Ejercicio 5: El objeto 'lista5' debe ser de tipo 'list'.", call. = FALSE)
                         base[[5]] <- 0
                 }
         }

        if(vector_objetos[6] %in% espacio){
                 if(is.list(lista6)){
                         if(length(lista6)==1){
                                 if(sum(dim(lista6[[1]]))!=8){
                                         warning("Ejercicio 6: La dimension del objeto guardado no es correcta.", call. = FALSE)
                                 }
                                 if(!all(lista6[[1]][,1]==c("ARG", "BRA", "URY"))){
                                         warning("Ejercicio 6: El objeto guardado no es correcto.", call. = FALSE)
                                 }
                                 base[[6]] <- ifelse(sum(dim(lista6[[1]]))==8 &&
                                                             all(lista6[[1]][,1]==c("ARG", "BRA", "URY")), 1, 0)
                         }else{
                                 warning("Ejercicio 6: El largo de la lista no es correcto.", call. = FALSE)
                                 base[[6]] <- 0
                         }
                 } else{
                         warning("Ejercicio 6: El objeto 'lista6' debe ser de tipo 'list'.", call. = FALSE)
                         base[[6]] <- 0
                 }
         }


        base1 <- do.call(rbind, base)
        base2<- cbind(base1, base1[,1], base1[,1])
        colnames(base2)<-c("Correcta", "Incorrecta", "Incompleta")
        base2[,1] <- ifelse(is.na(base2[,1]) | base2[,1]==0, 0, 1)
        base2[,2] <- ifelse(is.na(base2[,2]) | base2[,2]==1, 0, 1)
        base2[,3] <- ifelse(is.na(base2[,3]), 1, 0)
        base2 <- rbind(base2, TOTAL = colSums(base2))

        if(summary == TRUE){
                cat('\n============================================')
                cat('\n       RESULTADO DE LOS EJERCICIOS          ')
                cat('\n============================================')
                cat('\n')
                cat('\n')
                print(base2)
        }

        if(intentos == 1){
                notaF <- round((base2[nrow(base2),1]/cant_ej)*100)
        }

        if(intentos > 1){
                intentos <- as.integer(ceiling(intentos))
                detractor <- 0.02
                puntaje <- base2[nrow(base2), 1] - (detractor*(intentos-1))
                puntaje <- ifelse(puntaje < 0, 0, puntaje)
                notaF <- round((puntaje/cant_ej)*100)
        }

        if(isTRUE(enviar)){
                if(any(c(is.null(nombre), is.null(apellido), is.null(mail)))){
                        stop("El argumento 'nombre', 'apellido' y/o 'mail' no deben ser NULL si desea enviar su nota final. Complete esos campos y vuelva a correr la funcion.", call. = FALSE)
                }
                nota <- paste(notaF, "sobre 100")
                nota_escala <- escala(notaF)
                send_IntRo(mail     = mail,
                           nombre   = nombre,
                           apellido = apellido,
                           puntaje  = nota,
                           escala   = nota_escala,
                           modulo   = 4)
        }

}



