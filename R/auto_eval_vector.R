#' @title Autoevaluacion de 'Ejercicios I' (Modulo Vectores).
#'
#' @description Es una funcion que permite autoevaluar los ejercicios del modulo
#'     sobre vectores del curso Introduccion al software estadistico R.
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
#'     correspondientes al modulo de vectores.
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
#' @examples
#'
#' vec1 <- seq(2, 20, 2)
#'
#' auto_eval_vector(nombre = "Nicolas",
#'                  apellido = "Schmidt",
#'                  mail = NULL,
#'                  enviar = FALSE,
#'                  intentos = 3,
#'                  summary = TRUE)
#' @export

auto_eval_vector <- function(nombre = NULL,
                        apellido = NULL,
                        mail = NULL,
                        intentos = 1,
                        enviar = FALSE,
                        summary = TRUE){
        espacio <- ls(search()[1])
        cant_ej <- 12
        if(length(espacio)==0){
                stop("No hay ningun objeto creado en la sesion de trabajo. Puede consultarlo con la funcion 'objects()' o 'ls()'", call. = FALSE)
                }
        vector_objetos_t1 <- paste0("vec", 1:cant_ej)
        faltantes <- vector_objetos_t1 %in% espacio
        if(sum(faltantes)==0){
                stop("En el aspacio de trabajo no hay ninguno de los objeto del ejercicio creados. Recuerde que debe utilizar los mombres que figuran en los ejercicios", call. = FALSE)
                }

        base <- as.list(stats::setNames(rep(NA, cant_ej), paste("Ejercicio",1:cant_ej)))

        if(vector_objetos_t1[1] %in% espacio){
                if(length(vec1)!=10){
                        warning("Ejercicio 1: El largo (length()) del vector no corresponde con la secuencia solicitada", call. = FALSE)
                }
                if(!all(vec1 %in% seq(2,20,2))){
                        warning("Ejercicio 1: La secuancia numerica contiene errores", call. = FALSE)
                }
                base[[1]] <- ifelse(length(vec1)==10 && all(vec1 == seq(2,20,2)), 1, 0)
        }

        if(vector_objetos_t1[2] %in% espacio){
                if(!is.null(vec2)){
                        warning("Ejercicio 2: El contenido del objeto no es correcto. Asigne la dimension del siguiente modo: 'objeto1 <- dim(objeto2)'", call. = FALSE)
                }
                base[[2]] <- ifelse(is.null(vec2), 1, 0)
        }

        if(vector_objetos_t1[3] %in% espacio){
                if(!all(LETTERS[1:3] == vec3[1:3])){
                        warning("Ejercicio 3: Recuerde que las letras de la secuencia deben estan en mayuscula: use toupper() o LETTERS()", call. = FALSE)
                }
                if(length(vec3)!=45){
                        warning("Ejercicio 3: El largo del vector no es el correcto")
                }
                if(length(vec3)==45){
                        if(!all(vec3==rep(LETTERS[1:3], 15))){
                                warning("Ejercicio 3: La secuecnia no es correcta, debe repetir la secuuenia c('A','B','C') 15 veces)", call. = FALSE)
                        }
                }
                base[[3]] <- ifelse(all(LETTERS[1:3] == vec3[1:3]) && length(vec3) == 45, 1, 0)
        }

        if(vector_objetos_t1[4] %in% espacio){
                if(!is.factor(vec4)){
                        warning("Ejercicio 4: El vector no es un factor, puede chequearlo con la sentencia logica: is.factor()", call. = FALSE)
                }
                if(nlevels(vec4)!=3){
                        warning("Ejercicio 4: Los niveles del factor deben ser 3: c('A','B','C')", call. = FALSE)
                }
                base[[4]] <- ifelse(is.factor(vec4) && nlevels(vec4)==3, 1, 0)
        }

        if(vector_objetos_t1[5] %in% espacio){
                set.seed(2018)
                vec5R <- stats::rnorm(100)
                if(length(vec5)!=100){
                        warning("Ejercicio 5: El largo (length()) del vector no corresponde con la secuencia solicitada", call. = FALSE)
                }
                if(!all(vec5 == vec5R)){
                        warning("Ejercicio 5: La secuencia numerica no es correcta. Recuerde que debe usar una semilla para generar numeros aleatorios: set.seed(2018)", call. = FALSE)
                }
                base[[5]] <- ifelse(length(vec5)==100 && all(vec5 == vec5R), 1, 0)
        }

        if(vector_objetos_t1[6] %in% espacio){

                if(round(vec5R[1],2)==vec6[1]){
                        if(nchar(vec6)[1] > 5){
                                warning("Ejercicio 6: El redondeo no es correcto. Pruebe con la funcion 'round()'", call. = FALSE)
                        }
                        base[[6]] <- ifelse(nchar(vec6)[1] <= 5, 1, 0)
                }else{
                        warning("Ejercicio 6: El contenido del vector no es el del vector 'vec5'.", call. = FALSE)
                        base[[6]] <- 0
                }

                ifelse(nchar(vec6)[1] <= 5, 1, 0)

        }

        if(vector_objetos_t1[7] %in% espacio){
                if(sum(is.na(vec7))<1){
                        warning("Ejercicio 7: En el vector no hay NA", call. = FALSE)
                }
                if(length(stats::na.omit(vec7))!=4){
                        warning("Ejercicio 7: En el vector hay mas numeros de los que el ejercicio pide.", call. = FALSE)
                }
                base[[7]] <- ifelse(sum(is.na(vec7)) >= 1 && length(stats::na.omit(vec7))==4, 1, 0)
        }

        if(vector_objetos_t1[8] %in% espacio){
                if(vec8 != 33.25){
                        warning("Ejercicio 8: El promedio no es correcto. Verifique que el contenido del vector 'vec7' sea correctro o lea la ayuda de la funcion 'mean()' sobre como tratar los NA", call. = FALSE)
                }
                base[[8]] <- ifelse(vec8 == 33.25, 1, 0)
        }

        if(vector_objetos_t1[9] %in% espacio){
                if(length(vec9)!=2){
                        warning("Ejercicio 9: El vector 'vec9' debe tener una longitud de 2: el valor maximo del vector y la posicion de ese valor en el vector", call. = FALSE)
                }
                base[[9]] <- ifelse(length(vec9)==2, 1, 0)
        }

        if(vector_objetos_t1[10] %in% espacio){

                if(sum(is.na(vec10))!=16){
                        warning("Ejercicio 10: La cantidad de NA generados con la funcion 'ifelse()' no es correcta. Recuerde que debe aplicar la funcion sobre el vector aleatorio con semilla.", call. = FALSE)
                        }
                base[[10]] <- ifelse(sum(is.na(vec10))==16, 1, 0)
        }

        if(vector_objetos_t1[11] %in% espacio){
                if(!is.character(vec11)){
                        warning("Ejercicio 11: El vector no es de tipo 'character'. Puede chequearlo con la funcion 'typeof()'", call. = FALSE)
                        }
                if(vec11[1]!="2"){
                        warning("Ejercicio 11: El vector a convertir a 'character' es el vector 'vec1'. Recuerde que si tiene problemas en 'vec1' este ejercicio tendra problemas.", call. = FALSE)
                        }
                base[[11]] <- ifelse(is.character(vec11) && vec11[1]=="2", 1, 0)
        }

        if(vector_objetos_t1[12] %in% espacio){
                if(!is.character(vec12)){
                        warning("Ejercicio 12: El vector debe ser de tipo 'character'. Puede chequearlo con la funcion 'typeof()'", call. = FALSE)
                        }
                if(length(vec12)!=1){
                        warning("Ejercicio 12: El largo del vector no es correcto. El vector 'vec1' tiene 10 elementos y el 'vec12' debe tener 1 solo elemento", call. = FALSE)
                }else{
                        if(substr(vec12,2,3)!="-4"){
                                warning("Ejercicio 12: Recuerde que los numeros deben estar separados por un guion ('-')", call. = FALSE)
                        }
                }
                base[[12]] <- ifelse(length(vec12)==1 && substr(vec12,2,2)=="-", 1, 0)
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
                email <- gmailr::mime(
                        To = mail,
                        Bcc = "nicoschlab@gmail.com",
                        From = mail,
                        Subject = paste("Curso IntRo: Resultado de Ejercicio 1 de", nombre, apellido),
                        body = paste("Su nota final del Ejercicio I (Modulo vectores) es:", nota,". En la escala de notas equivale a un:", nota_escala))
                gmailr::send_message(email)
        }

}


