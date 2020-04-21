

escala <- function(puntaje){

        if(!is.numeric(puntaje)){
                stop("El puntaje debe ser un numero")
        }
        if(puntaje < 0 | puntaje > 100){
                stop("El puntaje debe ser un numero entre 0 y 100")
        }
        nota <- puntaje
        if (puntaje <  50) nota = 0L
        if (puntaje >= 50 & puntaje < 58  ) nota = 7L
        if (puntaje >= 58 & puntaje < 75  ) nota = 8L
        if (puntaje >= 75 & puntaje < 91  ) nota = 9L
        if (puntaje >= 91 & puntaje <= 100) nota = 10L
        nota
}




vars <- c(
        paste0('df', 1:8),
        paste0('lista', 1:6),
        paste0('mat', 1:7),
        paste0('vec', 1:12)
)

if(getRversion() >= "2.15.1"){
        utils::globalVariables(c('.', vars))
        utils::suppressForeignCheck(c('.', vars))
}























