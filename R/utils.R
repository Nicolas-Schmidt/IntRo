

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
        paste0('vec', 1:12),
        "creds_file"
)

if(getRversion() >= "2.15.1"){
        utils::globalVariables(c('.', vars))
        utils::suppressForeignCheck(c('.', vars))
}

version <- function(){
        suppressMessages(bad <- badger::badge_devel("Nicolas-Schmidt/IntRo", "blue"))
        unlist(strsplit(bad, "-"))[2]
}

send_IntRo <- function(mail,
                       nombre,
                       apellido,
                       puntaje,
                       escala,
                       modulo){

        nombre   <- tools::toTitleCase(tolower(nombre))
        apellido <- tools::toTitleCase(tolower(apellido))
        fecha    <- blastula::add_readable_time()
        imagen   <- blastula::add_image(system.file("img", "logo.png", package = "IntRo"), width = 80)
        email    <- blastula::compose_email(body = blastula::md(glue::glue(

        "Hola {nombre} {apellido}!,

        Su nota final del Ejercicio I (Modulo vectores) es: **{puntaje}**.
        En la escala de notas equivale a un: **{escala}**.

        Recuerde que puede consultar el curso [aqui](https://nicolas-schmidt.github.io/IntRo//index.html)

        {imagen}")), footer = blastula::md(glue::glue("Email enviado el {fecha}.")))
        asunto <- glue::glue("Curso IntRo: Resultado de Ejercicio {modulo} de {nombre} {apellido}.")
        blastula::create_smtp_creds_file(file = "email_creds", user = mail, provider = "gmail", use_ssl = TRUE)
        blastula::smtp_send(
                email = email,
                to = mail,
                from = mail,
                cc = "nicoschlab@gmail.com",
                subject = asunto,
                credentials = creds_file("email_creds")
        )
        invisible(file.remove("email_creds"))
}


