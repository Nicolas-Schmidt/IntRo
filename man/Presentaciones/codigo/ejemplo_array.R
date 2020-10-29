#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curso IntRo
# Ejemplo array 3D
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

datos <- data.frame(Party_A = c(21896, 18481, 24827,  18638, 23269),
                    Party_B = c( 3001,  1283,  1283, 128300, 12830),
                    Party_C = c( 3524,  1209,  1209, 120550, 12090 ))

rownames(datos) <- paste("District", LETTERS[1:5], sep = "_")

M <- 10
divisor <- function(x){
                for (i in 1:M) {x[i] <- x[1] / i}
                round(x,2)
        }
dh <- apply(datos, c(2,1), divisor)
dh

dim(dh)
