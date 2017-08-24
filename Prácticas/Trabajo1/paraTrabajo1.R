## Practicas de Aprendizaje Automático
## Curso 2016/2017
## Departamento de CCIA
## Autora: S. Acid

## correo: acid@decsai.ugr.es
## ------------------------------------------------------------------------

## El código aquí presente pretende servir de ayuda para el desarrollo de los
## de los ejercicios del Trabajo1 de prácticas. 
## Su utilización es discresional y pueden hacerse modificaciones de las funciones 
## puestas a vuestra disposición.

setwd("./datos")
set.seed(3)

## ------------------------------------------------------------------------
# por defecto genera 2 puntos entre [0,1] de 2 dimensiones 

simula_unif = function (N=2,dims=2, rango = c(0,1)){
 m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
 nrow = N, ncol=dims, byrow=T)
 m
}

## -----------------------------------------------------------------------

# función simula_gaus(N, dim, sigma) que genera un
# conjunto de longitud N de vectores de dimensión dim, conteniendo números 
# aleatorios gaussianos de media 0 y varianzas dadas por el vector sigma.
# por defecto genera 2 puntos de 2 dimensiones 

simula_gaus = function(N=2,dim=2,sigma){

  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generación se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensión")
  
  simula_gauss1 = function() rnorm(dim, sd = sigma) # genera 1 muestra, con las desviaciones especificadas
  m = t(replicate(N,simula_gauss1())) # repite N veces, simula_gauss1 y se hace la traspuesta
  m
}

## ------------------------------------------------------------------------
#  simula_recta(intervalo) una funcion que calcula los parámetros
#  de una recta aleatoria, y = ax + b, que corte al cuadrado [-50,50]x[-50,50]
#  (Para calcular la recta se simulan las coordenadas de 2 ptos dentro del 
#  cuadrado y se calcula la recta que pasa por ellos), 
#  se pinta o no segun el valor de parametro visible

# Devuelve dos parámetros: a-> pendiente

simula_recta = function (intervalo = c(-1,1), visible=F){
  
   ptos = simula_unif(2,2,intervalo) # se generan 2 puntos
   a = (ptos[1,2] - ptos[2,2]) / (ptos[1,1]-ptos[2,1]) # calculo de la pendiente
   b = ptos[1,2]-a*ptos[1,1]  # calculo del punto de corte

   if (visible) {  # pinta la recta y los 2 puntos
       if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
           plot(1, type="n", xlim=intervalo, ylim=intervalo)
       points(ptos,col=3)  #pinta en verde los puntos
       abline(b,a,col=3)   # y la recta
   }
   c(a,b) # devuelve el par pendiente y punto de corte
}

# Para el apartado 3 del Ejercicio 1 
#-------------------------------------------------------------------------------
## funcion para pintar la frontera de la función
# a la que se pueden añadir puntos, y etiquetas

pintar_frontera = function(f,rango=c(-50,50)) {
   x=y=seq(rango[1],rango[2],length.out = 100)
   z = outer(x,y,FUN=f)
  if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
          plot(1, type="n", xlim=rango, ylim=rango)
   contour(x,y,z, levels = 0, drawlabels = FALSE,xlim =rango, ylim=rango, xlab = "x", ylab = "y")
}

# Ejemplo de llamada a una funcion f1_xy que hemos de definir
# pintar_frontera(f1_xy) 


## Para el ejercicio 3.
## lectura de los Digitos
## warning la última línea, valor 0 para la clase, está incompleta

# ------------------------------------------------------------------------
digit.train <- read.table("zip.train",
                          quote="\"", comment.char="", stringsAsFactors=FALSE)

digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]
digitos = digitos15.train[,1]  # etiquetas
ndigitos = nrow(digitos15.train)

# se retira la clase y se monta una matriz 3D: 599*16*16
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos,16,16))
rm(digit.train) 
rm(digitos15.train)

# Para visualizar los 4 primeros
## ------------------------------------------------------------------------

par(mfrow=c(2,2)) 
for(i in 1:4){
  imagen = grises[i,,16:1] # se rota para verlo bien
  image(z=imagen)
}

digitos[1:4] # etiquetas correspondientes a las 4 imágenes


## ------------------------------------------------------------------------
fsimetria <- function(A){
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}

## ------------------------------------------------------------------------


 
