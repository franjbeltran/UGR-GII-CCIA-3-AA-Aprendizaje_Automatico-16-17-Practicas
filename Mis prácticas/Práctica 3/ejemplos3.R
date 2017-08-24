class(Auto)
head(Auto)
summary(Auto)
dim(Auto)

# Comparar todos los elementos
#pairs(Auto$mpg)

# attach(Auto)
# Permite no tener que escribir siempre Auto delante de una variable de él mismo

# lm
# Si se especifica: lm(mpg ~ weight, .....) solo aprende teniendo en cuenta weight.
# Si se especifica: lm(mpg ~ ., ...) aprende con todos.
# data es el conjunto de datos que se tiene.
# subset es el índice de elementos que va a utilizar para aprender.



# Página 18 -> cambiar donde pone Auto por Auto2 al hacer la partición de train y test.




# AppliedPredictiveModeling
data("segmentationOriginal")
class(segmentationOriginal)
names(segmentationOriginal)[1:10]
summary(segmentationOriginal[1:10])













