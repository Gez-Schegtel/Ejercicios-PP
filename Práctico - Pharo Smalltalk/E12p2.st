
"
1° Parcial Práctico.

Punto 2:

A partir de un texto de entrada arbitrario realizar el cifrado César, consistente
en desplazar una determinada cantidad de caracteres (también arbitrarios).
Ingresar el texto y el desplazamiento a través del UIManager.
Por ejemplo, con un desplazamiento de 3 la letra ‘A’ sería sustituída por la ‘D’.
Aclaración: El profesor indicó que si ocurre un desbordamiento, por ejemplo,
que se tenga que cifrar la letra ‘Z’ con un desplazamiento de 3, no se haga
nada. Por lo que no es necesario en el parcial que sea “cíclico” el cifrado.
"

| texto des |
texto:= UIManager default request: 'Ingrese el texto que desee cifrar: '.
(texto = nil) ifTrue: [ ^nil ].
texto:= texto asLowercase.
des:= UIManager default request: 'Ingrese el desplazamiento con el que desee realizar el cifrado: '.
(des = nil) ifTrue: [ ^nil ].
des:= des asInteger.
1 to: (texto size) do: [:i| texto at: i put: (((texto at: i) asciiValue + des) asCharacter) ].
^texto.