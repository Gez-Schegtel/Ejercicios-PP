
"Busque un elemento en un arreglo y retorne la posición. Lo debe realizar utilizando el concepto de bloque con dos parámetros de entrada"

|n1 bloque petSociety|

n1:= UIManager default request: 'Ingrese un número que desee hallar: '.
(n1 = nil) ifTrue: [ ^nil ].
n1:= n1 asInteger.
petSociety:= #(2 3 4 5 7 4).

bloque:=[:arr :elle | 1 to: arr size do:[:i| ((arr at:i) = elle) ifTrue:[^i]]. ^false].

^bloque value: petSociety value: n1.