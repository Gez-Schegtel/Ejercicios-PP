
"
1° Parcial Práctico.

Punto 1: 

Realizar la división entre dos números enteros utilizando restas hasta el primer
dígito decimal, si es necesario. Tener en cuenta que tanto el dividendo como el
divisor pueder ser positivos o negativos. Ingresar la información a través del
UIManager.
Devolver un string con el formato ‘eee, d’, siendo ‘e’ la parte entera y ‘d’ los
decimales.
"

| n1 n2 aux1 aux2 intResult decResult |
n1:= UIManager default request: 'Ingrese el numerador: '.
(n1 = nil) ifTrue: [ ^nil ].
n1:= n1 asNumber.
n2:= UIManager default request: 'Ingrese el denominador: '.
((n2 = nil) | (n2 = '0')) ifTrue: [ ^nil ].
n2:= n2 asNumber.
aux1:= n1 abs.
aux2:= n2 abs.
intResult:= 0.
[ aux1 >= aux2 ] whileTrue: [ intResult:= intResult + 1. aux1:= (aux1 - aux2) ].
(((n1 >= 0) & (n2 < 0)) | ((n1 <= 0) & (n2 > 0))) ifTrue: [ intResult:= intResult negated
].
decResult:= 0.
(aux1 ~= 0) ifTrue: [ aux1:= aux1 * 10. [ aux1 >= aux2 ] whileTrue: [ decResult:= decResult + 1. aux1:= (aux1 - aux2) ] ].
^intResult asString, ',', decResult asString.
