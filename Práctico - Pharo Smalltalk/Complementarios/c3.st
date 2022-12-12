
"Dada una lista de números, previamente ingresada por el usuario, determinar el menor y el mayor número de la lista."

|n1 cont lista bandera|

lista:= SortedCollection new.
bandera:= true.
[bandera = true] whileTrue:[n1:= (UIManager default request: 'Ingresa un valor: '). (n1 = 'a') ifTrue:[bandera:=false] ifFalse:[lista add: n1 asNumber]].

^(lista at: 1) asString ,String cr,  (lista at: lista size) asString.