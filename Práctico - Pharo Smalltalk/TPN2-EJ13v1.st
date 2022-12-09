"13.Realizar un algoritmo que lea una serie de números reales y verifique si están ordenados ascendentemente o no, informando por pantalla."

| input lista bandera |
lista:= OrderedCollection new.
input:= 0.
[ input ~= nil ] whileTrue: [ input:= UIManager default request: 'Ingrese un número.
Presione "Cancel" o "Esc" para salir.'. (input ~= nil) ifTrue: [ lista add: input
asNumber ]. ].
1 to: (lista size)-1 do: [:i| ( (lista at:i) <= (lista at:(i+1))) ifTrue: [ ^'El arreglo está ordenado.' ] ifFalse: [ ^'El arreglo no está ordenado.' ] ].