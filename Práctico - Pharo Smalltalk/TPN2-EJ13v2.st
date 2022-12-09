"13.Realizar un algoritmo que lea una serie de números reales y verifique si están ordenados ascendentemente o no, informando por pantalla."

| res input |
res:= UIManager default request: 'Ingrese un número. Presione "Cancel" o "Esc" para
salir.'.
(res = nil) ifTrue: [ ^self error: 'Se ha cancelado la operación antes de ingresar un
dato.' ].
res:= res asNumber.
input:= 0.
[ input ~= nil ] whileTrue: [ input:= UIManager default request: 'Ingrese un número. Presione "Cancel" o "Esc" para salir.'.
    (input ~= nil) ifTrue: [ input:= input asNumber. (res <= input) ifTrue: [ res:= input ] ifFalse: [ ^false ] ]
].
^true.