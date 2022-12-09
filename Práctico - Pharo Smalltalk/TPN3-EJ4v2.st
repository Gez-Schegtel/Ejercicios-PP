"4. Contar la cantidad de vocales de una frase."
"Opción 2, con los mensajes ‘to: do:’:"

| input cont |
input:= UIManager default request: 'Ingrese una frase para analizar la cantidad de vocales que posee: '.
(input = nil) ifTrue: [ ^nil ].
cont:= 0.
1 to: (input size) do: [:i| ((input at:i) isVowel) ifTrue: [ cont:= cont + 1 ] ].
^cont.