"4. Contar la cantidad de vocales de una frase."
"Opción 1, con el mensaje ‘do:’:"

| input cont |
input:= UIManager default request: 'Ingrese una frase para analizar la cantidad de vocales que posee: '.
(input = nil) ifTrue: [ ^nil ].
cont:= 0.
input do: [:each| (each isVowel) ifTrue: [ cont:= cont + 1 ] ].
^cont.