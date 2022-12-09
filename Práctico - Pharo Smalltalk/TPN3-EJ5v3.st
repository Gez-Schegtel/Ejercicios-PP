"5. Ingresar dos cadenas y devolver una tercera que contenga los elementos de las dos anteriores pero intercalados."

" Opción 2, a nivel de string siguiendo la operación por más que una frase haya terminado: "

| frase1 frase2 fraseMayor fraseMenor sally |
frase1:= 'La Universidad Tecnológica Nacional' substrings.
frase2:= 'Segundo año' substrings.
sally:= ''.
(frase1 size >= frase2 size) ifTrue: [ fraseMayor:= frase1. fraseMenor:= frase2 ]
ifFalse: [ fraseMayor:= frase2. fraseMenor:= frase1 ].
1 to: (fraseMenor size) do: [:i| sally:= sally, (fraseMayor at:i), ' ', (fraseMenor at:i), ' ' ].
(fraseMayor size > fraseMenor size) ifTrue: [ ((fraseMenor size) + 1) to: (fraseMayor size) do: [:i| sally:= sally, (fraseMayor at:i), ' ' ] ].
^sally.
"'La Segundo Universidad año Tecnológica Nacional '"