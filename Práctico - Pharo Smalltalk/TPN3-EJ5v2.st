"5. Ingresar dos cadenas y devolver una tercera que contenga los elementos de las dos anteriores pero intercalados."

" Opción 1, a nivel de string terminando la operación si una frase termina. Forma 2: "

| frase1 frase2 sally men |
frase1:= 'La Universidad Tecnológica Nacional' substrings.
frase2:= 'Segundo año' substrings.
sally:= ''.
(frase1 size <= frase2 size) ifTrue: [ men:= frase1 size ] ifFalse: [ men:= frase2 size ].
1 to: men do: [:i| sally:= sally, (frase1 at:i), ' ', (frase2 at:i), ' ' ].
^sally.
"'La Segundo Universidad año '"