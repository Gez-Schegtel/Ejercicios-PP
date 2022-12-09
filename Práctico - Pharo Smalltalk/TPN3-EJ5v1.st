"5. Ingresar dos cadenas y devolver una tercera que contenga los elementos de las dos anteriores pero intercalados."

"Opción 1, a nivel de string terminando la operación si una frase termina. Forma 1: "

| frase1 frase2 sally iteración |
frase1:= 'La Universidad Tecnológica Nacional' substrings.
frase2:= 'Segundo año' substrings.
iteración:= ((frase1 size) - (frase2 size)) abs.
sally:= ''.
1 to: iteración do: [:i| sally:= sally, (frase1 at:i), ' ', (frase2 at:i), ' ' ].
^sally.
"'La Segundo Universidad año '"