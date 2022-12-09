"3. Verificar si una frase es un palíndromo o no."

"Opción 3, ignorando los espacios, comas y puntos."

| string sally |
string:= 'Lavan,,,,,esa base naval.'.
sally:= ''.
"Recordar!: El 'do:' agarra cada elemento del objeto. En este caso, está agarrando cada caracter del string."
string do: [:i| ( (i ~= ($ )) & (i ~= ($,)) & (i ~= ($.)) ) ifTrue: [ sally:= sally, (i asString asLowercase) ] ].
^sally = sally reversed.