"3. Verificar si una frase es un palíndromo o no."

"Opción 1, utilizando los mensajes to: do:" 

| johnMcLane glados |
johnMcLane:= 'Lavan esa base naval'.
glados:= ''.
1 to: (johnMcLane size) do: [:i| ((johnMcLane at:i) ~= ($ )) ifTrue: [ glados:= glados, ((johnMcLane at:i) asString asLowercase) ] ].
^glados = glados reversed.