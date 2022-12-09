"1. Convertir una cadena a mayúsculas y minúsculas."

"A nivel de caracter, forma 2. "

| cadena1 cadena2 aux1 aux2 |.
cadena1:= 'a esta cadena la vas a estar viendo en mayúsculas, aunque se escribió en minúsculas.'.
cadena2:= 'A ESTA CADENA LA VAS A ESTAR VIENDO EN MINÚSCULAS, AUNQUE SE ESCRIBIÓ EN MAYÚSCULAS.'.
aux1:= ''.
aux2:= ''.
cadena1 do: [:caracter| aux1:= aux1, caracter asUppercase asString ].
cadena2 do: [:caracter| aux2:= aux2, caracter asLowercase asString ].
^aux1, String cr, aux2.