
"Convertir una cadena a mayúsculas, a nivel de caracter. Opción 3."

| cadena caracter |
cadena:= 'a esta cadena la vas a estar viendo en mayúsculas, aunque se escribió en minúsculas.'.
1 to: (cadena size) do: [:i| caracter:= cadena at:i. cadena at:i put: (caracter asUppercase) ].
^cadena.