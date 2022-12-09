
"Convertir una cadena a mayúsculas, a nivel de caracter. Opción 4."

| cadena |
cadena:= 'a esta cadena la vas a estar viendo en mayúsculas, aunque se escribió en minúsculas.'.
1 to: (cadena size) do: [:i| cadena at:i put: ((cadena at:i) asUppercase) ].
^cadena.