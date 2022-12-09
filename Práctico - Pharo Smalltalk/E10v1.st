
"Cálculo de PI con la fórmula de Leibniz:"
"Opción Agustinosa:"

|pi|
pi:=0.
0 to: 99 do: [:n| pi:=(pi + (((-1) ** n)/((2 * n) + 1)))].
pi