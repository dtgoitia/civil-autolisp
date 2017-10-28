# Engineering Cheatsheet

## Load

Once `_PersonalLibrary.lsp` loaded, use `c:EngSet` to load all the shortcuts described below.

## Setup

* `c:11` and `c:22`: calc max level from FFL for storm and foul.

* `c:1` set _ByLayer_

* `c:2` interpolate

* `c:3` extrapolation for private drainage \*

* `c:33` extrapolation (generic, with _PI_DT_ block insertion)

* `c:OO` to change OSMODE to 4.

* `c:RR` to rotate and move

* `c:pa` to insert _Parking-Fall-Arrow_ block

* `c:ra` to insert _Road-Fall-Arrow_ block

(*) Levels rounded to 2nd decimal, gradient recalculated after this rounding, and manhole and sewer label info updated


## Posible improvements

### IDEA 1: **DESESTIMADA** labels gripped after `c:3`
**idea**: con el c:3 insertar el gradiente en el texto del tirón en vez de copiarlo en el clipboard con el c:3 dejar gripadas (seleccionadas) los textos modificados par que si quieres ponerlos _ByLayer_ sólo tengas que hacer c:1.

**motivo de destimación**: si tienes que calcular varias tuberías que entran a una misma arqueta es un peñazo andar dándole al Esc cada vez que quieres hacer una comprobación, no creo que sea práctico. Es mejor ponerlas _ByLayer_ manualmente.


## Engineering process

Se supone que la drainage strategy está ya hecha (pipes sketched on plan). Así es como lo hace Wayne:

  1. Obtener los Cover Levels:
    1. Obtener 3dpolys para las centerline.
    2. Hacer 3Doffsets según sea camber o crossfall (no hace falta que sea hasta la channel line, con que cubra la arqueta basta)
    3. Trimear las 3dpolys en las intersecciones para que la triangulación sea correcta, pero no hagas 3Djunctions, solo que cubra las arquetas...
    4. Generar un 3Dmodel temporal
    5. Interpolar con KTF los puntos en el centro de cada arqueta para obtener los niveles
    6. Meter los CL en las arquetas
  2. Dar nombre a todas las arquetas con el criterio de MicroDrainage: empezar por el punto más alto de las ramas, y cuando llegas al punto donde se unen las ramas, no etiquetar la arqueta, sino que continuas por el punto más alto de la otra rama que vierte a la arqueta no vas a numerar. Solo se numbera la arqueta donde se unen las ramas cuando todas las ramas que vierten a esa arqueta ya están numeradas.
  3. Empezar a dar niveles por los puntos más altos de las ramas, manteniéndote siempre 1.25m por debajo de la superficie de la carretera (para eso querías los Cover Level) o más abajo si la pendiente del drenaje así lo marca.

Y listo.
Con esto ya se le puede pasar el diseño al departamento de drenajes para que lo metan a MicroDrainage y te digan los tamaños de tubería.
Y tú podrías seguir haciendo los cálculos de drenajes privados.
Además, de esta forma podrías empezar a paralelizar procesos (delegando) como poner los RWP en las casas, poner los manholes en los perfiles, etc.
