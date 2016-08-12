[![Build Status](https://travis-ci.org/Orga-UNQ/qsim.svg?branch=master)](https://travis-ci.org/Orga-UNQ/qsim)

# QSim

QSim es un simulador gráfico de la ejecución de programas en las distintas versiones de la Arquitectura Q.

## Cómo publicar una nueva versión

Está automatizado mediante Travis CI, utilizando el plugin Maven Release. Básicamente lo que hay que hacer es crear un tag y pushearlo a este repositorio, aunque esto está resuelto por el plugin:

```bash
mvn release:clean release:prepare
git push --follow-tags
```

El esquema de versiones sigue la especificación Semantic Versioning 2.0.0, la cual puede conocerse en http://semver.org/.
