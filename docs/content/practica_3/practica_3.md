---

title: "Práctica 3"

date: "2025-05-31"

summary: "El objetivo de esta práctica es identificar elementos esenciales de los lenguajes de programación en Python, a diferencia de la práctica anterior que se centró en C. Se analizará cómo Python maneja conceptos como nombres, marcos de activación, bloques de alcance y administración de memoria. Un aspecto clave a destacar es el tipado dinámico de Python y su gestión automática de la memoria, lo cual simplifica la programación en comparación con el control manual que ofrece C."

---

  

  

# Universidad Autónoma de Baja California

  

## Facultad de Ingeniería, Arquitectura y Diseño

  

  

### Paradigmas de la programación

  

  

### Práctica 3

  

#### *Instalación y funcionamiento de Haskell*

  

  

### Arturo Rafael Cornejo Escobar

  

  

### 31 de abril del 2025

  

  

___

## INTRODUCCIÓN

El objetivo de esta práctica es identificar los **elementos esenciales de los lenguajes de programación**, incluyendo: **nombres**, **marcos de activación**, **bloques de alcance**, **administración de memoria**, **expresiones**, **comandos**, **control de secuencia** (selección, iteración y recursión), **subprogramas** y **tipos de datos** en el ámbito de Python a diferencia de la práctica pasada que era de C.

Por lo que es importante marcar las diferencias en los elementos:

Los **nombres** manejan un tipado dinámico, lo que se entiendo como qué el tipo de una variable se es determinado durante el tiempo de la ejecución del programa y no durante la compilación, lo que abre la posibilidad de cambiar el tipo según el dato agregado.

En el caso de los **marcos de activación**, son manejados de forma automática también, esto infiere que cuando se llama a una función o sub programa, Python crea el entorno en la pila para las variables locales y las referencias, además de limpiarlo al finalizar, mientras que en C es un manejo más manual sobre la gestión de la pila.

La primera diferencia que se puede notar, son los **bloques de alcance** ya que, están delimitados por la identación a diferencia de definirlo por llaves ("{ }") y los **comandos** se terminan con una nueva línea y agrupados según la identación.

-----

  

  

## DESARROLLO

En la página oficial de Haskell, nos encontramos con el comando del [instalador](https://www.haskell.org/ghcup/).

```PowerShell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = 
[System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest 
https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -
UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

Después de confirmar los directorios de instalación y la instalación de modulos, se abre una ventana de MSY64 que terminará la descarga de los elementos necesarios.

Una vez terminada la instalación, hacemos verificación de esta, con el comando `gch --version` y regresa:

```PowerShell
The Glorious Glasgow Haskell Compilation System, version 9.6.7
```
Se procede a instalar [Docker](https://www.docker.com/products/docker-desktop/) que es una herramienta que te permite **crear, ejecutar y compartir aplicaciones en contenedores**.

