---
title: "Práctica 2"
date: "2025-05-31"
summary: "La práctica se trata de identificar los elementos esenciales de los lenguajes. Se analizan y ejemplifican: nombres, alcance (global, local, anidado), marcos de activación, tipos de datos (básicos y personalizados), administración de memoria (pila, montón, estática), expresiones, comandos y control de secuencia (selección, iteración, recursión), ilustrados con un programa en C de gestión de biblioteca."
---

  
  

# Universidad Autónoma de Baja California

## Facultad de Ingeniería, Arquitectura y Diseño

  

### Paradigmas de la programación

  

### Práctica 2

#### *Elementos básicos de los lenguajes de programación*

  

### Arturo Rafael Cornejo Escobar

  

### 31 de abril del 2025

  

___
## INTRODUCCIÓN

En el ámbito de la programación, los **nombres** son elementos fundamentales para la abstracción. Estos permiten hacer referencia a cualquier entidad en diversos contextos del código y una entidad puede contar con varios nombres que la referencian.

Cuando se hace mención del contexto del código, es debido que a que una característica esencial de un nombre es que debe ser limitado a una región de **alcance**.

 El alcance es una característica que define el contexto del que un nombre referencia, es decir, el lugar donde se buscara ese nombre. 

Los **bloques** son partes del código que organizan nombres con un alcance local y estos corresponden a un marco en el entorno. En la programación, es usual la existencia de bloques de función y los bloques anidados donde los nombres dentro de estos bloques solo son visibles dentro de ellos.

Un nombre está **sobrecargado** si refiere a múltiples entidades en el mismo alcance. Algunos lenguajes resuelven esto basándose en cómo se usa el nombre.

Los **marcos de activación** son espacios de memoria temporales que se crean cada vez que se llama a una función.

Contienen la información esencial para que esa función se ejecute correctamente, como:

-   **Parámetros:** Los valores que se le pasaron.
-   **Variables locales:** Las variables que la función declara para su uso interno.
-   **Dirección de retorno:** Dónde debe continuar el programa cuando la función termina.

Se apilan uno encima del otro en la **pila de llamadas** a medida que se invocan funciones, y se eliminan cuando cada función finaliza, liberando esa memoria. Esto permite que las funciones se ejecuten de forma independiente

El objetivo de esta práctica es identificar los **elementos esenciales de los lenguajes de programación**, incluyendo: **nombres**, **marcos de activación**, **bloques de alcance**, **administración de memoria**, **expresiones**, **comandos**, **control de secuencia** (selección, iteración y recursión), **subprogramas** y **tipos de datos**.

-----

  

## DESARROLLO

 

  

### Nombres