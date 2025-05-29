---
title: "Práctica 0"
date: "2025-03-21"
summary: "La práctica aborda el uso de Markdown como herramienta para formatear texto y su aplicación en la documentación técnica. Explica su sintaxis, incluyendo encabezados, listas, enlaces, imágenes y tablas. Además, introduce Git y GitHub, detallando su funcionamiento, comandos esenciales y la gestión de repositorios. Finalmente, se exploran GitHub Actions y Hugo para la automatización del despliegue de sitios web estáticos en GitHub Pages."
---


# Universidad Autónoma de Baja California  
## Facultad de Ingeniería, Arquitectura y Diseño  

### Paradigmas de la programación

### Práctica 0 
#### *Uso de repositorios*

### Arturo Rafael Cornejo Escobar  

### 14 de marzo del 2025  

---

## INTRODUCCIÓN

---
El uso de Markdown está enfocado en que los usuarios puedan escribir texto en una forma sencilla y convertirlo a un formato que permita su visualización en la web.  

Esta herramienta fue desarrollada en el lenguaje Perl, el cual tiene como propósito la manipulación de cadenas de caracteres, archivos y procesos. Por lo que, tiene una eficiencia alta en la administración de sistemas y en el desarrollo web.  

-----

## DESARROLLO  

---

### MARKDOWN  

---
#### ¿Qué es?  

Markdown es tanto un formato de texto como un lenguaje, creado en 2004 por John Gruber. Su propósito es servir como una herramienta para escribir texto de manera legible en un archivo de texto plano y poder convertirlo a un formato HTML para que un navegador web pueda interpretarlo. 

Los archivos de texto plano solo contienen texto puro, es decir, no tienen formato visible y son altamente compatibles, lo que facilita su conversión a otros formatos.  

Se considera un lenguaje porque permite usar símbolos para indicar cómo lucirá el texto al ser convertido a otro formato, es decir, utiliza una sintaxis específica para definir su estructura y estilo.  

---
#### ¿Cómo se utiliza?  

Al ser un formato altamente compatible y con una sintaxis sencilla, Markdown se convierte en una herramienta versátil para escribir libros, notas, sitios web o documentación técnica. Como es un formato de texto plano, tiene longevidad y puede ser utilizado en cualquier dispositivo o sistema operativo.  

- **Sitios Web**  

  >Markdown fue diseñado para la creación de contenido en sitios web, ya que su sintaxis es más legible que la del lenguaje HTML.  

- **Documentos**  

  >Aunque no es tan especializado como otros procesadores de texto, es suficientemente bueno para crear documentos básicos, ya que usa una sintaxis de signos para denotar formato.  

- **Libros**  

  >Escribir un libro en formato Markdown facilita su conversión a un libro electrónico, ya que permite exportarlo a diversos formatos automáticamente. También es posible convertirlo a un formato de impresión.  
---
### Sintaxis  

La mayoria de las aplicaciones de Markdown, utilizan la sintaxis basica que fue creada por John Gruber en el documento original.

##### Encabezado

Para crear un encabezado, se debe de agregar un número de signos numeral `#` delante del texto, el número de signos es acorde al nivel de encabezado.
```txt
# Encabezado de nivel 1 
## Encabezado de nivel 2 
### Encabezado de nivel 3 
```

En **HTML** se vería asi:
```txt
<h1>Encabezado de nivel 1</h1>
<h2>Encabezado de nivel 2</h2>
<h3>Encabezado de nivel 3</h3>
```

Existe una alternativa para poner encabezados añadiendo cualquier número de `==` para denotar un encabezado de nivel 1,  mientras que para el encabezado de nivel 2, se utiliza el `--`.

```txt
Encabezado de nivel 1
=====================
Encabezado de nivel 2
---------------------
```

##### Párrafo
Para crear un parráfo, se usa una linea blanca para separar las lineas de texto.
```txt
Parráfo 1
Parráfo 1

Parráfo 2
Parráfo 2
```

##### Saltos de linea
Para indicar un salto de linea, se indica con la tecla `return` después de dos espacios vacios. 
```txt
Esta es la primera linea.  
Esta es la segunda linea.
```
##### Negritas
En el caso de las negritas, estás son marcadas al poner dos asteriscos o guiónes bajos detras de la palabra. 
```txt
Palabra en **negritas**
Palabra en __negritas__
```
##### Cursiva
Ahora, para darle forma cursiva a la pabalabra, es usado un asterisco o un guión bajo.
```txt
Palabra en *cursiva*
Palabra en _cursiva_
```
##### Bloque de cita
Para hacer este bloque, se pone un signo `>`.
```txt
> Parráfo
```
Este bloque puede tener varios parráfos, en ese caso se deja un `>` entre ellos.
```txt
> Parráfo 1
>
> Parráfo 2
```
En el caso que se quiera anidar a otro nivel, se agrega otro `>` correspondiente al nivel de anidado.
```txt
> Parráfo 
>
>> Parráfo anidado
```
##### Lista
Esta permitido hacer listas, tanto ordenadas como desordenadas
- **Lista ordenada**

Se realizan numerando los elementos con un número seguido de un punto. Lo unico necesario es qué la lista empiece en `1`.
```txt
1. Elemento 1
2. Elemento 2
3. Elemento 3
```
- **Lista desordenada**

Se crean usando guiones, asteriscos o signos 'más' antes de cada elemento.
```markdown
- Elemento guión
* Elemento astérisco
+ Elemento más
```
Al utilizar listas, también podemos anidarlas, solamente indentando los elementos que se quieran anidar.
```markdown
1. Elemento 1
    1. Elemento anidado 1
    2. Elemento anidado 2
2. Elemento 2
3. Elemento 3
```
##### Bloque de código

En este procesador de texto, cuando se quiere diferenciar una parte de código del resto del texto, es correcto utilizar acentos graves ``. 

Para indicar una linea solo se pone dentro de un par de acentos graves, pero cuando es un fragmento más grande, se encierra en dos lineas de 3 acentos graves. Si se quiere denotar un color de sintaxis correspondiente al lenguaje, se etiqueta al lado de la primera linea.

```markdown
```html
<html>
    <head>
        <title>Título del sitio Web</title>
    </head>
    <body>
    </body>
</html>
```\
```
Se vería así.
```html
<html>
    <head>
        <title>Título del sitio Web</title>
    </head>
    <body>
    </body>
</html>
```

##### Linea horizontal
Estás son utilizadas para separar en partes en un segmento o el documento. Para indicar una linea horizontal hay 3 formas:
1. Asteriscos
2. Guión
3. Guión bajo

Cualquiera de los posibles, tienen que estar puestos tres en una linea.
```markdown
---
___
***
```

##### Enlace (Link)
En este procesador de texto, para generar un enlace se pone el titulo del link entre corchetes [Space Jam], continuando con el link entre parentésis (www.spacejam.com/1996/), que resultaría en esto [Space Jam](www.spacejam.com/1996/).
```markdown
[Space Jam](www.spacejam.com/1996/)
```
Tambíen se puede agregar titulos adicionales que se mostraran al pasar el mouse sobre el link, de esta manera [Space Jam](www.spacejam.com/1996/ "Sitio web de la pelicula Space Jam").
```markdown
[Space Jam](www.spacejam.com/1996/ "Sitio web de la pelicula Space Jam")
```
Al envolver un correo electronico, podemos convertirlo en un link automaticamente <email@falso.com>.
```markdown
<email@falso.com>
```
Es de buena practica, referenciar enlaces para mantener un texto facil de leer. Esto es hecho, asignando a una etiqueta el enlace junto con el titulo opcional, así solo usamos como referencia la etiqueta que contiene el enlace.
```markdown
[Space Jam][1]

[1]: www.spacejam.com/1996/ "Sitio web de la película Space Jam"
```
[Space Jam][1]

[1]: www.spacejam.com/1996/ "Sitio web de la película Space Jam"

##### Imagen
Para agregar una imagén, se ingresa un signo de exlamación, seguido del texto alternativo que se mostrara en caso de que la imagen no sea cargada correctamente encerrado en corchetes y ahora sí, la ruta de la imagen si esta en el ordenador o el URL si es el caso entre parentesís. Es posible agregar un titulo adicionar que aparecera al pasar el cursor añadiendolo al parentesis de la URL entre comillas despues del link.
```markdown
![Michael Jordan con Bugs Bunny](https://www.spacejam.com/1996/cmp/jamcentral/img/photos/liveanim5.jpg "Ser o no ser")
```
![Michael Jordan con Bugs Bunny](https://www.spacejam.com/1996/cmp/jamcentral/img/photos/liveanim5.jpg "Ser o no ser")


##### Tabla
Son hechas, primero por el encabezado de la columna, es realizado agregando un guión o más para indicarlo `---`, después se pueden separar cuantas columnas quiera con una barra vertical en los finales de ella `|`.
```markdown
| Año  | MVP              | Puntos |
| :--: | :--------------: | :----: |
| 2024 | Nikola Jokić     | 26.4   |
| 2023 | Joel Embiid      | 33.1   |
| 2022 | Nikola Jokić     | 27.1   |
| 2021 | Nikola Jokić     | 26.4   |
| 2020 | Giannis Antetokounmpo | 29.5   |
| 2019 | Giannis Antetokounmpo | 27.7   |
| 2018 | James Harden     | 30.4   |
| 2017 | Russell Westbrook| 31.6   |
| 2016 | Stephen Curry    | 30.1   |
| 2015 | Stephen Curry    | 23.8   |
```
Con el simbolo del semicolón `:`, se puede indicar la posición del texto poniendo según la posición en los finales de los guiones, si es centrado, entonces tiene que estar en los dos finales.
En la web es mostrada así:
| Año  | MVP              | Puntos |
| :--: | :--------------: | :----: |
| 2024 | Nikola Jokić     | 26.4   |
| 2023 | Joel Embiid      | 33.1   |
| 2022 | Nikola Jokić     | 27.1   |
| 2021 | Nikola Jokić     | 26.4   |
| 2020 | Giannis Antetokounmpo | 29.5   |
| 2019 | Giannis Antetokounmpo | 27.7   |
| 2018 | James Harden     | 30.4   |
| 2017 | Russell Westbrook| 31.6   |
| 2016 | Stephen Curry    | 30.1   |
| 2015 | Stephen Curry    | 23.8   |

---
## Git y Github
---
#
### GitHub
GitHub es una plataforma en la nube diseñada para guardar, compartir y colaborar en proyectos de programación. Su interfaz y herramientas permiten un trabajo eficiente y organizado en equipo.

Muchos desarrolladores prefieren utilizar GitHub porque:
- Es muy fácil subir y compartir su trabajo con otros.
- Permite seguir y organizar los cambios realizados en el código de manera eficiente.
- Ofrece la posibilidad de recibir comentarios y consejos de una comunidad amplia.

GitHub está basado en **Git**, un software de código abierto que le permite gestionar el control de versiones.

### Git
Git es un sistema de control de versiones que registra los cambios realizados en archivos. Esto es muy útil para diagnosticar errores o volver a versiones anteriores si es necesario. Con Git, los desarrolladores pueden:
- Ver qué cambios se realizaron, quién los hizo y cuándo se hicieron.
- Colaborar de manera eficiente, manteniendo un historial unificado y actualizado del proyecto.
- Evitar conflictos al alinear el trabajo en progreso.

Una de las características más importantes de **Git** son las **ramas**, las cuales se utilizan dentro de un **repositorio**.

##### Repositorio
Un repositorio es un almacén que contiene todos los archivos y carpetas de un proyecto, junto con un registro detallado de los cambios realizados. Este registro incluye:
- **Confirmaciones**: Fotos instantáneas de los cambios realizados a lo largo del tiempo. Cada confirmación guarda una versión específica del código e incluye detalles como el propósito de los cambios.

##### Ramas
Las ramas son líneas de desarrollo independientes dentro de un repositorio. Por ejemplo:
- Puedes tener una rama principal llamada **main**, que contiene la versión estable del proyecto.
- Puedes crear una nueva rama para trabajar en una nueva funcionalidad o corrección sin afectar el código principal.

Cuando los cambios realizados en una rama son revisados y aprobados, se pueden fusionar con la rama principal. Esto asegura que los cambios sean integrados de manera controlada, y si surge algún problema, siempre puedes regresar a una versión anterior.

#### Funcionamiento
GitHub funciona como la nube donde se guardan los repositorios de Git, también brinda a los desarrolladores instrumentos para crear un código mejor por medio de una línea de comandos propia de git, accesibilidad de proposición en la mejora del sistema, solicitud de cambios realizados en código lo que proporciona un mejor manejo de los proyectos, manejo o seguimiento del código.

Desde la forma en que se organizan los repositorios donde los usuarios pueden proyectar los requerimientos de los miembros del equipo hasta la tienda de aplicaciones que ofrece una variedad de herramientas para mejorar el flujo de trabajo, **GitHub** este enfocado para facilitar el desarrollo en colaboración.

#### Comandos esenciales
Para usar **Git**, los desarrolladores tienen una linea de comandos que sirven para copiar, crear, cambiar y fusionar el código. Estos comandos pueden ser ejecutados desde una línea de comandos o usando la aplicación de GitHub.

- **`git init`**  
 Inicia un nuevo repositorio de Git y empieza el seguimiento del direcorio creado. Esto lo hace creando una carpeta invisible dentro del directorio donde se esta realizando el trabajo, esta carpeta tiene los archivos con los datos para que Git lleve a cabo el control de las versiones.

- **`git clone`**  
  Crea una copia local de un proyecto remoto, incluyendo todos los archivos, historial y ramas.

- **`git add`**  
  Almacena provisionalmente los cambios hechos en el código. Es el primer paso para incluir modificaciones en el historial del proyecto como prueba.

- **`git commit`**  
  Guarda una instantánea del historial del proyecto, registrando todos los cambios almacenados previamente con `git add` como una confirmación.

- **`git status`**  
  Muestra el estado actual de los cambios en el proyecto como sin seguimiento, modificados o almacenados provisionalmente.

- **`git branch`**  
  Lista de las ramas locales que se estan trabajando en el repositorio.

- **`git merge`**  
  Combina los cambios de dos ramas distintas. Es útil para fucionar cambios de una rama con otra funcionalidad a la rama principal.

- **`git pull`**  
  Actualiza la rama local con los últimos cambios realizados en el repositorio remoto.

- **`git push`**  
  Sube las confirmaciones locales realizadas al repositorio remoto para compartir los cambios con otros.

---
## GitHub Actions y Hugo
---
#
### GitHub Actions
Es una herramiente para determinar procesos en el desarrollo de software y automatizarlos para su uso. Su objetivo es hacer más facil los procesos de:
- **Integración continua** la cual lleva a cabo el combinar los cambios hechos por diferentes desarrolladores en un proyecto, con esta herramienta puedes automatizar las pruebas que revisan el funcionamiento del código propuesto.
- **Despliegue continuo** donde una vez que el código pasa las pruebas, es enviado al entorno de trabajo principal donde se prondra en funcionamiento, algo como una aplicación en la nube.

Estos son llamados **flujos de trabajo**, estos estan definidos en un archivo **YAML** que es verificado en el repositorio y sera ejecutado cuando un evecto active el flujo de trabajo.
### Hugo
Este es un generador de sitios web estáticos y es de código abierto. Este genera contenido HTML a traves de archivos Markdown y lo renderiza en un sitio web y esto lo hace cuando se hace o modifica el contenido.

---
#### Generar un sitio estáctico
> **Notas:** 
Es necesario tener instalado Hugo y Git previamiente
Es recomendable correr los comandos en una terminar Linus como Git Bash

#
```bash
hugo new site quickstart
```
Esto crea una directorio con estructura para tu proyecto, -quickstart- es el nombre que se le dara al proyecto, este puede ser cambiado pero en este caso trabajaremos con ese nombre.
```bash
cd quickstart
```
El comando `cd` sirve para cambiar el directorio actual y lo usamos para cambiarlos a donde esta la raiz de nuestro proyecto con el nombre de quickstart.
```bash
git init
```
Con este comando, creamos una subcarpeta oculta dentro de nuestro proyecto que contiene la información del repositorio Git.
```bash
git submodule add https://github.com/theNewDynamic/gohugo-theme-ananke.git themes/ananke
```
El tema o plantilla **Ananke** es un tema predeterminado que es usado con frecuencia en Hugo por su facilidad de configurar gracias a las opciones que brinda el archivo en `config.toml`.

Este comando clonara esta plantilla en tu proyecto y lo grabara como un submodulo en tu repositorio Git

```bash
echo "theme = 'ananke'" >> hugo.toml
```
Imprime el texto en la configuración del sitio para inicializarlo en el tema actual que es Ananke.
```bash
hugo server
```
Inicia el serve de desarrollo de Hugo donde se puede visualizar el sitio estático.

---
#### Subir la información a Github
Una vez que queramos subir el sitio a la nube, en este caso Github haremos la siguiente serie de pasos.
```bash
hugo
```
Con este comando, compilamos el sitio estático en una carpeta llamada `public/`
```bash
git add .
```
Agregara los archivos cambiados del directorio del proyecto al área de preparación.
```bash
git commit -m "Proyecto_Funcion1"
```
Crea un nuevo commit que es la confirmación de los cambios hechos, esto crea un punto de guardado en forma de una captura instantanéa con los cambios realizados y demás información bajo una etiqueta que se indica en el texto entre comillas.
> Ya que los repositorios creados por GitHub crean por defento la rama main el siguiente paso es necesario.

```bash
git branch -M main
```
Renombra la rama actual de tu directorio de trabajo a main.
```bash
git remote add origin https://github.com/usuario-falso/repositorio-prueba.git
```
Vincula el repositorio del proyecto con un repositorio remoto en GitHub, el cual es el repositorio principal de la nube.
```bash
git push -u origin main
```
Sube los cambios confirmados del commit del repositorio local al vinculado remotamente en GitHub. Indicamos en el comando `-u` para configurar por defento la rama main.

---
#### GitHub Actions para publicar el sitio en GitHub Pages

> Primero es necesario configurar en la ruta del repositorio donde queremos publicar el sitio, con la dirección de nuestro sitio además de habilitar GitHub Actions. También recuerda seguir con la dirección de tu proyecto en la terminal.

```bash
mkdir -p .github/workflows
```
Creara la carpeta del flujo de trabajo.

```bash
nano .github/workflows/deploy.yml
```
Establecera un archivo de configuración para GitHub Actions donde detallara como se automatizaran los procesos en el repositorio de GitHub.

En este caso necesitamos definir las tareas que realizaran para desplegar un sitio en GitHub Pages. Existe una pagina de [GitHub](https://github.com/marketplace/actions/deploy-github-pages-site) que te indica como crear el formato de este archivo para subir el sitio.

Lo siguiente que seguiría es subir el archivo al area de preparación y hacer el commit como en la sección anterior, para finalizar, se tiene que realizar el push para subir el archivo al repositorio remoto y GitHub Actions se ejecutara automáticamente para subir el sitio cada vez que se haga un cambio en la rama.

---
**Referencias**

**Acerca de Git - documentación de GitHub.** (s. f.). GitHub Docs. https://docs.github.com/es/get-started/using-git/about-git

**Entender las GitHub Actions - documentación de GitHub.** (s. f.). GitHub Docs. https://docs.github.com/es/actions/about-github-actions/understanding-github-actions

Cone, M. (2020). **The markdown guide.**

---
**Dirección del repositorio:**
https://github.com/roixarturo/portafolio

**Dirección de la página de GitHub:**
https://roixarturo.github.io/portafolio/