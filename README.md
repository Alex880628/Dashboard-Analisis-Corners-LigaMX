# 📊 Dashboard Interactivo: Análisis de Tiros de Esquina en la Liga MX (2021-2025)

Este repositorio contiene el código para un dashboard interactivo desarrollado en R Shiny, como parte del Hackathon 2025. El proyecto realiza un análisis profundo de las estrategias y la efectividad de los tiros de esquina en cuatro temporadas de la Liga MX.

### ✨ Visualización en Vivo

**Puedes acceder y utilizar el dashboard interactivo en el siguiente enlace:**

[**https://ahernandezt.shinyapps.io/ligamx-abp/**](https://ahernandezt.shinyapps.io/ligamx-abp/)

---

### 📸 Capturas de Pantalla

<img width="800" alt="Radar Táctico" src="https://github.com/user-attachments/assets/b28a8583-c9e7-4045-8ab5-7242e35c1822" />
_El radar de perfil táctico permite un análisis comparativo del estilo y la eficacia de cada equipo._

<br>

<img width="800" alt="Mapa de Zonas" src="https://github.com/user-attachments/assets/6f168b84-4659-4aec-8ed7-a9a5be663078" />
_Los mapas de zonas revelan las áreas de destino preferidas para los centros de esquina de cada club._

<br>

<img width="800" alt="Gráfico de Dispersión" src="https://github.com/user-attachments/assets/cb631195-0942-4601-a6ce-9a85f82af957" />
_El gráfico de dispersión clasifica a los equipos según su volumen de ocasiones a balón parado y su dependencia en ellas._

---

### 🧠 Análisis y Hallazgos Clave

#### Sección 1-3: Análisis de Ocupación del Área Chica

- **Barras: Remates en córners:** Muchos córners comienzan con 0 jugadores plantados en área chica y con llegadas en carrera desde área grande/segundo palo/borde del área. La categoría "0 Atacantes" no significa que no haya nadie atacando, sino que el movimiento se inicia desde fuera para atacar el balón en zonas estratégicas como el primer poste, el punto de penalti o el segundo poste.
- **Remates con tooltips:** Los gráficos interactivos confirman el patrón: el mayor volumen de remates se produce cuando los atacantes no están fijados en el área chica. Se observa que con ocupaciones altas no mejora el xGoT/tiro (e incluso cae), lo que subraya que **volumen ≠ calidad**.
- **Goles con tooltips:** Los goles también siguen este patrón. La configuración de "0 Atacantes" es la que más goles produce en cifras absolutas, y las trayectorias de gol convergen en el "corazón del área" (entre el punto de penalti y el área chica), demostrando que son jugadas precisas.

---

#### Sección 4-6: Productividad y Eficiencia por Configuración

- **Comparación Agregada de Córners:** Este gráfico revela una dicotomía táctica fundamental:
  - **Estrategia de Volumen (0-1 Atacante):** Generan la mayor cantidad de remates y xG, ideal para equipos que buscan abrumar y generar segundas jugadas.
  - **Estrategia de Eficiencia (2+ Atacantes):** Convierten el peligro esperado (xG) en goles reales de forma mucho más consistente. La configuración de **3 atacantes** es la única que supera masivamente sus expectativas (14.9 xG vs. 20 goles).
- **Tasa de Gol por Remate:** Este gráfico confirma el "punto dulce" táctico.
  - La configuración de **3 Atacantes** emerge como la más letal, con una tasa de gol del **14.4%** (1 de cada 7 remates es gol), casi el triple que la configuración más común.
  - Se observa una "escalera de la eficiencia" que aumenta progresivamente hasta los 3 atacantes.

| Atacantes en Área Chica | Tasa de Gol | Frecuencia de Gol (aprox.) |
| :--- | :---: | :--- |
| 0 Atacantes | 4.9% | 1 de cada 20 remates |
| 1 Atacante | 6.1% | 1 de cada 16 remates |
| 2 Atacantes | 7.5% | 1 de cada 13 remates |
| **3 Atacantes** | **14.4%** | **1 de cada 7 remates** |
| 4+ Atacantes | 10.0% | 1 de cada 10 remates |

- **Visualización por Temporada:** El desglose anual confirma que estas tendencias no son una anomalía, sino patrones de comportamiento estables y predecibles en el fútbol mexicano.

---

#### Sección 7-14: Perfiles Tácticos y Análisis Avanzado

- **Efectividad por Tipo de Córner:**
  - **Letalidad:** `Inswingers` y `Outswingers` son los más letales (tasa de gol del 1.6%).
  - **Valor Global (OBV):** El `Inswinger` emerge como el cobro de mayor valor (+0.0153), creando las situaciones más peligrosas en general. El `Outswinger` también es muy valioso (+0.0123).
  - **Riesgo:** El córner en `Corto` tiene un valor OBV negativo (-0.0105), sugiriendo que es una estrategia de alto riesgo en la Liga MX.
- **Perfiles Tácticos (Radares, Mapas, Dispersión):** Estas secciones permiten un diagnóstico profundo para el scouting, identificando:
  - **Estilo de Juego:** Si un equipo es de "volumen" o de "precisión".
  - **Libro de Jugadas:** Las preferencias tácticas (tipos de cobro) y geográficas (zonas de destino) de cada club.
  - **Dependencia vs. Calidad:** Los gráficos de dispersión clasifican a los equipos en perfiles como "Especialistas de Élite", "Potencias Equilibradas", "Especialistas por Necesidad" o "Juego Abierto como Prioridad".
- **Ratios de Gol:** Las tablas de ratios diferencian entre la **letalidad directa** (gol en el primer remate) y la **presión sostenida** (gol en la misma secuencia de juego), revelando qué equipos son "francotiradores" y cuáles son "equipos de asedio".

---

### 🚀 Cómo Ejecutar este Proyecto

Para correr este dashboard en tu propia máquina, sigue estos pasos:

**1. Requisitos Previos (Instalar Librerías)**
Asegúrate de tener R y RStudio instalados. Luego, ejecuta el siguiente comando en la consola de R para instalar todas las librerías necesarias:

```R
install.packages(c("shiny", "bslib", "tidyverse", "arrow", "fs", "glue", "ggtext", "plotly", "ggsoccer", "fuzzyjoin", "scales", "gt", "ggrepel", "ggimage", "base64enc"))
