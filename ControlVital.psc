Proceso Sistema_Control_De_Datos_Medicos_Control_Vital
    
    Definir clasificacion_imc, detalles_alerta, continuar Como Cadena;
    Definir talla, peso, presion_sistolica, presion_diastolica, glucosa, temperatura, imc Como Real;
    Definir datos_normales Como Logico;
    
	
    Repetir
        Limpiar Pantalla;
        Escribir "=== SISTEMA DE CONTROL DE DATOS MÉDICOS BÁSICOS ===";
		
       
		Escribir "Ingrese el nombre completo del paciente:"
		Definir nombre Como Caracter
		Definir esNumero Como Logico
		Repetir
			leer nombre
			esNumero<- Verdadero
			para i <- 1 hasta Longitud(nombre) Hacer
				si Subcadena(nombre,i,i) < "0" o Subcadena(nombre,i,i) > "9" Entonces
					esNumero <-Falso
				FinSi
			FinPara
			
			si esNumero Entonces
				escribir "Error: No se permiten numeros. SOLO LETRAS"
			FinSi
		Hasta Que NO esNumero
		escribir "Hola:" , nombre
        
		
        Definir fecha_nac Como Cadena
		Definir dia_txt, mes_txt, anio_txt Como Cadena
		Definir dia_nac, mes_nac, anio_nac Como Entero
		Definir dia_hoy, mes_hoy, anio_hoy Como Entero
		Definir edad, i Como Entero
		Definir es_valida Como Logico
		
		Repetir
			es_valida <- Verdadero
			Escribir "Ingrese su fecha de nacimiento en formato DD-MM-AAAA:"
			Leer fecha_nac
			
			// Validar formato mínimo
			Si Longitud(fecha_nac) <> 10 O SubCadena(fecha_nac, 3, 3) <> "-" O SubCadena(fecha_nac, 6, 6) <> "-" Entonces
				Escribir "Formato inválido. Use el formato DD-MM-AAAA."
				es_valida <- Falso
			Sino
				// Extraer partes
				dia_txt <- SubCadena(fecha_nac, 1, 2)
				mes_txt <- SubCadena(fecha_nac, 4, 5)
				anio_txt <- SubCadena(fecha_nac, 7, 10)
				
				// Validar que día y mes sean solo números
				Para i <- 1 Hasta 2
					Si SubCadena(dia_txt, i, i) < "0" O SubCadena(dia_txt, i, i) > "9" Entonces
						es_valida <- Falso
					FinSi
					Si SubCadena(mes_txt, i, i) < "0" O SubCadena(mes_txt, i, i) > "9" Entonces
						es_valida <- Falso
					FinSi
				FinPara
				Para i <- 1 Hasta 4
					Si SubCadena(anio_txt, i, i) < "0" O SubCadena(anio_txt, i, i) > "9" Entonces
						es_valida <- Falso
					FinSi
				FinPara
				
				Si No es_valida Entonces
					Escribir "La fecha contiene letras o símbolos. Solo se permiten números."
				FinSi
			FinSi
			
			// Convertir a número y validar rangos
			Si es_valida Entonces
				dia_nac <- ConvertirANumero(dia_txt)
				mes_nac <- ConvertirANumero(mes_txt)
				anio_nac <- ConvertirANumero(anio_txt)
				
				Si dia_nac < 1 O dia_nac > 31 Entonces
					Escribir "Día fuera de rango (1-31)."
					es_valida <- Falso
				FinSi
				Si mes_nac < 1 O mes_nac > 12 Entonces
					Escribir "Mes fuera de rango (1-12)."
					es_valida <- Falso
				FinSi
				Si anio_nac < 1900 O anio_nac > 2025 Entonces
					Escribir "Año fuera de rango (1900-2025)."
					es_valida <- Falso
				FinSi
			FinSi
			
		Hasta Que es_valida
		
		// Fecha actual (fija)
		dia_hoy <- 2
		mes_hoy <- 8
		anio_hoy <- 2025
		
		// Cálculo de edad
		edad <- anio_hoy - anio_nac
		Si (mes_hoy < mes_nac) O (mes_hoy = mes_nac Y dia_hoy < dia_nac) Entonces
			edad <- edad - 1
		FinSi
		
		// Resultado
		Escribir "HOLA: ", nombre, ",",   "EDAD: ", edad, " años."
		
        // ======== INGRESO DE DATOS MÉDICOS =========
        Escribir "Ingrese la talla (m):"; Leer talla;
        Escribir "Ingrese el peso (kg):"; Leer peso;
        Escribir "Ingrese la presión sistólica (mmHg):"; Leer presion_sistolica;
        Escribir "Ingrese la presión diastólica (mmHg):"; Leer presion_diastolica;
        Escribir "Ingrese glucosa (mg/dL):"; Leer glucosa;
        Escribir "Ingrese temperatura (°C):"; Leer temperatura;
		
        // ======== CALCULAR IMC =========
        imc <- peso / (talla * talla);
		
        Si imc < 18.5 Entonces
            categoria_imc <- 1;
        Sino
            Si imc < 25 Entonces
                categoria_imc <- 2;
            Sino
                Si imc < 30 Entonces
                    categoria_imc <- 3;
                Sino
                    categoria_imc <- 4;
                FinSi
            FinSi
        FinSi
		
        // ======== CLASIFICAR IMC CON SEGUN =========
        Segun categoria_imc
            1:
                clasificacion_imc <- "Bajo peso";
            2:
                clasificacion_imc <- "Normal";
            3:
                clasificacion_imc <- "Sobrepeso";
            4:
                clasificacion_imc <- "Obesidad";
        FinSegun
		
        // ======== VALIDAR RANGOS =========
        datos_normales <- Verdadero;
        detalles_alerta <- "";
		
        Si No((presion_sistolica >= 90 Y presion_sistolica <= 120) Y (presion_diastolica >= 60 Y presion_diastolica <= 80)) Entonces
            detalles_alerta <- detalles_alerta + "- Presión arterial fuera de rango (90-120/60-80) ";
            datos_normales <- Falso;
        FinSi
		
        Si No(glucosa >= 70 Y glucosa <= 100) Entonces
            detalles_alerta <- detalles_alerta + "- Glucosa fuera de rango (70-100) ";
            datos_normales <- Falso;
        FinSi
		
        Si No(temperatura >= 36 Y temperatura <= 37.5) Entonces
            detalles_alerta <- detalles_alerta + "- Temperatura fuera de rango (36.0-37.5) ";
            datos_normales <- Falso;
        FinSi
		
        Si clasificacion_imc <> "Normal" Entonces
            detalles_alerta <- detalles_alerta + "- IMC fuera de rango (" + clasificacion_imc + ") ";
            datos_normales <- Falso;
        FinSi
		
        // ======== RESULTADOS =========
        Escribir "----------------------------------------";
        Escribir "Paciente: ", nombre;
        Escribir "Edad: ", edad, " años";
        Escribir "IMC: ", imc, " (", clasificacion_imc, ")";
        Escribir "Presión arterial: ", presion_sistolica, "/", presion_diastolica;
        Escribir "Glucosa: ", glucosa;
        Escribir "Temperatura: ", temperatura;
        Escribir "----------------------------------------";
		
        Si datos_normales Entonces
            Escribir "Todos los datos están en rango normal.";
        Sino
            Escribir "ALERTA: Datos fuera de rango:";
            Escribir detalles_alerta;
        FinSi
		
        Escribir "¿Desea registrar otro paciente? (si/no):";
        Leer continuar;
		
    Hasta Que continuar = "no";
	
    Escribir "Sistema finalizado. Gracias por utilizar el servicio.";
FinProceso


