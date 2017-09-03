(ns logical-interpreter)
(require '[clojure.string :as str])

(def parent-database "
	varon(juan).
	varon(pepe).
	varon(hector).
	varon(roberto).
	varon(alejandro).
	mujer(maria).
	mujer(cecilia).
	padre(juan, pepe).
	padre(juan, pepa).
	padre(hector, maria).
	padre(roberto, alejandro).
	padre(roberto, cecilia).
	hijo(X, Y) :- varon(X), padre(Y, X).
	hija(X, Y) :- mujer(X), padre(Y, X).
")

(deftype Fact [expression])
(deftype Rule [expression])

(defn getTypeExpression [sentence] (if (str/includes? sentence ":-") Rule Fact))

;nombre(param1,param2)
(defn getSentenceName [sentence]
  "Retorna el nombre de la sentencia"
  (re-find (re-matcher #"^[^\(]*" sentence)))

;nombre(param1,param2)
(defn getParameters [sentence]
  "Retorna los parametros de la sentencia" 
  (nth (re-find (re-matcher #"(\(){1,1}[^\(\)]{1,}(\)){1,1}" sentence)) 0))

(defn getRuleParams [sentence]
  "Retorna los parametros de entrada de una Rule"
  (let [matchs (re-matcher #"\(([^)]+)\)" sentence)]
  (str/split (nth (re-find matchs) 1) #",")))

;Si sentence es una fact, indica si es igual a la query
;Si sentence es una Rule, indica si tiene el mismo nombre y parametros que la query dada
(defmulti compareSentence (fn [sentence query] (getTypeExpression sentence)))
(defmethod compareSentence Fact [sentence query] 
  (and 
    ( = (getSentenceName sentence) (getSentenceName query))
    ( = (getParameters sentence) (getParameters query))
  )
)
(defmethod compareSentence Rule [sentence query] 
  (and 
    ( = (getSentenceName sentence) (getSentenceName query))
    ( = (count (getRuleParams sentence)) (count (getRuleParams query)))
  )
)
(defmethod compareSentence :default [sentence query] true)

; ;Evalua el valor de verdad de la query contra una sentencia que puede ser una Rule o una Fact
; (defmulti evaluate (fn [sentence, query] getTypeExpression sentence))
; (defmethod evaluate Fact [sentence, query]
; 	(= sentence query))

; ;Chequea si la sentencia tiene un formato válido
(defmulti hasValidFormat (fn [sentence] (getTypeExpression sentence)))
 (defmethod hasValidFormat Fact [sentence]
 ;\([^\( \) \: \-]{1,},[^\( \) \: \-]{1,}\)
  ( >
    ( count 
      ( re-find 
        ( re-matcher #"^([^\(\)\:\-]{1,})(\([^\(\)\:\-\,]{1,})(,[^\(\)\:\-\,]{1,}){0,}\)$" sentence )
      )
    ) 0
  )
 )

 (defmethod hasValidFormat Rule [sentence]
  ( >
    ( count 
      ( re-find 
        ( re-matcher #"^([^\(\)\:\-]{1,})(\([A-Z]{1,})(,[A-Z]{1,}){0,}\):-([^\(\)\:\-]{1,})(\([A-Z]{1,})(,[A-Z]{1,}){0,}\)([^\(\)\:\-]{1,})(\([A-Z]{1,})(,[A-Z]{1,}){0,}\)$" sentence )
      )
    ) 0
  )
 )

(defmethod hasValidFormat :default [sentence] false) 

(defn getDatabaseSentence [database query]
  "Retorna, si existe la primer sentencia de la base de datos que matchea con la consulta"
  ( filter 
    (fn [s] (compareSentence s query)) 
    database
  )
)

;Aplica la query a la sentencia
(defmulti evaluate (fn [sentence query] (getTypeExpression sentence)))

(defmethod evaluate Fact [sentence query]
  (if ( = sentence nil )
    false
    true
  )
)

(defn hasInvalidSentences [parsedDatabase]
  "Indica si la base de datos tiene sentencias que no cumplen con el formato correcto"
  (some false? (map hasValidFormat parsedDatabase))
)

(defn cleanSentence [sentence]
"Remueve de la sentencia los blancos, el punto del final y tabs al inicio si es que los hubiera"
  (str/replace sentence #"[ \. \t]" "")
)

(defn isEmptySentence [sentence]
  "Indica si la sentencia está vacía"
  (re-find (re-matcher #"(^[ \t]*\n)" sentence)))

(defn removeAllEmptySentences [database]
  "Remueve todas las sentencias vacias"
  (filter (fn [s] (not (isEmptySentence s))) database))

(defn getAllDatabaseSentences [database]
  "Retorna una lista de las Facts y Rules de la base de datos"
  (map cleanSentence (removeAllEmptySentences (str/split database #"\n"))))

(defn getSentenceName [sentence]
  "Retorna el nombre de la sentencia"
  (re-find (re-matcher #"^[^\(]*" sentence)))

(defn replaceFirstParam [sentence, params, replacements]
  "dada una lista de variables y de valores, reemplaza en la sentencia la primer variable por el primer valor"
  (if (>(count params) 0)
      (do
        (str/replace sentence (last params) (last replacements))    
        (replaceFirstParam sentence (butlast params) (butlast replacements))
      )
  )
)

(defn replaceParams [sentence, params]
  "Reemplaza todas las variables de la sentencia por los parametros dados"
  (let [parameters (getRuleParams sentence)]
       (replaceFirstParam sentence parameters params)
  )
)

;;pruebas
;(println parsedDatabase)
;(println (map (fn[s] (str "'" s "'")) parsedDatabase))


;(println (parse parent-database))
;(println (count (parse parent-database)))
;(println (map (fn[sentence] (re-find (re-matcher #"^([^\( \) \: \-]{1,})" sentence))) (parse parent-database)))
;(println (re-find (re-matcher #"^[^\( \) \: \-]{1,}" "padre(hector, maria)")))

;^[^\(\)\:\-]{1,}\(([^\(\)\:\-]{1,}[,]{0,1}){1,}\)


;para una rule, primero reemplazo los parametros que me pasa la query y despues ejecuto cada una de las facts que se arman con una and


(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]

  ;parseo la base
  (let [parsedDatabase (getAllDatabaseSentences parent-database)]
  
  ;si no es valida la base, muestro un mensaje de error
  ; (if (hasInvalidSentences parsedDatabase)
  ;   (println "La base de datos tiene sentencias incorrectas"))
  
  ;quitar los espacios de la query y validarla

  ;veo si existe la query en la base -> si no existe retorno nil

  (let [sentence (getDatabaseSentence parsedDatabase query)];TODO: tomar solo una sentence, para el caso en que esté repetidas
  (if (= sentence nil)
    nil
    (do
      (println (evaluate sentence query))
    )
  ))
  ;veo el valor de verdad de la query
  nil))

(evaluate-query parent-database "varon(juan)")
(evaluate-query parent-database "varon(maria)")