(ns logical-interpreter)
(require '[clojure.string :as str])

(declare evaluate-query)

; (def parent-database "
;   varon(juan).
; 	varon(pepe).
; 	varon(hector).
; 	varon(roberto).
; 	varon(alejandro).
; 	mujer(maria).
; 	mujer(cecilia).
; 	padre(juan, pepe).
; 	padre(juan, pepa).
; 	padre(hector, maria).
; 	padre(roberto, alejandro).
; 	padre(roberto, cecilia).
; 	hijo(X, Y) :- varon(X), padre(Y, X).
; 	hija(X, Y) :- mujer(X), padre(Y, X).
; ")

(deftype Fact [expression])
(deftype Rule [expression])

(defn getTypeExpression [sentence] (if (or (= sentence nil) (str/includes? sentence ":-")) Rule Fact))

;nombre(param1,param2)
(defn getSentenceName [sentence]
  "Retorna el nombre de la sentencia"
  (re-find (re-matcher #"^[^\(]*" sentence)))

;nombre(param1,param2)
(defn getParameters [sentence]
  "Retorna los parametros de la sentencia" 
  (let [regexResult (re-find (re-matcher #"(\(){1,1}[^\(\)]{1,}(\)){1,1}" sentence))]
    (if (> (count regexResult) 0)
      (nth regexResult 0)
      []
    )
  )
)
(defn getRuleParams [sentence]
  "Retorna los parametros de entrada de una Rule"
  (let [matchs (re-matcher #"\(([^)]+)\)" sentence)]
  (str/split (nth (re-find matchs) 1) #",")))

(defmulti canEvaluate (fn [query sentence] (getTypeExpression sentence)))
(defmethod canEvaluate Fact [query sentence] 
  (if (or (= query nil) (= sentence nil))
    false
    (and 
      ( = (getSentenceName sentence) (getSentenceName query))
      ( = (count (getRuleParams sentence)) (count (getRuleParams query)))
    )
  )
)
(defmethod canEvaluate Rule [query sentence] 
  (if (or (= query nil) (= sentence nil))
    false
    (and 
      ( = (getSentenceName sentence) (getSentenceName query))
      ( = (count (getRuleParams sentence)) (count (getRuleParams query)))
    )
  )
)

;Si sentence es una fact, indica si es igual a la query
;Si sentence es una Rule, indica si tiene el mismo nombre y parametros que la query dada
(defmulti compareSentence (fn [sentence query] (getTypeExpression sentence)))
(defmethod compareSentence Fact [sentence query] 
  ; (if ( = (getSentenceName sentence) (getSentenceName query))
  ;   ( = (getParameters sentence) (getParameters query))
  ; )

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
(defmethod compareSentence :default [sentence query] false)

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
        ( re-matcher #"^[a-zA-Z0-9]{1,}\([A-Z](,[A-Z]){0,}\):-[a-zA-Z0-9]{1,}\([A-Z](,[A-Z]){0,}\)(,[a-zA-Z0-9]{1,}\([A-Z](,[A-Z]){0,}\)){0,}$" sentence )
      )
    ) 0
  )
 )

(defmethod hasValidFormat :default [sentence] false) 

(defn canEvaluateQuery [database query]
  "Indica si existen facts o rules con el mismo nombre y cantidad de parametros que la query dada"
  (some true? (map (fn [x] (canEvaluate query x)) database))
)

(defn getDatabaseSentence [database query]
  "Retorna, si existe la primer sentencia de la base de datos que matchea con la consulta"
  (let [sentences ( filter 
      (fn [s] (compareSentence s query)) 
      database
    )]
    (if (> (count sentences) 0)
      (first sentences)
      nil
    )
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

; (defn isEmptySentence [sentence]
;   "Indica si la sentencia está vacía"
;   (re-find (re-matcher #"(^[ \t]*\n)" sentence)))

(defn removeAllEmptySentences [database]
  "Remueve todas las sentencias vacias"
  (filter (fn [s] (not (empty? s))) database))

(defn getAllDatabaseSentences [database]
  "Retorna una lista de las Facts y Rules de la base de datos"
  (removeAllEmptySentences (map cleanSentence (str/split database #"\n"))))

(defn getSentenceName [sentence]
  "Retorna el nombre de la sentencia"
  (re-find (re-matcher #"^[^\(]*" sentence)))

(defn replaceFirstParam [sentence, params, replacements]
  "dada una lista de variables y de valores, reemplaza en la sentencia la primer variable por el primer valor"
  (if (>(count params) 0)
      (do
        (replaceFirstParam 
          (str/replace sentence (last params) (last replacements)) 
          (butlast params) 
          (butlast replacements)
        )
      )
      sentence
  )
)

(defn replaceParams [sentence, params]
  "Reemplaza todas las variables de la sentencia por los parametros dados"
  (let [parameters (getRuleParams sentence)]
    (replaceFirstParam sentence parameters params)
  )
)

(defn evaluateList [database factList]
  "Dada una lista de Facts, las evalua contra la base de datos y retorna true si todas son verdaderas"
  (every? true? 
    (map (fn [x] (evaluate-query database x)) factList)
  )
)

(defn getRuleComponents [sentence]
  "Retorna los componentes de una rule. Dada r(X):-a(X),b(X) retorna [a(X) b(X)]"
  (str/split 
    (str/replace ;reemplazo las comas separadoras de facts por pipes para poder hacer el split sin que separe los parametros de los facts
      (str/replace sentence #"^([^:]*:\-)" "") 
      #"\)," 
      ")|") 
    #"\|"
  )
)

;Aplica la query a la sentencia
(defmulti evaluate (fn [database sentence query] (getTypeExpression sentence)))

(defmethod evaluate Fact [database sentence query]
  (if (= sentence nil)
    false
    (if ( = sentence query ) ;;TODO: sentence == query?
      true
      false
    )
  )
)

(defmethod evaluate Rule [database sentence query]
  (if (= sentence nil)
    false
    (evaluateList database (getRuleComponents (replaceParams sentence (getRuleParams query))))
  )
)

; (defn evaluateQuery [database query]
;   "Retorna el resultado de evaluar la query en la base de datos parseada"
;   (let [sentence (getDatabaseSentence database query)];TODO: tomar solo una sentence, para el caso en que esté repetidas
;      (if (= sentence nil)
;       nil
;       (do
;         (evaluate database sentence query)
;       )
;     )
;   )
; )

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
  ;La query es válida?
  (if (not (hasValidFormat query))
    (println (str "La consulta tiene un formato incorrecto"))
    (do
      ;antes de leer la base, tengo que borrar las lineas en blanco
      ;parseo la base
      (let [parsedDatabase (getAllDatabaseSentences database)]
        ; (println (count parsedDatabase))
        ; (println (count (filter (fn [x] (not (empty? x))) parsedDatabase)))
        ;si no es valida la base, muestro un mensaje de error
        (if (hasInvalidSentences parsedDatabase)
          (do 
            (println "La base de datos tiene sentencias incorrectas")
            nil  
          )
          ;quitar los espacios de la query y validarla
          (let [cleanQuery (cleanSentence query)]
            (if (canEvaluateQuery parsedDatabase query)
              (let [sentence (getDatabaseSentence parsedDatabase cleanQuery)];TODO: tomar solo una sentence, para el caso en que esté repetidas
                (evaluate database sentence cleanQuery)
              )
              nil
            )
          )
        )
      )
    )
  )
)

 ;(println (evaluate-query parent-database "varon(maria)"))
; (println (evaluate-query parent-database "varon(juan)"))
; (println (evaluate-query parent-database "mujer(maria)"))
; (println (evaluate-query parent-database "hijo(pepe,juan)"))
; (println (evaluate-query parent-database "hijo(pepe,juana)"))
; (println (evaluate-query parent-database "son(pepe,juana)"))


;(println (evaluateList (getRuleComponents(replaceParams "hija(X,Y):-mujer(X),padre(Y,X)" ["pepe" "pipo"])) "hija(pipa,popo)"))

