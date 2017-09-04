(ns logical-interpreter)
(require '[clojure.string :as str])

(declare evaluate-query)

;Nombre de las sentencias: facts, rules y queries
(def formatSentenceName "[a-zA-Z0-9]{1,}")

;Parametros de las rules
(def formatRuleParams "\\([A-Z](,[A-Z]){0,}\\)")

(def formatFactQueryParams "\\([a-z0-9]{1,}(,[a-z0-9]{1,}){0,}\\)")

;Componentes de las rules
(def formatComponents (str formatSentenceName "\\([A-Z](,[A-Z]){0,}\\)(," formatSentenceName "\\([A-Z](,[A-Z]){0,}\\)){0,}"))

(def formatAllRuleParams "\\(([^)]+)\\)")

(def regFactQueryParams (re-pattern formatFactQueryParams))

(def regAllRuleParams (re-pattern formatAllRuleParams))

;Caracteres a remover de todas las sentencias
(def regRemoveCharacters #"[ \. \t]")

;Regular expression que define el formato válido de una Rule
(def regRule (re-pattern (str "^" formatSentenceName formatRuleParams ":-" formatComponents "$")))

;Regular expression que define el formato valido de una Fact
(def regFact (re-pattern (str "^" formatSentenceName formatFactQueryParams "$")))

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
  (let [regexResult (re-find (re-matcher regFactQueryParams sentence))]
    (if (> (count regexResult) 0)
      (nth regexResult 0)
      []
    )
  )
)
(defn getRuleParams [sentence]
  "Retorna los parametros de entrada de una Rule"
  (let [matchs (re-matcher regAllRuleParams sentence)]
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

; ;Chequea si la sentencia tiene un formato válido
(defmulti hasValidFormat (fn [sentence] (getTypeExpression sentence)))
 (defmethod hasValidFormat Fact [sentence]
  ( >
    ( count 
      ( re-find 
        ( re-matcher regFact sentence )
      )
    ) 0
  )
 )

 (defmethod hasValidFormat Rule [sentence]
  ( >
    ( count 
      ( re-find 
        ( re-matcher regRule sentence )
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
  (str/replace sentence regRemoveCharacters "")
)

(defn removeAllEmptySentences [database]
  "Remueve todas las sentencias vacias"
  (filter (fn [s] (not (empty? s))) database))

(defn getAllDatabaseSentences [database]
  "Retorna una lista de las Facts y Rules de la base de datos"
  (removeAllEmptySentences (map cleanSentence (str/split-lines database))))

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
    (if ( = sentence query )
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
(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  ;La query es válida?
  (let [cleanQuery (cleanSentence query)]
    (if (not (hasValidFormat cleanQuery))
      (println (str "La consulta tiene un formato incorrecto"))
      (do
        ;antes de leer la base, tengo que borrar las lineas en blanco
        ;parseo la base
        (let [parsedDatabase (getAllDatabaseSentences database)]
          ;si no es valida la base, muestro un mensaje de error
          (if (hasInvalidSentences parsedDatabase)
            (do 
              (println "La base de datos tiene sentencias incorrectas")
              nil  
            )
            ;quitar los espacios de la query y validarla
            (if (canEvaluateQuery parsedDatabase cleanQuery)
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

