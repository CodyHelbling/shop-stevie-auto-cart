(ns chromex-sample.content-script.helpers)

(defn compress-and-lower [s]
  "Remove whitespace and dashes from string"
  (clojure.string/lower-case (clojure.string/replace #"-|\s" "")))

(defn parse-text [text]
  "TODO: if first chars of string aren't correct, don't run regex"
  (log "Parsing For Id")
  (re-find #"\"rid\":\d{11}" text))

(defn log-seq
  ([stuff]
   (log (str "log-seq: " (aget stuff 0)))
   (log-seq stuff 1))
  ([stuff index]
   (if (< index  (.-length stuff))
     (do
       ; (log (aget (aget stuff index) "src"))
       (log (parse-text (aget (aget stuff index) "text")) )
       ; (parse-text "this is a test")
       (log (aget (aget stuff index) "type"))
       ; (log (str (aget (aget stuff index) "text") "\n\n\n"))
       (log-seq stuff (inc index))))))
