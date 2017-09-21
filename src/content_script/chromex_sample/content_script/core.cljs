(ns chromex-sample.content-script.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :refer [<!]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.protocols :refer [post-message!]]
            [chromex.ext.runtime :as runtime :refer-macros [connect]]))

; -- a message loop ---------------------------------------------------------------------------------------------------------

(defn process-message! [message]
  (log "CONTENT SCRIPT: got message:" message)
  (log "Pieces: " (aget message "tail") 0)
  (log "Piece One: " (nth (aget message "tail") 0)))

(defn run-message-loop! [message-channel]
  (log "CONTENT SCRIPT: starting message loop...")
  (go-loop []
    (when-some [message (<! message-channel)]
      (process-message! message)
      (recur))
    (log "CONTENT SCRIPT: leaving message loop")))

; -- a simple page analysis  ------------------------------------------------------------------------------------------------

(defn do-page-analysis! [background-port]
  (let [link-elements (.getElementsByTagName js/document "script")
        link-count (.-length link-elements)
        title (.-title js/document)
        msg (str "CONTENT SCRIPT: document '" title "' contains " link-count " link tags.")]
    (log msg)
    (log-seq link-elements)
    (post-message! background-port (str link-elements))))


(defn parse-text
  ([match text]
   (if (= (nth match 0) (nth text 0))
     (parse-text match text 0 1)
     (parse-text match text 1 1)))
  ([match text match-index parse-index]
   (if (= (nth match match-index) (nth text parse-index))
     (do
       (if (> match-index 10)
         (do
           (log "Test agains regex")
           (re-find #"\"variantId\":\d{11}" (subs text (- parse-index match-index))))
         (parse-text match text (inc match-index) (inc parse-index))))
     (parse-text match text 0 (inc parse-index)))))
   

(defn log-seq
  ([stuff]
   (log (str "log-seq: " (aget stuff 0)))
   (log-seq stuff 1))
  ([stuff index]
   (if (< index  (.-length stuff))
     (do
       ;(log (aget (aget stuff index) "src"))
       (parse-text "variantID\":" (aget (aget stuff index) "src"))
       (log (aget (aget stuff index) "type"))
       (log (str (aget (aget stuff index) "text") "\n\n\n"))
       (log-seq stuff (inc index))))))


(defn load-webpage [link]
  (log "Loading webpage: " link))

(defn find-id [text]
  (log "Finding Id"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defn find-desired-pieces                                                                        ;;
;;   ([link-elements desired-pieces background-port index]                                          ;;
;;   (let [found-pieces []]                                                                         ;;
;;     (if (< (count link-elements) 1)                                                              ;;
;;       (post-message! background-port "Looked through all pieces")                                ;;
;;       (do (if (some #{(nth link-elements index)} desired-pieces)                                 ;;
;;             (post-message! background-port (str "Found desired piece: " (nth desired-pieces 0))) ;;
;;             (find-desired-pieces [lin                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn connect-to-background-page! []
  (let [background-port (runtime/connect)]
    (post-message! background-port "hello from CONTENT SCRIPT!")
    (run-message-loop! background-port)
    (do-page-analysis! background-port)))

; -- main entry point -------------------------------------------------------------------------------------------------------

(defn init! []
  (log "CONTENT SCRIPT: init")
  (connect-to-background-page!))
