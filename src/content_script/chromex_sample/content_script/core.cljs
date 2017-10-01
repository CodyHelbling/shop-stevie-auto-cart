(ns chromex-sample.content-script.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :refer [<!]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.protocols :refer [post-message!]]
            [chromex.ext.runtime :as runtime :refer-macros [connect]]))


(defn compress-and-lower [s]                                       
  "Remove whitespace and dashes from string"
  (log "----- compress-and-lower -----")
  (log "  s: " s)
  (clojure.string/lower-case (clojure.string/replace s #"\-|\s" ""))) 

(defn piece-in-links
  ([piece links]
   (log "----- piece-in-links -----")
   (log "   piece: " piece)
   (log "   links: " links)
   (let [compressed-piece (compress-and-lower piece)
         compressed-link (compress-and-lower (aget (aget links 0) "href"))
         piece-in-link (re-find #"wholesale" compressed-link)]
     (log "here")
     (if (< (.-length links)1)
       nil)
     (log "here 2")
     (if piece-in-link
       (aget (aget links 0) "href")
       (piece-in-links piece links 1))))
  ([piece links link-index]
   (log "----- Recurse: piece-in-links -----")
   (log "   piece: " piece)
   (log "   links: " links)   
   (let [compressed-piece (compress-and-lower piece)
         compressed-link  (compress-and-lower (aget (aget links link-index) "href"))
         piece-in-link (re-find #"wholesale" compressed-link)]
     (if (> link-index (count links))
       nil)
     (if piece-in-link
       (aget links link-index)
       (piece-in-links piece links link-index)))))

(defn parse-text [text]                                            
  "TODO: if first chars of string aren't correct, don't run regex" 
  (log "Parsing For Id")                                           
  (re-find #"\"rid\":\d{11}" text))                                

(defn log-seq                                                      
  ([stuff]                                                         
   (log (str "--------- log-seq: START ---------\n" (aget stuff 0)))                          
   (log-seq stuff 1))                                              
  ([stuff index]                                                   
   (if (< index  (.-length stuff))                                 
     (do
       (log (aget (aget stuff index) "href"))
       ;(log (aget (aget stuff index) "src"))                     
       ;(log (parse-text (aget (aget stuff index) "text")) )        
       ; (parse-text "this is a test")                             
       ;(log (aget (aget stuff index) "type"))                      
       ; (log (str (aget (aget stuff index) "text") "\n\n\n"))     
       (log-seq stuff (inc index)))
     (log "---------- log-seq: COMPLETE ---------"))))                             

(defn append-to-vector
  ([v]
   (if (> (count v) 1)
     (append-to-vector (conj TEST (nth v 0)) 1))
   ([v index]
    (if (< (count v) index)
      (append-to-vector (conj TEST (nth v index) (inc index)))))))
  
(defn find-desired-pieces
  ([links pieces]
   (log "----- find-desired-pieces -----")
   (log "   pieces: " pieces)
   (log "   links: " links)
   (let [found-pieces []
         in-links (piece-in-links (nth pieces 0) links)]
     (log "Finding desired pieces!")
     (if (and in-links (> (count pieces) 1))
       (find-desired-pieces links pieces (conj found-pieces {piece-in-links (nth pieces 0)}) 1)
       (find-desired-pieces links pieces found-pieces 1))
     (if (< (count pieces) 1)
       [] )))
  ([links pieces found-pieces piece-index]
   (let [found-pieces {}
         in-links (piece-in-links (nth pieces piece-index) links)]
     (log "Finding desired pieces!")
     (if (< (inc piece-index) (count pieces))
       (do
         (if in-links
           (find-desired-pieces links pieces (conj found-pieces {piece-in-links (nth pieces piece-index)}) (inc piece-index))
           (find-desired-pieces links pieces found-pieces (inc piece-index))))
       found-pieces))))


; -- a message loop ---------------------------------------------------------------------------------------------------------

(defn process-message! [message background-port]
    (log "CONTENT SCRIPT: got message:" message)
    ; (log "Pieces: " (aget message "tail") 0)
    ; (log "Piece: " (js->clj (nth (aget message "tail") 0)))
    (do-page-analysis! (aget message "tail") background-port))
    

(defn run-message-loop! [message-channel]
  (log "CONTENT SCRIPT: starting message loop...")
  (go-loop []
    (when-some [message (<! message-channel)]
      (process-message! message message-channel)
      (recur))
    (log "CONTENT SCRIPT: leaving message loop")))

; -- a simple page analysis  ------------------------------------------------------------------------------------------------

(defn do-page-analysis! [pieces background-port]
  (let [script-elements (.getElementsByTagName js/document "script")
        link-elements (.getElementsByTagName js/document "a")
        link-count (.-length link-elements)
        title (.-title js/document)
        msg (str "CONTENT SCRIPT: document '" title "' contains " link-count " link tags.")
        found-pieces (if pieces
                       (find-desired-pieces link-elements pieces))]
    (log msg)
    (log-seq link-elements)
    (log "Pieces: " pieces)
    ; 
    (post-message! background-port (str link-elements))))


(defn connect-to-background-page! []
  (let [background-port (runtime/connect)]
    (post-message! background-port "hello from CONTENT SCRIPT!")
    (run-message-loop! background-port)))
    ; (do-page-analysis! background-port)))

; -- main entry point -------------------------------------------------------------------------------------------------------

(defn init! []
  (log "CONTENT SCRIPT: init")
  (connect-to-background-page!))
