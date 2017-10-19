(ns chromex-sample.content-script.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :refer [<!]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.protocols :refer [post-message!]]
            [chromex.ext.runtime :as runtime :refer-macros [connect]]
            [ajax.core :refer [GET POST]]))


(defn compress-and-lower [s]                                       
  "Remove whitespace and dashes from string"
  ; (log "----- compress-and-lower -----")
  ; (log "  s: " s)
  (if s
    (clojure.string/lower-case (clojure.string/replace s #"\-|\s" ""))
    nil))


(defn get-link [dom-element]
  (if (nil? (aget dom-element "href"))
    nil
    (aget dom-element "href")))
    

(defn find-piece-in-link [piece, link]
  ; (log "find-piece-in-link")
  ; (log "   piece: " piece)
  ; (log "   link: " link)
  ; (log "   response: " (re-find (re-pattern piece) link))
  (re-find (re-pattern piece) link))
   

(defn piece-in-links
  ([piece links]
   ;(log "----- piece-in-links -----")
   ;(log "   piece: " piece)
   ;(log "   links: " links)
   (let [compressed-piece (compress-and-lower piece)
         compressed-link (compress-and-lower (aget (aget links 0) "href"))
         piece-in-link (find-piece-in-link compressed-piece compressed-link)]
     ;(log "  compressed piece: " compressed-piece)
     ;(log "  compressed link: " compressed-link)
     (if (= compressed-link nil)
       (piece-in-links piece links 1)
       (do
         (if (< (.-length links) 1)
           nil
           (do
             (if piece-in-link
               (aget (aget links 0) "href")
               (piece-in-links piece links 1))))))))
  ([piece links link-index]
   "TODO: The way the function recurses is incorrect. Alswas misses last link"
   ;(log "----- Recurse: piece-in-links -----")
   ;(log "   piece: " piece)
   ;(log "   links: " links)
   ;(log "   link-index: " link-index)
   ; (log "   test: " (>= link-index (.-length links)))
   (let [compressed-piece (compress-and-lower piece)
         link-href (get-link (aget links link-index))
         compressed-link  (compress-and-lower link-href)
         piece-in-link (find-piece-in-link compressed-piece compressed-link)]
     ; (log "   piecce-in-link: " piece-in-link)
     (if (>= link-index (- (.-length links) 1))
       nil
       (do
         ; (log piece-in-link)
         (if (= link-href nil)
           (piece-in-links piece links (inc link-index))
           (if piece-in-link
             (do
               (log "!! :) FOUND : " piece " in " (aget links link-index))
               (aget links link-index))
             (piece-in-links piece links (inc link-index)))))))))


(defn parse-text [text]                                            
  "TODO: if first chars of string aren't correct, don't run regex" 
  (log "Parsing For Id")
  (log text)
  ; TODO: This regex needs to be updated to look for the id inside the
  ; variants array
  (let [ regex-result (re-find #"variants\":\[\{\"id\":\d{11}" text) ]
    (if regex-result
      (subs regex-result 17))))


(defn log-seq                                                      
  ([stuff]                                                         
   (log (str "--------- log-seq: START ---------\n" (aget stuff 0)))                          
   (log-seq stuff 1))                                              
  ([stuff index]                                                   
   (if (< index  (.-length stuff))                                 
     (do
       (log (aget (aget stuff index) "href"))
       (log (aget (aget stuff index) "src"))                     
       (log (parse-text (aget (aget stuff index) "text")) )        
       ; (parse-text "this is a test")                             
       ; (log (aget (aget stuff index) "type"))                      
       (log (str (aget (aget stuff index) "text") "\n\n\n"))     
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
     (log "   in-links: " in-links)
     (if (> (count pieces) 0)
       (if in-links
         (do
           ; (log "found-pieces: " (conj found-pieces in-links))
           (find-desired-pieces links pieces (conj found-pieces in-links) 1))
         (find-desired-pieces links pieces found-pieces 1))
       [])))
  ([links pieces found-pieces piece-index]
   (let [in-links (piece-in-links (nth pieces piece-index) links)]
     (log "Finding desired pieces!")
     (log "   found-pieces" found-pieces)
     (if (< (inc piece-index) (count pieces))
       (do
         (if in-links
           (find-desired-pieces links pieces (conj found-pieces in-links) (inc piece-index))
           (find-desired-pieces links pieces found-pieces (inc piece-index))))
       found-pieces))))


(defn handler [response]
  (log "----- handler -----")
  (.log js/console (str  response)))

(defn error-handler [{:keys [status status-text]}]
  (log "----- error-handler -----")
  (.log js/console (str "something bad happened: " status " " status-text)))


(defn find-piece-ids
  ([found-pieces script-elements]
   (log "----- find-piece-ids -----")
   (log "   found-pieces: " found-pieces)
   (enable-console-print!)))
   ; (GET "https://stevieteam.com/collections/wholesale-packs/products/alice-top")))


(defn find-piece-id
  ([scripts]                                                         
   (log (str "--------- find-piece-id: START ---------\n" (aget scripts 0)))                          
   (log-seq scripts 1))                                              
  ([scripts index]                                                   
   (if (< index  (.-length scripts))                                 
     (do
       ; (log (aget (aget scripts index) "href"))
       ; (log (aget (aget scripts index) "src"))                     
       ; (parse-text "this is a test")                             
       ; (log (aget (aget scripts index) "type"))                      
       (log (str (aget (aget scripts index) "text") "\n\n\n"))     
       (if
           (log (parse-text (aget (aget scripts index) "text")))
         (log-seq scripts (inc index)))))))
     

(defn add-item-to-cart [item-id]
   (POST "https://stevieteam.com/cart/add.js"
         {:body (str "id=" item-id)
          :response-format :json
          :timeout 100}))

   

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
                       (find-desired-pieces link-elements pieces))
        piece-id-map (if found-pieces
                       (find-piece-ids found-pieces script-elements))]
    (if script-elements
      (log-seq script-elements))
    (log msg)
    (log-seq link-elements)
    (log "Pieces: " pieces)
    (log "Found Pieces: " found-pieces)
  
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
