(ns chromex-sample.content-script.core
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [cljs.core.async :refer [<!]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.protocols :refer [post-message!]]
            [chromex.ext.runtime :as runtime :refer-macros [connect]]
            [ajax.core :refer [GET POST]]))

(def found-pieces-data [])

(defn handler [response piece-name]
  (log "----- handler -----")
  (log "piece-name: " piece-name)
  ; (.log js/console (str response))
  (let [piece-name (parse-out-name (str response))
        piece-id (parse-out-id (str response))
        piece-data {:piece-name piece-name :piece-id piece-id}]
    (conj found-pieces-data piece-data)
    (add-item-to-cart piece-data)))


(defn error-handler [{:keys [status status-text]}]
  (log "----- error-handler -----")
  (.log js/console (str "something bad happened: " status " " status-text)))


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
         compressed-link (compress-and-lower (get-link (aget links 0)))
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
               (get-link (aget links 0))
               (piece-in-links piece links 1))))))))
  ([piece links link-index]
   "TODO: The way the function recurses is incorrect. Always misses last link"
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


(defn parse-out-id [text]                                            
  "TODO: if first chars of string aren't correct, don't run regex" 
  ; (log "Parsing For Id")
  ; (log text)
  ; TODO: This regex needs to be updated to look for the id inside the
  ; variants array
  (let [ regex-result (re-find #"variants\":\[\{\"id\":\d{11}" text) ]
    (if regex-result
      (do
        ; (log "regex-result: " (subs regex-result 17))
        (subs regex-result 17)))))

(defn parse-out-name [text]
  (let [ regex-result (re-find #"\"og\:title\" content=\".*\"" text) ]
    (if regex-result
      (do 
        (subs regex-result 20 (- (count regex-result) 1))))))


;; (defn log-seq                                                      
  ;; ([stuff]                                                         
  ;;  (log (str "--------- log-seq: START ---------\n" (aget stuff 0)))                          
  ;;  (log-seq stuff 1))                                              
  ;; ([stuff index]                                                   
  ;;  (if (< index  (.-length stuff))                                 
  ;;    (do
  ;;      (log (aget (aget stuff index) "href"))
  ;;      (log (aget (aget stuff index) "src"))                     
  ;;      (log (parse-out-id (aget (aget stuff index) "text")) )        
  ;;      ; (parse-out-id "this is a test")                             
  ;;      ; (log (aget (aget stuff index) "type"))                      
  ;;      (log (str (aget (aget stuff index) "text") "\n\n\n"))     
  ;;      (log-seq stuff (inc index)))
  ;;    (log "---------- log-seq: COMPLETE ---------"))))                             


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
   ; (log "   pieces: " pieces)
   ; (log "   links: " links)
   (let [found-pieces []
         in-links (piece-in-links (nth pieces 0) links)
         page (GET (get-link in-links) {:handler handler :error-handler error-handler})]
     ; (log "Finding desired pieces!")
     ; (log "   count pieces: " (count pieces))
     (if (> (count pieces) 0)
       (if in-links
         (do
           ; (log "   found link!")
           ; (log "   found-pieces: " found-pieces)
           ; (log "   first pieces: " (first pieces))
           ; (log "   :piece-name (first pieces): " {:piece-name (first pieces)})
           ; (log "   get: " (get {:piece-name (first pieces)} :piece-name))
           (find-desired-pieces links pieces (conj found-pieces {:piece-name (first pieces)
                                                                 :piece-id ""}) 1))
         (do
           (log "   didn't find link!")
           (find-desired-pieces links pieces found-pieces 1)))
       [])))
  ([links pieces found-pieces piece-index]
   (let [in-links (piece-in-links (nth pieces piece-index) links)
         page (GET (get-link in-links) {:handler handler :error-handler error-handler})]
     ; (log "Finding desired pieces R!")
     ; (log "   found-pieces:" found-pieces)
     (if (< (inc piece-index) (count pieces))
       (do
         (if in-links
           (find-desired-pieces links pieces (conj found-pieces {:piece-name (nth pieces piece-index)}) (inc piece-index))
           (find-desired-pieces links pieces found-pieces (inc piece-index))))
       (do
         (log "----- found-desired-pieces -----\n" found-pieces "\n\n")
         ; (log "(first found-pieces): " (first found-pieces))
         ; (log "(get (first found-pieces) :piece-name): " (get (first found-pieces) :piece-name))
         found-pieces)))))


(defn find-piece-id
  ([piece-name scripts]                                                         
   (let [ text (aget (aget scripts 0) "text")
         piece-id (if text
                    (parse-out-id text)
                    nil)]
     (log "----- find-piece-id -----")
     (if piece-id
       piece-id
       (if (< 1 (.-length scripts))
         (find-piece-id piece-name scripts 1)))))
  ([piece-name scripts index]
   (let [ text (aget (aget scripts index) "text")
         piece-id (if text
                    (parse-out-id text)
                    nil)]
     ; (log (str "Index: " index " Max: " (.-length scripts)))
     (if (< (inc index)  (.-length scripts))
       (if piece-id
         piece-id
         (find-piece-id piece-name scripts (inc index)))
       nil))))


(defn find-piece-ids
  ([found-pieces script-elements]
   (log "----- find-piece-ids -----")
   (log "   found-pieces: " found-pieces)
   (let [ piece-id (find-piece-id (first found-pieces) script-elements) ]
     (if piece-id
       (log "Found piece-id: " piece-id)
       (log "Failed to find piece-id"))
     (if (> (count found-pieces) 1)
       (if piece-id
         (do
           (log "Adding :id to found-pieces: " (conj { :piece-name (first found-pieces) :piece-id piece-id } (rest found-pieces)))
           (find-piece-ids (conj { :piece-name (first found-pieces) :piece-id piece-id } (rest found-pieces)) script-elements 1))
         (find-piece-ids found-pieces script-elements 1))
       (if piece-id
         (do
           (log "Returning - Added: " {:piece-name (first found-pieces) :piece-id piece-id} " to found-pieces")
           [{:piece-name (first found-pieces) :piece-id piece-id}])
         (do
           (log "Found No Ids" found-pieces "\n\n\n")
           found-pieces)))))
     

  ([found-pieces script-elements index]
   (let [piece-id (find-piece-id (nth found-pieces index) script-elements) ]
     (log "----- find-piece-ids r -----")
     (log "found-pieces r: " found-pieces)
     (if piece-id
       (log "Found piece-id: " piece-id)
       (log "Failed to find piece-id"))
     (if (< index (.-length found-pieces))
       (if piece-id
         (do
           (log "Adding :id to found-pieces r:" (conj { :piece-name (nth found-pieces (inc index)) :piece-id piece-id }))
           (find-piece-ids (conj { :piece-name (nth found-pieces (inc index)) :piece-id piece-id }
                                 (rest found-pieces)) script-elements 1))
         (find-piece-ids found-pieces script-elements (inc index)))
         (do
           (log "----- find-piece-ids return r-----" found-pieces "\n\n\n")
           found-pieces)))))
   ; (GET "https://stevieteam.com/collections/wholesale-packs/products/alice-top")))


(defn add-item-to-cart [item]
  (log "----- add-item-to-cart -----")
  (let [id (get item :piece-id)]
  (log "   item: " item)
  (log "Adding " id  " to cart!")
  (POST "https://stevieteam.com/cart/add.js"
        {:body (str "id=" id)
         :response-format :json
         :timeout 1000})))


(defn add-items-to-cart
  ([piece-id-map-list]
  ;; (if piece-id-map-list (apply add-item-to-cart piece-id-map-list))))
  (log "----- add-items-to-cart -----")
   (log "piece-id-map-list: " piece-id-map-list)
   (if (> (count piece-id-map-list) 0)
     (do
       (add-item-to-cart (first piece-id-map-list))
       (add-items-to-cart piece-id-map-list 1))))
  ([piece-id-map-list index]
   (log "----- add-items-to-cart R -----")
   (log "piece-id-map-list: " piece-id-map-list)
   (if (< (count piece-id-map-list) index)
     (do
       (add-item-to-cart (get  piece-id-map-list "piece-id"))
       (add-items-to-cart piece-id-map-list (inc index))))))       

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
  (enable-console-print!) ; Set print default
  (let [script-elements (.getElementsByTagName js/document "script")
        link-elements (.getElementsByTagName js/document "a")
        link-count (.-length link-elements)
        title (.-title js/document)
        msg (str "CONTENT SCRIPT: document '" title "' contains " link-count " link tags.")
        found-pieces (if pieces
                       (find-desired-pieces link-elements pieces))]
        ; piece-id-map-list (if found-pieces
                            ; (find-piece-ids found-pieces script-elements))]
    ; (if script-elements
      ; (log-seq script-elements))
    (log msg)
    ; (log-seq link-elements)
    (if pieces (log "Pieces: " pieces))
    (if found-pieces (log "Found Pieces: " found-pieces))
    ; (if piece-id-map-list (log "Piece Id Map: "   piece-id-map-list ))
    ; (add-items-to-cart piece-id-map-list)
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
