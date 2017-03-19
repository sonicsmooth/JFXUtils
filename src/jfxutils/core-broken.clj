(ns jfxutils.core

  (:import (javafx.application Application)
           (javafx.beans.value ChangeListener)
           (javafx.scene Scene)
           (javafx.scene.layout BorderPane)
           (javafx.collections FXCollections)
           (javafx.event ActionEvent EventHandler)
           (javafx.scene.control TableView TableColumn TableCell
                                 Button TextArea TextField ComboBox MenuButton MenuItem  Label)

           (javafx.scene.paint LinearGradient RadialGradient ImagePattern CycleMethod Stop)
           (java.io PrintWriter)))



;;[javafx.scene.control TextArea Label TableView]

(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))


(defn classpath []
  (doseq [cp (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))]
    (println cp)))

;; This prevents thread from finishing when last window closes and
;; allows restarting of application
(javafx.application.Platform/setImplicitExit false)

(defn run-later* [f]
  (javafx.application.Platform/runLater f))

(defmacro run-later [& body]
  `(run-later* (fn [] ~@body)))

(defn run-now* [f]
  (let [result (promise)]
    (run-later
     (deliver result (try (f) (catch Throwable e e))))
    @result))

(defmacro run-now [& body]
  `(run-now* (fn [] ~@body)))


(defmacro jcvargs
  "Apply args to Java constructor.  This just wraps the first argument
  in parentheses and doesn't check whether it's actually a Java
  constructor, so you can actually pass any function or method with a
  list of arguments.

  If the last argument is any sort of collection, it gets passed to
  to-array.  If it does not eval to an actual vector, list, etc., the
  macro will fail.

  If you want to force the type of array that gets created, use
  jcvargst."
  [method & args]
  (if (coll? (last args))
    `(~method ~@(butlast args) (into-array ~(last args)))
    `(~method ~@args)))

(defmacro jcvargst 
  "Apply args to Java constructor.  This just wraps the 
  first argument in parenthese and doesn't check whether 
  it's actually a Java constructor, so you can actually pass
  any function or method with a list of arguments.

  The last argument is passed to to-array with type specified 
  by the previous argument.  If the last argument 
  is an expression that doesn't eval to a collection, the macro will fail.

  See jcvargs for simpler method."
  [method & args]
  (if (coll? (last args))
    (let [blargs (butlast args)]
      `(~method ~@(butlast blargs) (into-array ~(last blargs)  ~(last args))))
    `(~method ~@args)))


(defn add-children [container args]
  "Adds items to container's children"
  (let [children_list (.getChildren container)]
    (.addAll children_list (to-array args ))))

(defn make-linear-gradient [x0 y0 x1 y1 proportional cycle & stops]
  (let [stopslist (map #(Stop. %1 %2) (range (count stops)) stops)]
    (LinearGradient. (double x0) (double y0) (double x1) (double y1)
                     proportional cycle
                     (into-array stopslist))))

(defn make-create-call [klass]
  (let [klassname (second (.split (str klass) " "))]
    (eval (list (symbol (str klassname "/create"))))))

(defmacro jbuild [builder & args]
  `(.. (doto (make-create-call ~builder) ~@args) 
       (~'build)))


(defn camel-case
  "Convert camel-case-whatever string to camelCaseWhatever string.  If
  prop is supplied and non-false, returns CamelCaseWhatever"
  [s & [prop?]]
  (let [lower-words (.split (.toLowerCase s) "-")
        cased-words (map #(apply str (cons (Character/toUpperCase (first %)) (rest %)))
                         lower-words)]
    (if prop?
      (clojure.string/join cased-words)
      (clojure.string/join (cons (first lower-words) (rest cased-words))))))


(defn process-keyword*
  "Verifies kw arg, and returns symbol for adding children.
  Otherwise, returns symbol for setting property.  Throws exception if
  arg is not keyword."
  [kw]
  (when (not (keyword? kw)) (throw (Exception. "Supplied key must be keyword")))
  (cond (= :children kw) 'add-children
        (= :extra kw) nil
        ;; make ".setCamelCaseWhatever" symbol
        :else (symbol (str ".set" (camel-case (name kw) true)))))

(defn accum-kvps*
  "Process and accumulate key-value pairs, expanding anything inside an :extra clause"
  [ks]
  (loop [kvps ks
         out '()]
    (if (empty? kvps) out ;; finished, return accumulated symbols
        (let [[k v] (first kvps) ;; grab next key-value pair
              prockw (process-keyword* k)] ;; figure out what it needs to be
          (if (symbol? prockw)
            (recur (rest kvps) (conj out (list prockw v)))
            (recur (rest kvps) (concat out v) ))))))

(defmacro jfxnode
  "Makes any new object of type Node, passing constructor values and
  setting properies given as keyword-value pairs.  Each non-keyword
  argument after the first (node) argument is passed directly to the
  constructor, up until the first keyword.  After that, each keyword
  is converted to camelCase, and prepended with \".set\".  :children
  keyword is special case, and will add children vector in objects
  that support them.

  Contrived unrealistic Example:
  (make-object Scene bla1 bla2
  :width 100 
  :children [b1  b2]) 

  becomes 
  (let [s (new Scene bla1 bla2)]
  (.setWidth s 100)
  (add-children s [b1 b2])
  s) "
  [node & args]
  (let [[ctor-args kvpseq] (split-with #(not (keyword? %)) args)
        kvps (partition 2 kvpseq)]
    `(doto (new ~node ~@ctor-args)
       ~@(accum-kvps* kvps))))


(defmacro eventhandler
  "Reifies the eventhandler interface.
  args must be args list, eg [arg1 arg2], and body is like
  any other function.  \"this\" argument is silently inserted
  and made available to the function, but I don't think this works"
  [args & body]
  `(reify javafx.event.EventHandler
     (~'handle [this# ~@args]
       ~@body)))

(defmacro change-listener
  "Creates anonymous implementation of ChangeListener.
  arg must be vector of 4 variables: this, observable value, oldvalue,
  newvalue.  Body is the function to be executed when change occurs"
  [arg & body]
  `(reify javafx.beans.value.ChangeListener
     (~'changed ~arg ~@body)))


(defmacro add-listener
  "Adds listener to property of node.  propkw must be keyword"
  [node propkw listener]
  (let [propname (symbol (str "." (camel-case (name propkw)) "Property"))]
    `(.addListener (~propname ~node) ~listener)))



(defn make-console-scene
  ([] (make-console-scene ""))
  ([init_text]
   ;; Creates appropriate writers, etc., and returns PrintWriter and scene
   ;; printwriter should be bound to *out* later
   (let [ta (jfxnode TextArea :text init_text)
         output-stream (proxy [java.io.OutputStream] []
                         (write [buf offset length] ;; buf is bytes[] of 8192 long
                           (.appendText ta (String. (byte-array (take length buf))))))
         print-writer (PrintWriter. output-stream true)
         label (Label.)
         scene (Scene. (jfxnode BorderPane ta :bottom label))
         dimprint (fn [w h] (str "Width: " w ", Height: " h))
         listener (change-listener [_ _]
                                   (.setText label (dimprint (.getWidth scene) (.getHeight scene))))]
     (doto scene 
       (add-listener :height listener)
       (add-listener :width listener))
     [print-writer scene])))


(defn get-watches [^clojure.lang.IRef reference]
  (keys (.getWatches reference)))

(defn remove-watches! [atoms properties]
  (doseq [a atoms p properties]
    (remove-watch a p)))



#_(defmacro make-button [text func]
    `(jbuild ButtonBuilder
             (.text ~text)
             (.prefWidth 150)
             (.onAction (event-handler [evt]
                                      (~func)))))

(defmacro make-button [txt & body]
  `(jfxnode Button txt :on-action ~@body))

(defn obslist [& items]
  (FXCollections/observableArrayList (into-array items)))

