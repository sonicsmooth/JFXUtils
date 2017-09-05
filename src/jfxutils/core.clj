(ns jfxutils.core
  (:require [clojure.string  :as s])
  (:use [clojure repl pprint reflect set])
  (:import   [com.sun.javafx.stage StageHelper]
             [de.codecentric.centerdevice.javafxsvg SvgImageLoaderFactory]
             [java.io PrintWriter]
             [java.net URL URI ]
             [javafx.application Platform]
             [javafx.animation RotateTransition Animation Interpolator]
             [javafx.beans.property ReadOnlyIntegerProperty]
             [javafx.beans.value ChangeListener ObservableValue]
             [javafx.collections FXCollections]
             [javafx.event ActionEvent EventHandler]
             [javafx.geometry Insets]
             [javafx.scene.control Button]
             [javafx.scene Group Scene Node Parent]
             [javafx.scene.shape Rectangle]
             [javafx.scene.image Image ImageView]
             [javafx.scene.layout BorderPane StackPane Background BackgroundFill CornerRadii]
             [javafx.scene.paint LinearGradient Stop CycleMethod Color]
             [javafx.scene.text Text]
             [javafx.stage Stage]
             [javafx.util Duration ]
             [java.nio.file Paths Path]))


(SvgImageLoaderFactory/install) ;; Allows us to use SVG files in JavaFX
(defonce force-toolkit-init (javafx.embed.swing.JFXPanel.))         

;; For some reason the following must be imported after initting the toolkit
(import  [javafx.scene.control Label TextArea])


;; This prevents thread from finishing when last window closes and
;; allows restarting of application
(javafx.application.Platform/setImplicitExit false)

(defn classpath
  "Shows the classpath.  Not really a jfx util, but useful anyway"
  []
  (doseq [cp (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))]
    (println cp)))

(defn resource-path
  "Returns Path object of location specified by string s"
  [s]
  (Paths/get (.toURI (clojure.java.io/resource s))))

(defn last-path
  "Returns last part of path, itself a path"
  [path]
  (if (= 0 (.getNameCount path))
    (.getRoot path)
    (.getName path (dec (.getNameCount path)))))


(defn path
  "Returns locator as java.nio.files.Path object.  Locator is string, URI, or URL."
  [locator]
  (cond (instance? Path locator) locator
        (instance? String locator) (Paths/get locator (into-array String []))
        (instance? URI locator) (Paths/get (str "file://" locator))
        (instance? URL locator) (path (.getPath locator))))




(defn run-later*
  "Returns nil"
  [f]
  (javafx.application.Platform/runLater f))

(defmacro run-later [& body]
  `(run-later* (fn [] ~@body)))

(defn run-now*
  "Returns result of f"
  [f]
  (let [result (promise)]
    ;; run-later returns nil, but deliver puts the return value of f
    ;; into the promise.  This makes sense since run-now is blocking,
    ;; but run-later returns immediately.
    (run-later
     (deliver result (try (f) (catch Throwable e e))))
    @result))

(defmacro run-now [& body]
  `(run-now* (fn [] ~@body)))

(defn capitalize-words [words]
  (map s/capitalize words))
(defn capitalize-rest-words [words]
  (cons (first words) (capitalize-words (rest words))))
(defn split-hyph [txt]
  (s/split txt #"-"))

(defn camel-case
  "Convert camel-case-whatever string to camelCaseWhatever string.  If
  prop is supplied and truthy, returns CamelCaseWhatever"
  ([txt]
   (camel-case txt false))
  ([txt capitalize-first?]
   (if capitalize-first?
     (->> txt split-hyph capitalize-words s/join)
     (->> txt split-hyph capitalize-rest-words s/join))))

;; Need to fix these to use reflection, so special cases are not
;; needed.  Somehow must determine which properties are observable
;; lists vs. other properties such as Boolean or other.  Can also
;; expand the various map protocols so we can access :fields of the
;; JFX object
(defn add-children! [container items]
  "Adds items to container's children.  Returns nil."
  (.addAll (.getChildren container) items))

(defn set-children! [container items]
  "Sets containers children as items.  Returns nil."
  (.setAll (.getChildren container) items))

(defn add-items! [container items]
  "Adds items to container's items.  Returns nil."
  (.addAll (.getItems container) items)) ;; need to-array?

(defn set-items! [container items]
  "Sets items in container's items.  Returns nil."
  (.setAll (.getItems container) items)) ;; need to-array?

(defn add-menus! [container items]
  "Adds items to container's menus.  Returns nil."
  (.addAll (.getMenus container) items))

(defn set-menus! [container items]
  "Sets container's menus as items.  Returns nil."
  (.setAll (.getMenus container) items))

(defn add-columns! [container items]
  "Adds items to container's columns.  Returns nil."
  (.addAll (.getColumns container) items))

(defn set-columns! [container items]
  "Sets container's columns as items  Returns nil."
  (.setAll (.getColumns container) items))

(defn add-tabs! [container items]
  "Adds items to container's tabs.  Returns nil."
  (.addAll (.getTabs container) items))

(defn set-tabs! [container items]
  "Sets items as container's tabs.  Returns nil."
  (let [obslist (.getTabs container)]
    (.setAll obslist items)))

(defn add-transforms! [container items]
  "Adds items to container's transforms.  Returns nil."
  (.addAll (.getTransforms container) items))

(defn set-transforms! [container items]
  "Sets container's transforms as items.  Returns nil."
  (.setAll (.getTransforms container) items))

(defn set-buttons! [container items]
  "Sets container's transforms as items.  Returns nil."
  (.setAll (.getButtons container) items))




(defn set-list!
  "Sets list specified by which-list keyword to items seq.  Returns
  whatever .setAll returns."
  [obj which-list items]
  (case which-list
    :children (.setAll (.getChildren obj) items)
    :menus (.setAll (.getMenus obj) items)
    :items (.setAll (.getItems obj) items)
    :columns (.setAll (.getColumns obj) items)
    :tabs (.setAll (.getTabs obj) items)
    :transforms (.setAll (.getTransforms obj) items)
    :buttons (.setAll (.getButtons obj) items))) ;; for SegmentedButton

(defn add-list!
  "Adds items to list specified by which-list keyword.  Returns
  whatever .addAll returns."
  [obj which-list items]
  (case which-list
    :children (.addAll (.getChildren obj) items)
    :items (.addAll (.getItems obj) items)
    :menus (.addAll (.getMenus obj) items)
    :columns (.addAll (.getColumns obj) items)
    :tabs (.addAll (.getTabs obj) items)
    :transforms (.addAll (.getTransforms obj) items)
    :buttons (.addAll (.getButtons obj) items))) ;; for SegmentedButton

(defmacro set-prop-val!*
  "Sets property value to value.  property arg must be keyword
  literal."
  [obj prop value]
  (let [prop-name# (str "." (camel-case (name prop)) "Property")]
    `(.set (~(symbol prop-name#) ~obj) ~value)))

(defn set-prop-val!
  "Sets property value to value.  property arg must evaluate to
  keyword.  Uses reflection."
  [obj prop value]
  (let [methodname (str "set" (-> prop
                                  name
                                  (camel-case true)))]
    (clojure.lang.Reflector/invokeInstanceMethod
     obj methodname (into-array [value]))))

(defmacro get-prop-val*
  "Gets property value.  property arg must be keyword literal."
  [obj prop]
  (let [prop-name# (str "." (camel-case (name prop)) "Property")]
    `(.get (~(symbol prop-name#) ~obj))))

(defn get-prop-val
  "Gets property value.  property arg must evaluate to keyword.  Uses
  reflection."
  [obj prop]
  (let [methodname (str "get" (-> prop
                                  name
                                  (camel-case true)))]
    (.. obj
        getClass
        (getMethod methodname nil)
        (invoke obj nil))))

(defmacro get-property*
  "Gets the property of obj.  Prop must be a keyword literal."
  [obj# prop#]
  (let [prop-name# (camel-case (name prop#) false)
        get-prop-fn# (symbol (str "." prop-name# "Property"))]
    `(~get-prop-fn# ~obj#)))

(defn get-property
  "Gets the property of obj.  Prop must evaluate to keyword literal,
  but doesn't have to be a keyword. Uses reflection."
  [obj prop]
  (let [methodname (-> prop
                       name
                       camel-case
                       (str "Property"))]
    (.. obj
        getClass
        (getMethod methodname nil)
        (invoke obj nil))))


(defn linear-gradient [x0 y0 x1 y1 proportional cycle & stops]
  (let [stopslist (map #(Stop. %1 %2) (range (count stops)) stops)]
    (LinearGradient. (double x0) (double y0) (double x1) (double y1)
                     proportional cycle
                     (into-array stopslist))))

(defn all-in?
  "Returns true only if all items in set keys are in set s"
  [s keys]
  (and (empty? (difference keys s))
       (not (empty? (intersection s keys)))))

(defn all-keys
  "Returns all keys from list of maps"
  [ms]
  (sort (apply union (map #(set (keys %)) ms))))


(defn nested-compare
  "Compares two maps based on vector of funcs.  For
  example (nested-comare x y [:first-name :last-name] compares x and y
  based on #(:first-name %) then #(:last-name %).  Returns 0 if equal,
  -1 if x is less than y, and +1 if x is greater than y"
  [x y sort-order]
  (loop [sort-order sort-order]
    (let [ff (first sort-order)
          rf (rest sort-order)
          c (if (and (map? ff)
                     (= :reverse (val (first ff))))
              (compare ((key (first ff)) y) ((key (first ff)) x))
              (compare (ff x) (ff y)))]
      (cond (= 0 c)
            (if (empty? rf) c
                (recur rf))
            :else c))))

(defn nested-comparator
  "Returns new nested comparator based on args.  Each arg is a
  function to be applied to item before comparison."
  [sort-order]
  (fn [x y]
    (nested-compare x y sort-order)))

(defn nested-sort-by
  "Returns collection sorted by applying arg funcs in sequence until a
  non-equal comparison appears."
  [coll sort-order]
  (sort-by identity (nested-comparator sort-order) coll))

(defn clean-sort-order
  "Removes map from any reverse argument. For example if sort-order
  is [:one :two {:three :reverse}], returns [:one :two :three]"
  [sort-order]
  (map #(if (map? %) (key (first %)) %) sort-order))

(defn move-sorted-keys-to-front
  "Takes allkeys which is a vector, and sort-order.  For each element
  in sort-order, removes it from allkeys, then adds the elements of
  sort-order back in, in the correct sequence.  For
  example (move-sorted-keys-to-front [10 20 3 4 5] [4 3] returns (4 3
  10 20 5)"
  [allkeys sort-order]
  (let [reduced-list (remove #(contains? (set sort-order) %) allkeys)]
    (concat sort-order reduced-list)))

#_(defn xmembers
    "Returns members of object based on flags.  See print-members."
    [obj & flags]
    (let [members1 (:members (clojure.reflect/reflect obj))
          members2 (map #(dissoc % :declaring-class) members1)
          members3 (if flags
                     (filter #(all-in? (:flags %) (set flags)) members2)
                     members2)
          members4 (sort-by :name members3)]
      members4))

(defn members
  "Returns members of object based on flags.  See print-members."
  [obj & flags]
  (letfn [(reduce-flags [mmbrs]
            (reduce #(apply conj %1 %2) (map :flags mmbrs)))]
    (as-> (->> obj clojure.reflect/reflect :members) mmbrs
      (->> mmbrs
           (map #(dissoc % :declaring-class))
           (filter #(intersection(:flags %) (if flags (set flags) (reduce-flags mmbrs))))))))


(defn print-members
  "Pretty-prints members of obj.  obj can be class or instance.  flags
  arg is vector of one or more of the following keywords: private,
  protected, static, final, synthetic.  If flags is nil, shows all
  members."
  [obj & flags]
  (let [sorted-members (apply members obj flags)]
    (print-table (all-keys sorted-members) sorted-members)))

(defn print-sorted-members
  "Pretty-prints members of obj.  obj can be class or instance.  flags
  arg is vector of one or more of the following keywords: private,
  protected, static, final, synthetic.  If flags is nil, shows all
  members."
  [obj sort-order & flags]
  (let [sorted-members (nested-sort-by sort-order (apply members obj flags))
        allkeys (all-keys sorted-members)
        display-keys (move-sorted-keys-to-front allkeys sort-order)]
    (print-table display-keys sorted-members)))

(defn fxparents
  "Returns list of all the parent classes."
  [klass]
  (take-while identity (iterate #(first (bases %)) klass)))


(defn class-or-class [o]
  (if (class? o) o (class o)))

(defn find-common-parent
  "Finds closest common parent of classes.  Assumes single inheritance
  of concrete classes only."
  [klass & klasses]
  ;; Assume Object is common, so reverse parents and go from there
  (let [klass (class-or-class klass)
        klasses (map class-or-class klasses)]
    (let [matcher (fn [a b] (map #(and (= %1 %2) %1) a b))
          matched (reduce matcher
                          (reverse (fxparents klass))
                          (map #(reverse (fxparents %)) klasses))]
      (last (filter identity matched)))))


(defn constructors
  "Returns the members of (members obj) which are constructors"
  [obj]
  (let [klass2 (if (instance? java.lang.Class obj) obj (class obj))
        sklass2 (second (.split (str klass2) " "))
        symklass2 (symbol sklass2)
        mmbrs (members obj)]
    (filter #(= symklass2 (:name %)) mmbrs)))

(defn has-var-constructor?
  "Returns true if obj has a vararg constructor."
  [obj]
  (let [c (constructors obj)
        fc (filter #(contains? (:flags %) :varargs) c)]
    (if (empty? fc) false true)))

(defn process-keyword*
  "Verifies kw arg, and returns symbol for adding children or columns.
  Otherwise, returns symbol for setting property.  Throws exception if
  arg is not keyword."
  [kw]
  (when (not (keyword? kw)) (throw (Exception. "Supplied key must be keyword")))
  (condp = kw ;; should be consistent with set- or add- !
    :children  'set-children!
    :menus  'set-menus!
    :items 'set-items!
    :columns 'set-columns!
    :tabs 'set-tabs!
    :transforms 'set-transforms!
    :buttons 'set-buttons!
    :extra nil
    ;; make ".setCamelCaseWhatever" symbol
    ;;:else
    (symbol (str ".set" (camel-case (name kw) true)))))

(defn accum-kvps*
  "Process and accumulate key-value pairs, expanding anything inside an :extra clause"
  [ks]
  (loop [kvps ks
         out '()]
    (if (empty? kvps) out ;; finished, return accumulated symbols
        (let [[k v] (first kvps) ;; grab next key-value pair
              prockw (process-keyword* k)] ;; figure out what it needs to be
          (if (symbol? prockw)
            (recur (rest kvps) (conj out (list prockw v))) ;; list adds parens to .setWhatever
            (recur (rest kvps) (concat out v))))))) 

(defmacro jfxnew
  "Makes any new object of klass, passing constructor values and
  setting properies given as keyword-value pairs.  Each non-keyword
  argument after the first (klass) argument is passed directly to the
  constructor, up until the first keyword.  After that, each keyword
  is converted to camelCase, and prepended with \".set\".  :children
  keyword is special case, and will add children vector in objects
  that support them.  :column keyword is special case, and will set
  columns vector in objects (ie TableView) that support them.  :extra
  keyword can be specified if following by a vector of one-arg
  functions, where the arg is the newly-created object.  If the
  constructor of the object supports var-args, then the last
  constructor argument is examined (before the first keyword).  If
  this argument is a vector, etc., then it is converted to an array
  via (into-array...).

  Contrived unrealistic Example:
  (jfxnew Scene bla1 bla2
  :width 100 
  :children [b1  b2]) 

  becomes 
  (let [s (new Scene bla1 bla2)]
    (.setWidth s 100)
    (add-children s [b1 b2])
    s) "
  [klass & args]
  (let [[ctor-args kvpseq] (split-with #(not (keyword? %)) args)
        var-ctor? (has-var-constructor? (eval klass))
        static-arg (if var-ctor?
                     (if (sequential? (last ctor-args))
                       (butlast ctor-args)
                       ctor-args)
                     ctor-args)
        var-arg (if var-ctor?
                  (if (sequential? (last ctor-args))
                    (last ctor-args)
                    nil)
                  nil)
        kvps (partition 2 kvpseq)]
    ;; ~@ctor-args
    (if var-arg
      `(doto (new ~klass ~@static-arg (into-array (find-common-parent ~@var-arg) ~var-arg))
         ~@(accum-kvps* kvps))
      `(doto (new ~klass ~@static-arg)
         ~@(accum-kvps* kvps)))))

(defmacro jfx-button
"Shortcut for (jfxnew Button name :on-action (event-handler [evt]
  body)).  The event variable evt is available inside the function
  body."
  [name & body]
  `(jfxnew Button ~name :on-action (event-handler [evt] ~@body)))

(defmacro event-handler
  "Returns new instance of object that implements EventHandler interface.
  args must be vector of one arg (the event), and body is like any
  other function.  The \"this\" object, which is the anonymous
  reification itself, is not available to the function"
  [args & body]
  `(reify javafx.event.EventHandler
     (~'handle [this# ~@args]
       ~@body)))

(defmacro change-listener
  "Creates anonymous implementation of ChangeListener.
  arg must be vector of 2 variables: oldvalue, newvalue.  Body is the
  function to be executed when change occurs. The variables 'this' and
  'observable' are available in the function body."
  [arg & body]
  `(reify javafx.beans.value.ChangeListener
     (~'changed [~'this ~'observable ~@arg] ~@body)))

(defmacro add-listener!*
  "Adds listener to property of node.  property must be
  keywordized-and-hyphenated JFX property, minus \"property\".  For
  example to add a ChangeListener mylistener to myCoolProperty
  belonging to Node mynode you would say (add-listener!
  mynode :my-cool mylistener).  You can create a ChangeListener
  with (change-listener a b c d)"
  [node property listener]
  (let [propname (symbol (str "." (camel-case (name property)) "Property"))]
    `(.addListener (~propname ~node) ~listener)))

(defn add-listener!
  "Adds listener to property of node.  Prop must evaluate to keyword
  property.  Uses reflection."
  [node property listener]
  (let [prop (get-property node property)]
    (.addListener prop listener)))

(defmacro remove-listener!*
  "Removes listener.  Just for symmetry."
  [node property listener]
  (let [propname (symbol (str "." (camel-case (name property)) "Property"))]
    `(.removeListener (~propname ~node) ~listener)))

(defn add-event-filter!
  "Adds event handler to a node for an event. You can create an
  EventHandler with (event-handler [evt] bla bla).  This function
  incldude for consistency/completeness, although invoking the javafx
  function directly would be shorter."
  [node evt handler]
  (.addEventFilter node evt handler))

(defmacro set-event-handler!
  "Sets event handler"
  [node event handler]
  (let [setfn (symbol (camel-case (str ".set-on-" (name event))))]
    `(~setfn ~node ~handler)))



(defmacro callback 
  "Reifies the callback interface
Example: (callback [column] (blablabla)).
No need to provide 'this' argument as the macro does this."
  [args & body]
  `(reify javafx.util.Callback
     (~'call [this# ~@args]
       ~@body)))

(defn console-scene
  ([out]
   ;; Creates appropriate writers, etc., and returns Scene
   ;; printwriter should be bound to *out* later
   (let [ta (TextArea.)
         output-stream (proxy [java.io.OutputStream] []
                         (write [buf offset length] ;; buf is bytes[] of 8192 long
                           (.appendText ta (String. (byte-array (take length buf))))))
         print-writer (PrintWriter. output-stream true)
         label (Label.)
         scene (Scene. (jfxnew BorderPane ta :bottom label))
         dimprint (fn [w h & args] (str "Width: " w ", Height: " h ", "  args))
         linecount (fn [] (count (clojure.string/split (.getText ta) #"\n")))
         status-bar-update (fn [] (run-later (.setText label (dimprint
                                                              (.getWidth scene)
                                                              (.getHeight scene)
                                                              (linecount) "lines"))))
         listener (change-listener [_ _] (status-bar-update))]
     ;; Attempt to return this thread's *out* to previous PrintWriter
     (alter-var-root #'*out* (constantly print-writer))
     (add-listener! ta :text listener)
     (add-listener! scene :height listener)
     (add-listener! scene :width listener)
     scene)))


(defn get-watches [^clojure.lang.IRef reference]
  (keys (.getWatches reference)))

(defn remove-watches!
  "First arg is coll of refs, args, vars, or agents.  Second arg is coll
  of properties to remove from watch"
  [mutable-things properties]
  (doseq [ref mutable-things, p properties]
    (remove-watch ref p)))

(defn remove-handler
  "Returns new EventHandler which removes watches from atoms.  Used
  for removing handlers upon window close event."
  [atoms keys]
  (event-handler [_] (remove-watches! atoms keys)))

(defn count-watches
  "Returns number of watches on an atom, ref, agent, or var"
  [mutable]
  (count (get-watches mutable)))

(defn has-parent?
"Returns true if parent is in class hierarchy of klass"
  [klass parent]
  (boolean (seq  (filter #(= parent %) (supers klass)))))

(defprotocol StageProtocol
  "Create and show a top-level Stage.  With one argument, arg0 must be
  an FX Node or Scene to display.  With two arguments, arg0 is an FX
  Node or Scene, following by arg1 as a [width height] vector.
  Alternately, with two arguments, arg0 is a width and arg1 is a
  height, in which case an empty Stage is created.  If this is called
  from outside the FX Thread, then a new Stage is created, displayed,
  and returned.  If this is called from within the FX Thread, then the
  Stage is created and returned, but not displayed."
  (stage [arg0] [arg0 arg1]))

(extend-protocol StageProtocol
  java.lang.Long ;; used for empty window of specified size
  (stage
    ([num0] (throw (Exception. "Must pass width and height to (window ...)")))
    ([width height]
     ;; Make a blank window with the given width and height
     ;; Window only shows immediately if we're not in the FX thread
     ;;(println "HERE 1!!!!!!!!")
     (if (Platform/isFxApplicationThread)
       (jfxnew Stage :width width, :height height)
       (run-now (doto (jfxnew Stage :width width, :height height) .show)))))

  javafx.scene.Scene ;; used for Scene window of specified or unspecified size
  (stage
    ([scene]
     ;; Make a window with the given scene, leaving width and height
     ;; unspecified.  Window only shows immediately if we're not in
     ;; the FX thread.
     ;;(println "HERE 2!!!!!!!!")
     (if (Platform/isFxApplicationThread)
       (jfxnew Stage :scene scene)
       (run-now (doto (jfxnew Stage :scene scene) .show))))
    ([scene [width height]]
     ;; Make a window with the given scene, width, and height 
     ;; Window only shows immediately if we're not in the FX thread
     ;;(println "HERE 3!!!!!!!!")
     (if (Platform/isFxApplicationThread)
       (jfxnew Stage :width width, :height height :scene scene)
       (run-now (doto (jfxnew Stage :width width, :height height, :scene scene) .show)))))
  
  javafx.scene.Node ;; used for Node window of specified or unspecified size
  (stage
    ([node]
     ;; Check if node can be put directly in scene, ie whether it
     ;; derives from Pane.  Put it in a Group if not, typically a
     ;; Canvas or similar.  Call 1-arg window again with no size
     ;; specification once we have a Scene."
     ;;(println "HERE 4!!!!!!!!")
     (if (has-parent? (class node) javafx.scene.Parent)
       (stage(Scene. node)) 
       (stage (Scene. (Group. [node]))))) 
    ([node [width height]]
     ;; Check if node can be put directly in scene, ie whether it
     ;; derives from Parent.  Put it in a StackPane if not, typically a
     ;; Canvas or similar.  Call 2-arg window again with size
     ;; specification once we have a Scene.
     ;;(println "HERE 5!!!!!!!!")
     (if (has-parent? (class node) javafx.scene.Parent)
       (stage (Scene. node) [width height]) ;; calls 2-arg Scene version with size
       (stage (Scene. (jfxnew StackPane :children [node])) [width height]))))) ;; calls 2-arg Scene version with size


(defn select-values [map keyseq]
  (vals (select-keys map keyseq)))

(defmacro printexp
  "Allows quick printing of expression literal and their evaluated value"
  [exp]
  `(let [exp-val# ~exp
         s# (with-out-str (pprint exp-val#))]
     (print ~(str exp ":") s#)
     (flush)
     exp-val#))

(defn observable
  "Returns ObservableArrayList version of argument"
  [coll]
  (FXCollections/observableArrayList (to-array coll)))


#_(defn uni-bind!
  ;; Create a one-way binding from Property to var via a
  ;; ChangeListener on the JFX Property.  This is used for
  ;; implementing the observer pattern.  The Property is associated
  ;; with a specific nested sub-element of the atom/ref map.

  ;; old comments: prop is the UI property. var is the atom/ref in
  ;; question. full-accesspath is a vector of arguments to assoc-in or
  ;; update-in or get-in. This does NOT add a watch on the var.  If
  ;; extra-fn is provided, it is executed after updating the var.
  [prop actionfn & [extra-fn]]
  (.addListener ^Node prop (change-listener [oldval newval]
                           (actionfn newval)
                           (when extra-fn (extra-fn)))))

(defn showstack []
  (try
    (throw (Exception. ""))
    (catch Exception e
      (.printStackTrace e *out*))))

(defn countstack []
  (try
    (throw (Exception. ""))
    (catch Exception e (count (.getStackTrace e)))))



(defmacro jfxmodify
  "Modifies node with args, where args is a sequence of key-value
  pairs such as those passed to jfxnew."
  [node & args]
  (let [[_unused_ kvpseq] (split-with #(not (keyword? %)) args)
        kvps (partition 2 kvpseq)
        sym  `(doto ~node 
                ~@(accum-kvps* kvps))
        me (macroexpand-1 sym)]
    (println me)
    sym))


(defn make-draggable!
  "Adds listeners to node to make it draggable.  Returns node."
  [node]
  (when (nil? (.getUserData node))
    (.setUserData node {}))
  ;; Capture states of mouse and node position
  (.setOnMousePressed node (event-handler [mouse-event]
                                          (let [oldud (.getUserData node)
                                                newmouse {:down-mouse-pos [(.getSceneX mouse-event) (.getSceneY mouse-event)]
                                                          :down-node-pos [(.getTranslateX node) (.getTranslateY node)]}
                                                newud (merge oldud newmouse)]
                                            (.setUserData node newud))))
  ;; Update node position
  (.setOnMouseDragged node (event-handler [mouse-event]
                                          (let [[sx sy] [(.getSceneX mouse-event) (.getSceneY mouse-event)]
                                                ud (.getUserData node)
                                                [ox oy] (:down-mouse-pos ud)
                                                [tx ty] (:down-node-pos ud)]
                                            (.setTranslateX node (+ (- sx ox) tx))
                                            (.setTranslateY node (+ (- sy oy) ty)))))
  node)

(defn make-clipped!
  "Adds listeners and clipping region to node so its children don't go
  outside the bounds.  This is mostly used for Pane.  Taken from
  http://news.kynosarges.org/2016/11/03/javafx-pane-clipping/"
  [^javafx.scene.Node node]
  (let [clip (Rectangle.)]
    (.setClip node clip)
    (add-listener! node :layout-bounds
                   (change-listener [ov nv]
                                    (.setWidth clip (.getWidth nv))
                                    (.setHeight clip (.getHeight nv))))
    node))

(defn set-layout!
  ;; Sets the layout position of the given node. Returns the node.
  [^javafx.scene.Node node xpos ypos]
  (doto node
    (.setLayoutX xpos)
    (.setLayoutY ypos)))

(defn get-layout [^javafx.scene.Node node]
  [(.getLayoutX node) (.getLayoutY node)])

(defn set-translate!
  ;; Sets the translate position of the given node. Returns the node.
  ;; Arguments are either two doubles, or a Point2D

  ([^javafx.scene.Node node ^double xpos ^double ypos]
   (doto node
     (.setTranslateX xpos)
     (.setTranslateY ypos)))

  ([^javafx.scene.Node node, ^javafx.geometry.Point2D pos]
   (doto node
     (.setTranslateX (.getX pos))
     (.setTranslateY (.getY pos)))))

(defn get-translate [^javafx.scene.Node node]
  [(.getTranslateX node) (.getTranslateY node)])

(defn set-center!
  ;; Sets the center position of the given node. Returns the node.
  ;; Arguments are either two doubles, or a Point2D

  ([^javafx.scene.Node node ^double xpos ^double ypos]
   (doto node
     (.setCenterX xpos)
     (.setCenterY ypos)))

  ([^javafx.scene.Node node, ^javafx.geometry.Point2D pos]
   (doto node
     (.setCenterX (.getX pos))
     (.setCenterY (.getY pos)))))

(defn get-translate [^javafx.scene.Node node]
  [(.getTranslateX node) (.getTranslateY node)])

(defn set-scale!
  ;; Sets the scale property of the given node.  This does not work on
  ;; Affine Transforms; the scale works on the node's center, not
  ;; necessarily its origin.  If one arge is given, this scale is
  ;; applied to both axes.  If two args are given, the scales are
  ;; applied to x and y axes, respectively.  Returns the node.
  ([^javafx.scene.Node node scale]
   (set-scale! node scale scale))
  ([^javafx.scene.Node node xscale yscale]
   (doto node
     (.setScaleX xscale)
     (.setScaleY yscale))))

(defn get-scale [^javafx.scene.Node node]
  [(.getScaleX node) (.getScaleY node)])


;; See https://stackoverflow.com/questions/25566146/multiple-arity-in-defmacro-of-clojure
(defn- set-xyfn!
  ;; Sets the X,Y versions of a node's property.  For example, to
  ;; change the translateX,Y of a node, you say (set-xy! mynode
  ;; :translate newlocation) where newlocation is of type Point2D.
  ;; Depending on the node, suitable values of property are
  ;; :translate, :layout, :scale, :center, :start, :end.  If the
  ;; property is *only* X, as in .setX, you leave out the property
  ;; argument: (set-xy! mynode newlocation)

  ;; All args are symbols!!, not the actual thing
  ([node newpt] ;; no propname, just .setX, .setY
   `(let [evnp# ~newpt]
      (doto ~node
        (.setX (.getX evnp#))
        (.setY (.getY evnp#)))))

  ([node property newpt]
   (let [propname (camel-case (name property) true)
         setXfn (symbol (str ".set" propname "X"))
         setYfn (symbol (str ".set" propname "Y"))]
     `(let [evnp# ~newpt]
        (doto ~node
          (~setXfn (.getX evnp#))
          (~setYfn (.getY evnp#)))))))

(defmacro set-xy!
  [node & args]
  (apply set-xyfn! node args))

;; See https://stackoverflow.com/questions/25566146/multiple-arity-in-defmacro-of-clojure
(defn- get-xyfn
  ;; Gets the Point2D(X,Y) versions of a node's property.  For
  ;; example, to get the translateX,Y of a node, you say (get-xy!
  ;; mynode :translate).  Depending on the node, suitable values of
  ;; property are :translate, :layout, :scale, :center, :start, :end.
  ;; If the property is *only* X, as in .setX, you leave out the
  ;; property argument: (get-xy! mynode)

  ;; All args are symbols!!, not the actual thing
  ([node] ;; no propname, just .setX, .setY
   `(Point2D. (.getX ~node) (.getY ~node)))

  ([node property]
   (let [propname (camel-case (name property) true)
         getXfn (symbol (str ".get" propname "X"))
         getYfn (symbol (str ".get" propname "Y"))]
     `(Point2D. (~getXfn ~node) (~getYfn ~node)))))

(defmacro get-xy
  [node & args]
  (apply get-xyfn node args))


(defmacro defn-memo [name & body]
  `(def ~name (memoize (fn ~body))))


(defn close-all-stages
  "Closes/hides windows.  Assumes implicit exit has been set properly
  for desired behavior"
  []
  (doseq [stage (StageHelper/getStages)]
    (.close stage)))

(defn get-stages
"Returns all top windows (aka Stages)"
  []
  (StageHelper/getStages))


(def default-icon-size 16)
(defn image [s]
  (Image. s))

(defn image-view
  "Icon is either a path string or Path object to a suitable image
  file, or an existing Image instance."
  [icon & [size]]
  (when icon
    (let [iv (cond (string? icon) (ImageView. (Image. icon)) ;; Image ctor takes a String
                   ;; This branch (using Path) is broken                
                   (instance? Path icon) (ImageView. (Image. (str icon))) ;; Image ctor takes a String
                   (instance? Image icon) (ImageView. icon))]
      (when size
        (.setFitWidth iv default-icon-size)
        (.setFitHeight iv default-icon-size))
      iv)))

(defn simple-vert-gradient
  "Creates a stretched vertical gradient with two colors"
  [c1 c2]
  (linear-gradient 0 0 0 1 true CycleMethod/NO_CYCLE c1 c2 ))
(defn simple-horiz-gradient
  "Creates a stretched horizontal gradient with two colors"
  [c1 c2]
  (linear-gradient 0 0 1 0 true CycleMethod/NO_CYCLE c1 c2 ))

(defn gradient-background
  "Makes smooth background with two colors."
  [direction c1 c2]
  (let [grad (condp = direction
                 :vertical (simple-vert-gradient c1 c2)
                 :horizontal (simple-horiz-gradient c1 c2))]
    (Background. (into-array
                  [(BackgroundFill. grad CornerRadii/EMPTY  Insets/EMPTY)]))))


(defn background-populate
  "Does the following in a new thread:
  1.  Calls prefn, which must return a value that can be applied to displayfn.
      If not provided, a default function is used.  This default function returns a default graphic node.
  2.  Calls displayfn on the FX thread, with result from prefn.
  3.  Calls longfn, a long-running function which must return a value that can be applied to displayfn.
  3.  Calls displayfn in FX thread, with result from longfn.
  4.  Returns nil."
  ([prefn longfn displayfn]
   (letfn [(background-thread []
             (let [result (prefn)]
               (run-later (displayfn result)))
             (let [result (longfn)]
               (run-later (displayfn result))))]
     (.start (Thread. background-thread))))
  ([longfn displayfn]  ;; Use standard graphic
   (background-populate #(image-view "icons/loading.gif") longfn displayfn)
   ;;(background-populate animated-wait longfn displayfn)
   ))

(defmacro fxtime
  "Displays elapsed time of body in FX thread.  Returns evaluated
  value of body."
  [& body]
  `(let [nt0# (java.lang.System/nanoTime)
         result# ~@body
         nt1# (java.lang.System/nanoTime)
         dtus# (/ (- nt1# nt0#) 1000.0)]
     (run-later (println "Elapsed time " '~@body ": " dtus# " us"))
     result#))

(defn when-done
  "Calls fun when fut(ure) is done, using value returned from future"
  [fut fun]
  (future (fun @fut)))

(defn animated-wait
  "Creates a rotating rectangle."
  []
  (let [rect (jfxnew Rectangle 100 100 :fill Color/RED)
        rotator (jfxnew
                 RotateTransition (Duration. 1000) rect
                 :to-angle 180
                 :auto-reverse false
                 :cycle-count Animation/INDEFINITE
                 :interpolator Interpolator/LINEAR)]
    (.play rotator)
    rect))

#_(defn map-replace
"Ignores old.  Returns new.  Used in swap! instead of reset!"
  [old new] new)


(defn uuid
  "Generates new random string"
  []
  (str (java.util.UUID/randomUUID)))


(defn subnodes
  "Returns a node's subnodes, such as children or items.  Returns nil
  if none found.  Uses reflection, so it's probably slow."
  [node]
  (let [klass (class node)]
    (if-let [method (or (try (.getMethod klass "getChildren" nil)
                             (catch NoSuchMethodException e))
                        (try (.getMethod klass "getItems" nil)
                             (catch NoSuchMethodException e))
                        (try (.getMethod klass "getButtons" nil)
                             (catch NoSuchMethodException e)))]
      (.invoke method node nil))))

(defn dfs-search
  ;;Depth first search.  Goal is value to look for.  Node is node that
  ;;contains one ore more values.  Getsubs returns next list of nodes.
  ;;getval returns value for a given node.
  [goal node getval]
  (letfn [(inner-search [node]
            (if (= goal (getval node))
              node
              (loop [nodes (subnodes node)]
                (if (empty? nodes) nil
                    (if-let [result (inner-search (first nodes))]
                      result
                      (recur (rest nodes)))))))]
    (inner-search node)))

(defn lookup-children
  "Does 'manual' lookup of chidren.  Used when node is not in Scene
  graph."
  [id node]
  (if (= (.getId node) id) node
      (dfs-search id node #(.getId %))))

(defn lookup
  "Lookup up a node with id string starting at Node node.  If
  direction is omitted, or :down, looks down the scene graph
  hierarchy.  If direction is :up, finds parent Scene first, then
  looks down, ie it will search in the entire screne graph for the id.
  If the node is in a scene graph, (ie in a Scene which is
  instantiated), then the function will use the JavaFX Node.lookup
  function.  If the node is not in a scene graph, then only :down (or
  omitting the third argument) will work, in which case a 'manual'
  search is done."
  ([id node] (lookup id node :down))
  ([id node direction]
   (let [full-id (str "#" id)]
     (if-let [scene (.getScene node)]
       (condp = direction
         :up (.lookup (.getScene node) full-id)
         :down (.lookup node full-id))
       ;; no scene, so search down
       (if (= direction :up)
         (throw (Exception. "Must specify :down or no direction when node is not in scene graph."))
         (lookup-children id node))))))



(defn assoc-in-pairs
 "Same as assoc-in, ecxept allows pairs of keys and values"
  [m ks v & args]
  (when (and args (odd? (count args)))
    (throw (Exception. "Args, if supplied, must be even")))
  (let [newm (assoc-in m ks v)]
    (if (empty? args)
      newm
      (let [arg-pairs (partition 2 args)
            reducer (fn [m [k v]] (assoc-in m k v))]
        (reduce reducer newm arg-pairs)))))

(defn -main [& args]
  (javafx.application.Platform/setImplicitExit true)
  (println "I'm a library.  Don't run me.")
  (run-now (let [stage (Stage.)
                 bp (BorderPane.)
                 scene (Scene. bp)
                 text (Text. "I'm a library.\nDon't run me")]
             (.setCenter bp text)
             (.setScene stage scene)
             (.show stage))))






