(ns jfxutils.ui
  (:require [jfxutils.core :as jfxc]
            [jfxutils.bind :as jfxb]))

(def ERROR-PSEUDO-CLASS (javafx.css.PseudoClass/getPseudoClass "error"))
(def OUT-OF-RANGE-PSEUDO-CLASS (javafx.css.PseudoClass/getPseudoClass "out-of-range"))
(def EDITING-PSEUDO-CLASS (javafx.css.PseudoClass/getPseudoClass "editing"))


(defn number-range-check
  "Checks if x is between mn and mx, inclusive.  If yes, then returns x.
  If x is outside the range, then the following occurs based on the
  value of action:
  nil            Returns nil
  :noaction      Returns x
  :throw         Throws a NumberFormatException
  :clip          Limits the number to mn or mx
  :clip-and-tell Limits the number to mn or mx, and wraps it in a vector"
  ([x mn mx]
   (if (nil? x) nil
     (and (>= x mn) (<= x mx) x)))
  ([x mn mx action]
   (if (nil? x) nil
       (if (or (> x mx) (< x mn))
         (condp = action
           nil nil
           :noaction x
           :throw (throw (NumberFormatException.))
           :clip (jfxc/clip x mn mx)
           :clip-and-tell (jfxc/clip x mn mx :tellme))
         x))))

(defmacro nullable-string-converter
  "Returns a proxy of StringConverter of the type given by numtype, eg
  Integer, Double, etc.  The presumption is that numtype is some
  numeric type, such that min and max can be used.  If the string is
  convertible to numtype and is within range, then that value is
  returned.  If it is not convertible, then nil is returned.  If it is
  convertible but out of range, then action is taken.  In this case,
  if action is :clip, then the value is limited to min and max.  If
  action is :noaction, then the value is returned as is.  If action
  is :nil, then nil is returned.  If min, max, and action are all
  omitted, then a 'simple' version is returned which uses the
  numtype's built-in MIN_VALUE and MAX_VALUE, and which returns nil
  with out-of-range or unparsable inputs."
  ([numtype min max action]
   (let [convfn ({'Long (symbol "long")
                  'Integer (symbol "int")
                  'Double (symbol "double")} numtype)
         cname (str "javafx.util.converter." (name numtype) "StringConverter")
         csym (symbol cname)
         newaction ({:noaction :noaction
                     :nil :throw ;; :nil returns nil when out of range, but the number fn requires :throw
                     :clip :clip } action)
         gs (gensym)]
     `(proxy [~csym] []
        (~'toString
         ([] (str "custom " ~cname)) ;; need zero-arity fn for repl display
         ([~gs] ;; always return a string
          (try (proxy-super ~'toString (~convfn ~gs))
               (catch NullPointerException e# "")
               #_(catch Exception e# ;; not sure what exception might be thrown
                   #_(println ~(str "nullable-" (clojure.string/lower-case numtype)
                                    "string-converter Exception in .toString from \"")
                              ~gs "\" of " (str (class ~gs)))))))
        (~'fromString [s#] ;; can return nil
         (try (number-range-check (proxy-super ~'fromString s#) ~min ~max ~newaction)
              (catch NumberFormatException e#
                nil))))))
  ([numtype]
   (let [num-min (fn [numtype] (symbol (str numtype "/MIN_VALUE")))
         num-max (fn [numtype] (symbol (str numtype "/MAX_VALUE")))]
     `(nullable-string-converter ~numtype ~(num-min numtype) ~(num-max numtype) :nil))))

;; Make easy-to-use functions for the general case
(defn integer-nullable-string-converter
  ([] (nullable-string-converter Integer))
  ([min max action] (nullable-string-converter Integer min max action)))
(defn long-nullable-string-converter
  ([] (nullable-string-converter Long ))
  ([min max action] (nullable-string-converter Long min max action)))

;; This one returns number strings according to the given format
(defn double-nullable-string-converter
  ([^String fmt]
   (double-nullable-string-converter Double/MIN_VALUE Double/MAX_VALUE :nil fmt))
  ([min max action fmt]
   (proxy [javafx.util.converter.DoubleStringConverter] []
     (toString
       ([] (proxy-super toString))
       ([n] (try (format fmt (double n))
                 (catch NullPointerException e ""))))
     (fromString [s]
       (let [newaction ({:noaction :noaction
                         :nil :throw ;; :nil returns nil when out of range, but the number fn requires :throw
                         :clip :clip
                         } action)]
         (try (number-range-check
               (proxy-super fromString s) (double min) (double max) newaction)
              (catch NumberFormatException e
                nil)))))))

(defn setup-generic-checkbox! [state lu & specs]
  (doseq [spec specs]
    (let [cb (lu (:checkbox spec))
          argmap {:var state
                  :keyvec (:keyvec spec)
                  :property :selected
                  :targets [cb]}
          argmap (if (find spec :init)
                   (assoc argmap :init (:init spec))
                   argmap)
          arglist (apply concat argmap)]
      (apply jfxb/bind! arglist))))

(defn setup-generic-color-selector! [state lu & specs]
  "Args is list of maps with :color, :keyvec, init"

  (doseq [spec specs]
    (let [argmap {:var state
                  :keyvec (:keyvec spec)
                  :property :value
                  :no-action-val nil
                  :targets [(lu (:picker spec))]}

          argmap (if-let [v (:init spec)]
                   (assoc argmap :init v)
                   argmap)
          argmap (if-let [v (:var-to-prop-fn spec)]
                   (assoc argmap :var-to-prop-fn v)
                   argmap)
          argmap (if-let [v (:prop-to-var-fn spec)]
                   (assoc argmap :prop-to-var-fn v)
                   argmap)

          arglist (apply concat argmap)]
      (apply jfxb/bind! arglist))))


(defn err-range-listener! [converter in-range?]
  "Returns change listener which converts the 'newval' string and runs
  it through the in-range? fn. The in-range? fn returns true if the
  converted value is within range."
  (jfxc/change-listener ;; newval is a string, converted to nil or good, no Exception
   (let [newval-from-string (.fromString converter newval)
         textfield (.getBean observable)]
     (.pseudoClassStateChanged textfield ERROR-PSEUDO-CLASS (nil? newval-from-string))
     (.pseudoClassStateChanged textfield OUT-OF-RANGE-PSEUDO-CLASS (not (in-range? newval-from-string))))))

(defn enable-editing-pseudostate [textfield]
  (jfxc/event-handler [event]
                 (let [code (.getCode event)]
                   (when (or (.isDigitKey code)
                             (.isKeypadKey code)
                             (.isLetterKey code)
                             (.isWhitespaceKey code)
                             (= javafx.scene.input.KeyCode/BACK_SPACE code)
                             (= javafx.scene.input.KeyCode/DELETE code))
                     (.pseudoClassStateChanged textfield EDITING-PSEUDO-CLASS true)))))

;; call TextField's on-action, same as user pressing enter
(def txt-defocus-listener
  (jfxc/change-listener
   (let [bean (.getBean observable)]
     (when (false? newval)
       (when-let [action (.getOnAction bean)]
         (.handle action (javafx.event.ActionEvent.)))
       (.pseudoClassStateChanged bean EDITING-PSEUDO-CLASS false)))))


(defn setup-number-textfield! 
  "Sets up properties and listeners on textfield so it shows green
  while editing proper numerical values, yellow while editing
  out-of-range values, and red while editing nonparsable text.
  textfield is self-described.  Lower and upper set the valid range.
  convspec is either an instance of a StringConverter, or a numeric
  Type such as Long.  Rng is the range check function. Returns
  textfield."
  ([textfield cnvspec rngfn]
   (let [numtype? (not (instance? javafx.util.StringConverter cnvspec))
         converter (condp = cnvspec
                     Integer (nullable-string-converter Integer );; (integer-nullable-string-converter)
                     Long (nullable-string-converter Long)       ;; (long-nullable-string-converter)
                     Double  (double-nullable-string-converter "%.2f")
                     cnvspec)
         formatter (and numtype? (javafx.scene.control.TextFormatter. converter))]
     (when numtype?
       (jfxc/set-prop-val! textfield :text-formatter formatter))
     (jfxc/add-listener! textfield :focused txt-defocus-listener)
     (jfxc/add-listener! textfield :text (err-range-listener! converter rngfn))
     (.setOnKeyPressed textfield (enable-editing-pseudostate textfield))
     textfield))
  ([textfield cnvspec lower upper]
   (let [rngfn #(number-range-check % lower upper)]
     (setup-number-textfield! textfield cnvspec rngfn))))

#_(defn setup-generic-text [state lu & specs]
  "Args is list of maps"
  (doseq [spec specs]
    (let [[lower upper] (:range spec)
          textfield (lu (:textfield spec))]
      (setup-number-textfield! textfield (:type spec) lower upper)
      (jfxb/bind! :var state, :init (:init spec), :keyvec (:keyvec spec)
                  :property :value
                  :no-action-val nil
                  :range-fn #(number-range-check % lower upper :clip)
                  :targets [(jfxc/get-prop-val textfield :text-formatter)]))))

(defn setup-generic-slider-and-text [state lu & specs]
  "Args is list of maps such as:
{:slider 'sl-zoom-ppu'
 :textfield 'tf-zoom-ppu'
 :keyvec [:zoom-ppu]
 :type Long % or Double
 :var-fn! var-fn!
 :snap-to 0.1 ;; defaults to 1.0
 :range [0 20]
 :init init 
 :major-tick-unit 50
 :minor-tick-count 9
 :show-tick-marks true
 :show-tick-labels true
 :block-increment 5
}"

  (doseq [spec specs]
    (let [[lower upper] (:range spec)
          slider (lu (:slider spec))
          textfield (lu (:textfield spec))
          p2vmap (condp = (:type spec)
                   Long {:prop-to-var-fn long}
                   Double {})]
      (setup-number-textfield! textfield (:type spec) lower upper)
      (when-let [x lower] (.setMin slider x))
      (when-let [x upper] (.setMax slider x))
      (when-let [x (:major-tick-unit spec)] (.setMajorTickUnit slider x))
      (when-let [x (:minor-tick-count spec)] (.setMinorTickCount slider x))
      (when-let [x (:show-tick-marks spec)] (.setShowTickMarks slider x))
      (when-let [x (:show-tick-labels spec)] (.setShowTickLabels slider x))
      (when-let [x (:block-increment spec)] (.setBlockIncrement slider x))
      (when (= (:type spec) Long) (jfxutils.core/long-slider slider (:snap-to spec 1)))
      (when (= (:type spec) Double) (jfxutils.core/double-slider slider (:snap-to spec 1.0)))

      ;; We can't rely on the slider's clipping/limiting functionality
      ;; since the slider doesn't let us know the setValue has been clipped
      (let [argmap {:var state
                    ;;:init (or (:init spec) 0)
                    :keyvec (:keyvec spec)
                    :property :value
                    :no-action-val nil
                    :range-fn #(number-range-check % lower upper :clip)
                    :var-fn! (:var-fn! spec)
                    :targets {(jfxc/get-prop-val textfield :text-formatter) {} ;; no prop-to-var-fn because converter returns proper value or nil
                              slider p2vmap}}
            argmap (if (find spec :init)
                     (assoc argmap :init (:init spec))
                     argmap)
            arglist (apply concat argmap)]
        
        (apply jfxb/bind! arglist)))))



(defn integer-spinner-value-factory
  "Returns an instance of
  SpinnerValueFactory.IntegerSpinnerValueFactory.  The spinner will
  stay within min and max, but the number converter will return any
  proper integer, or nil if the string is unreadable."
  [min max]
  (let [mn Integer/MIN_VALUE
        mx Integer/MAX_VALUE]
    (doto (javafx.scene.control.SpinnerValueFactory$IntegerSpinnerValueFactory. min max)
      (.setConverter (nullable-string-converter Integer mn mx :nil)))))

