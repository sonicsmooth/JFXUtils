;; Implement binding fns
(ns jfxutils.bind
  (:use [jfxutils.core :exclude [-main]]))


(defn- update-targets!
  "Sends newval to all targets in tgts-map, passing the value through
  var-to-prop-fn when supplied in tgts-map.  If :no-action is also
  specified in tgts-map, and var-to-prop-fn returns such a value, then
  the value is not propagated to the targets, ie 'nothing happens'."

  ;; What's with no-action?  The problem is the user may set the
  ;; :no-action value to nil, so (:no-action var) returns nil, which
  ;; is indistinguishable from when the user does not set the value.
  ;; So we destructure the no-action key anyway, which may return nil,
  ;; and later on, if the key is found in the map, then we use the
  ;; value.

  [tgts-map newval]
  (doseq [[target {:keys [property var-to-prop-fn change-listener no-action]
                   :as properties}] tgts-map]

    ;; Removing listener is presumably a cheap operation so do it
    ;; regardless of the internal logic, ie there may be paths which
    ;; do not change the property value, in which case removing the
    ;; listener is not needed.
    (remove-listener! target property change-listener )

    (if var-to-prop-fn 
      ;; var-to-prop is specified and not nil, so we need to pay attention to no-action
      (let [v2pval (var-to-prop-fn newval)
            no-action-explicit? (find properties :no-action-val)]
        (if no-action-explicit?
          (do
            ;;(println "no-action is specified as" no-action)
            (if (= v2pval no-action)
              (println "var-to-prop-fn output matches" no-action ", so no action taken")
              (do
                ;;(println "var-to-prop-fn output <"v2pval "> doesn't match" no-action "so setting" target property "to" v2pval)
                (set-prop-val! target property v2pval))))
          (do
            ;;(println "No-action not specified")
            ;;(println "Taking action, setting" target property "to" v2pval)
            (set-prop-val! target property v2pval))

          ))
      ;; var-to-prop is nil or not specified, so just set property to
      ;; newval directly, ignoring :no-action-val
      (do
        ;;(println "No var-to-prop-fn, setting" target property "to" newval)
        (set-prop-val! target property newval)))
    
    
    ;; Adding listener is also presumably a cheap operation
    (add-listener! target property change-listener)))

(defn dirtify! [v]
  (swap! v assoc :dirty true))

(defn undirtify! [v]
  (swap! v assoc :dirty false))

(defn bind!
  "Bind a clojure var with one or more javafx properties.  If you want
  to bind JavaFX properties only, with no Clojure var, then do not use
  this function; use JavaFX Bindings instead.  Arguments are
  keyword-value pairs.  The required keywords
  are :var, :keyvec, :targets, and :property.  Everything else is
  optional.  :var and :keyvec must be specified outside the :targets
  map.  By default, each property is both a sink and a source of
  change events, but can be changed to sink only, aka :terminal.  This
  function returns the args as a map.

  :var is the var (currently only atoms are supported). 

  :keyvec is the vector of keys which access the value inside the var,
  same as get-in or assoc-in.

  :property is the property of the javafx Nodes to bind to.

  :targets is either a seq of javafx Nodes or a map with keys which
  are javafx Nodes.  If a seq, then each target Node shares the same
  binding parmeters.  If a map, then each target can have its own
  binding parmaeters.  If a :keyword is specified generally and also
  inside the :targets map, then the one in the targets map takes
  precedence.

  ;; validator not implemented... maybe it'll go away
  :validator is a function which returns true when value swapped into
  var is valid, and either returns false or raises an exception
  otherwise.  This argument cannot go in the :targets map.

  :range-fn is a combination range-clipping and validation function,
  called after the listener receives the new value, and after
  prop-to-var (if it exists).  range-fn takes one argument which is
  the proposed new value, and returns the final value to be swapped
  in.  This fn can return nil or whatever the no-action val is, or it
  can clip the value to be within some range, or anything else.

  :terminal is a bool indicating whether the property should only
  accept changes and cannot generate changes.  A ChangeListener will
  not be installed on this Node.  An example would be a read-only text
  field or label which reports the value of the var.  If you
  programmatically set this value outside the binding mechanism, the
  change will not propagate.

  :var-to-prop-fn is a function taking one arg and returning one
  value, which is called when the var changes, prior to being assigned
  to the property's value.  If nil or omitted, this fn is not called
  and the value of the var is written directly to the property.  This
  function may return the value of :no-action-val if it determines no
  state should change.

  :prop-to-var-fn is a function taking one arge and returning one
  value, which is called when the target's value changes, prior to
  being assigned to the var's value.  If nil or omitted, this fn is
  not called and the value of the property is written directly to the
  var.  This function may return the value of :noaction if it
  determines no state should change.

  :var-fn! is a function taking one arg, which is used to write the new
  value to the var, instead of the default swap!.  Its return value is
  ignored.  If prop-to-var-fn is specified, then the value passes
  through that function first before being passed to var-fn.

  :no-action-val is the value which will be returned from prop-to-var-fn
  or var-to-prop-fn which results in no changes being propagated. For
  example if this is set to :bluberries, then prop-to-var-fn and
  var-to-prop-fn must return :blueberries as an indictor to the
  binding system *not* to propagate the change.  This value can be
  specified as nil of course, in which case the fns must return nil
  for nothing to happen.  If :no-action-val is left unspecified, then
  :blueberries or nil or whatever will be the value propagated to
  the var and other targets.

  :init is the value which is swap!'ed or var-fn'd into var after
  everything is set up.  This is intended to initialize the targets
  with a known value.  The propagation of this value ignores the
  no-action-val value.  If :init is not specified, then the var remains at
  its previous value (including possibly not even existing in the
  atom), and the Node properties are left alone."

  [& kvps]
  (let [opts (apply hash-map kvps)
        ;; These are the required keys
        {:keys [var keyvec targets property]} opts

        ;; This is a map with the remaining keys
        optionals (dissoc opts :var :keyvec :targets :property)

        ;; Create a common map which occurs in all the values of the
        ;; targets-map.  The keys of the targets-map are either the
        ;; keys of targets if targets is a map, or the elements of
        ;; targets if targets is a seq.  The values of targets-map is
        ;; either the merge of each original value in targets with the
        ;; common map if targets is a map, or the common map if
        ;; targets is a seq.
        swapper! (or (:var-fn! optionals)
                     (fn [val]
                       ;;(println "swapping in" keyvec "->" val)
                       (swap! var assoc-in keyvec val)))
        common-map (merge {:property property} optionals)
        target-keys (if (map? targets) (keys targets) targets)
        target-vals (if (map? targets)
                      (map merge (repeat common-map) (vals targets))
                      (repeat (count target-keys) common-map))

        listnr (fn [properties]
                 (invalidation-listener
                  (let [newval (.getValue observable)
                        p2v (or (:prop-to-var-fn properties) identity)
                        rngfn (or (:range-fn properties) identity)
                        newreal (p2v newval)
                        newreal (rngfn newreal)
                        no-action-valid? (find properties :no-action-val)
                        dont-swap? (= newreal (:no-action-val properties))]
                    (if (and no-action-valid? dont-swap?)
                      (dirtify! var) ;; just refresh
                      ;; Otherwise, validate, then swap and refresh
                      (do (swapper! newreal)
                          (dirtify! var))))))
        
        invalidation-listeners (map listnr target-vals)
        target-vals (map #(merge %1 {:change-listener %2}) target-vals invalidation-listeners)
        targets-map (let [buncha-maps (map hash-map target-keys target-vals)]
                      (into {} buncha-maps))] ;; returns single map


    ;; Add watch to atom to update all the objects. jfx properties
    ;; won't call change listener if the value is programmatically set
    ;; to what is already there.  Clojure atom will call watch even if
    ;; it's the same value. targets-map is the unique key for the
    ;; watch; using something simple like :id didn't work because
    ;; later watches would overwrite earlier ones.

    ;; This whole thing assumes the value in the var is the "real"
    ;; value, whatever that is.  The properties in the Nodes are
    ;; somehow "representations" of this value.  For example the var
    ;; can be 1000um, but the property can be "1.0mm", "1.mm", "1mm",
    ;; "1e-3m", "1e-6km", etc.  Of course the var and the property can
    ;; be the same thing, such as a Color object, etc.
    (add-watch var targets-map
               (fn [_key _ref oldmap newmap]
                 ;; Update targets only when keyvec value is found.  Of course a legitimate
                 ;; nil value is indistinguishable from not being found, so we use a
                 ;; default return value which is extremely unlikely to be used 
                 ;;(println "updating-targets with" keyvec)
                 (let [unique (jfxutils.core/uuid)
                       newval (get-in newmap keyvec unique)]
                   (when-not (= newval unique)
                     (update-targets! targets-map newval)))))
    
    ;; Validator must be set outside this bind! call

    ;; For each target in tuple, add listener to property of target
    (doseq [[target {:keys [terminal property change-listener]}] targets-map]
      (when-not terminal ;; Only if :terminal is falsey
        (add-listener! target property change-listener)))

    ;; Set var to init val and force targets anyway, so things may get triggered twice
    ;; Be careful, as there is a distinction between an :init value deliberately set false,
    ;; and a non-existent :init value
    (when (find optionals :init)
      (swapper! (:init optionals)))
    opts))


;; Do something to validate values when they are set externally via swap
;; Do something to deal with snapping to nearest value, as in zoom slider
;; var-to-prop-fn and prop-to-var-fn return nil if they can't convert

#_(defn- slider [id]
  (jfxnew javafx.scene.control.Slider -20 20 0.0
          :block-increment 5
          :show-tick-marks true
          :show-tick-labels true
          :major-tick-unit 5
          :snap-to-ticks true
          :pref-width 350
          :id id))

#_(defn bind-test []
  (let [slider1 (long-slider (slider "slider1"))
        slider2 (long-slider (slider "slider2"))
        slider3 (long-slider (slider "slider3"))
        sliders [slider1 slider2 slider3]
        tf1 (javafx.scene.control.TextFormatter. (javafx.util.converter.LongStringConverter.))
        tf2 (javafx.scene.control.TextFormatter. (javafx.util.converter.LongStringConverter.))
        tf3 (javafx.scene.control.TextFormatter. (javafx.util.converter.LongStringConverter.))
        tfs [tf1 tf2 tf3]
        txt1 (jfxnew javafx.scene.control.TextField :text-formatter tf1)
        txt2 (jfxnew javafx.scene.control.TextField :text-formatter tf2)
        txt3 (jfxnew javafx.scene.control.TextField :text-formatter tf3)
        txts [txt1 txt2 txt3]
        nodes (concat sliders txts)
        targets (concat sliders tfs)
        state (atom nil)
        vb (jfxnew VBox :children nodes)

        ;; Anything with nil can be omitted, except :no-action
        mb (bind! 
            :init 0  ;; starting value
            :var state ;; atom 
            ;;:var-fn! nil ;; use this fn with new value instead of swap! to change var
            :var-fn! #(let [nval (Math/round (double %))]
                        (println "new val is" nval)
                        (swap! state assoc :field1 nval)) ;; make this so :field1 is not necessary
            :validator #(let [val (:field1 %)] ;; make this so :field1 not necessary
                          (if val
                            (>= (double val) 0.0)
                            true))
            :keyvec [:field1] ;; how to get-in the value from the var

            ;; Specify multiple targets and a common property
            :property :value
            :targets targets
            ;;:no-action-val nil      ;; The value returned by below fns indicating swap! should not occur
            ;;:var-to-prop-fn nil ;; Convert var value before setting property
            ;;:prop-to-var-fn nil ;; Convert property value before setting val
                  
            ;; Or specify multiple targets, properties, and fns
            #_:targets #_{slider1 {:property :value}
                          slider2 {:property :value}
                          slider3 {:property :value}
                          tf1 {:property :value}
                          tf2 {:property :value}
                          tf3 {:property :value}}



            )]

    ;; Need to change it so instead of :targets [{:target target,
    ;; :property prop}], it's :targets {target {:property prop, :var2prop v2p}}, etc.
    
    (stage vb [640 400])
    state))




#_(defn -main [& args]
  (def state (bind-test)))












