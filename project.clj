(defproject jfxutils "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;;[de.codecentric.centerdevice/javafxsvg "1.0.0"]
				 ]
  :plugins [[lein-exec "0.3.6"]]

  ;; The normal behavior is to init the toolkit, so change this to
  ;; prevent toolkit init, typically when you know nothing requires
  ;; the toolkit at compile-time
  :jvm-opts ["-Dtoolkit-debug=true"
             "-Dtoolkit-compile-timeout=100"]
  
  :aliases {"go" ["do" "uberjar," "install"]}

  :aot :all
  :main jfxutils.core

  )
