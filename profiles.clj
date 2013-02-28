{:user
 {:plugins [[lein-swank "1.4.4"]
            ;;             [lein-ritz "0.6.0"]
            [lein-cljs "0.2.2"]
            [lein-difftest "1.3.7"]
            [lein-marginalia "0.7.1"]
            [lein-pprint "1.1.1"]
            [lein-midje "2.0.4"]
            [lein-noir "1.2.1"]
            [com.stuartsierra/lazytest "1.2.3"]
            [org.cloudhoist/pallet-lein "0.5.0"]
            [pallet/lein-template "0.2.3"]]
  ;; :dependencies [[ritz/ritz-nrepl-middleware "0.6.0"]]
  ;; :repl-options {:nrepl-middleware
  ;;                [ritz.nrepl.middleware.javadoc/wrap-javadoc
  ;;                 ritz.nrepl.middleware.simple-complete/wrap-simple-complete]}
  ;; :hooks [ritz.add-sources]
  :search-page-size "30"
  :repositories {"stuart"              "http://stuartsierra.com/maven2"
                 "googleapis"          "http://mavenrepo.google-api-java-client.googlecode.com/hg/"
                 "google-api-services" "http://google-api-client-libraries.appspot.com/mavenrepo"
                 "sonatype"            "http://oss.sonatype.org/content/repositories/releases"
                 "sonatype-snapshots"  "http://oss.sonatype.org/content/repositories/snapshots"}}}
