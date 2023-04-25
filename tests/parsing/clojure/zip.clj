(require '[clojure.xml :as xml]
         '[clojure.zip :as zip])
     


(defn zip-str [s]
  (zip/xml-zip 
      (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(zip-str "<a href='test.com'/>")
[{:tag :a, :attrs {:href "test.com"}, :content nil} nil]

(println (xml/emit-element (zip/root [{:tag :a, :attrs {:href "test.com"}, :content nil} nil])))
