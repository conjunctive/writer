(ns writer.core)

(defmacro as-writer
  [tell! & body]
  `(let [output# (transient [])]
     (letfn [(~tell! [x#] (conj! output# x#) nil)]
       (let [result# (do ~@body)]
         (->writer result# (persistent! output#))))))

(defmacro with-writer
  [writer tell! & body]
  `(let [output# (transient (vec (exec-writer ~writer)))]
     (letfn [(~tell! [x#] (conj! output# x#) nil)]
       (let [result# (do ~@body)]
         (->writer result# (persistent! output#))))))
