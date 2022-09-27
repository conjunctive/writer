(ns writer.core
  "Accumulate an output alongside a result.

  `Writer` is the abstract protocol.
  `writer` is the concrete representation as a record.

  `as-writer` is the macro for constructing a `writer`.
  `with-writer` is the macro for binding over an existing `writer`.

  `writer-rf` is a reducing function for binding one `writer` with another.
  `bi-writer-rf` is a reducing function for binding one `writer` with another associatively.
  `bind-writer-rf` is a higher-order reducing function for binding one `writer` with another.
  `trace-rf` is a higher-order utility reducing function for tracing reduction steps.

  `transcribe` is a transducer context that constructs a `writer` through parameterizing xforms with `tell!`.

  `pure` is a function that constructs a `writer` from a result value.
  `purely` is a higher-order function that applies a function over the result of a `writer`.

  `bind` is a transducer that applies a function over the result of a `writer` input,
  expecting the function to accept a result value and return a new `writer` instance.")

(defprotocol Writer
  "Accumulate an output alongside a result."
  (run-writer [w] "Unwrap a `Writer` as a [`result` `output`] pair.")
  (exec-writer [w] "Extract the `output` from a `Writer`.")
  (bind-writer
    [w1 w2]
    [w1 w2 f]
    "Combines the `output` of one `Writer` with another.
By default, only the `result` of `w2` is kept.
When a 2-arity function `f` is provided,
it will be passed the results of `w1` and `w2`,
acting as a point of resolution for determining a new result."))

(defrecord writer [result output]
  Writer
  (run-writer [w] [(:result w) (:output w)])
  (exec-writer [w] (:output w))
  (bind-writer [w1 w2] (assoc w2 :output (into (:output w1) (:output w2))))
  (bind-writer [w1 w2 f] (assoc w2
                                :output (into (:output w1) (:output w2))
                                :result (f (:result w1) (:result w2)))))

(defmacro as-writer
  "Constructs a `writer` from the `body` computation.
  Return value of `body` is the result of the `writer`.
  Calls to the `tell!` binding within `body` accumulate output."
  [tell! & body]
  `(let [output# (transient [])]
     (letfn [(~tell! [x#] (conj! output# x#))]
       (let [result# (do ~@body)]
         (->writer result# (persistent! output#))))))

(defmacro with-writer
  "Variation of `as-writer` where instead of pure construction,
  the output is combined with that of an existing `writer`.
  Effectively (bind-writer writer (as-writer ...))"
  [writer tell! & body]
  `(let [output# (transient (vec (exec-writer ~writer)))]
     (letfn [(~tell! [x#] (conj! output# x#))]
       (let [result# (do ~@body)]
         (->writer result# (persistent! output#))))))

(defn pure
  "Construct writer from `result`."
  {:inline (fn [result] `(->writer ~result []))}
  [result]
  (->writer result []))

(defn purely
  "Apply a function `f` over result of `writer`."
  [f]
  (fn [w]
    (bind-writer w (pure (f (:result w))))))

(defn writer-rf
  "Reducing function over writer.
  Associative over output, bind determines result."
  ([] (pure nil))
  ([w] (run-writer w))
  ([w1 w2] (bind-writer w1 w2)))

(defn bind-writer-rf
  "Higher-order reducing function over writer.
  Provided reducing function `rf` determines result.
  Zero-arity call to `rf` constructs the initial result.
  Two-arity call to `rf` resolves the accumulated and returned result.
  Output is associative."
  [rf]
  (fn binding-rf
    ([] (pure (rf)))
    ([w] (run-writer w))
    ([w1 w2] (bind-writer w1 w2 rf))))

(def bi-writer-rf
  "Reducing function over writer.
  Both output and result are associative."
  (bind-writer-rf conj))

(defn trace-rf
  "Higher-order utility reducing function.
  Traces calls to provided reducing function `rf`."
  [rf]
  (fn tracing-rf
    ([]
     (as-writer tell!
       (let [v (rf)]
         (tell! {:rf/arity 0, :rf/value v})
         v)))
    ([w]
     (run-writer
      (with-writer w tell!
        (let [acc (:result w)
              v (rf acc)]
          (tell! {:rf/arity 1, :rf/value v, :rf/args [acc]})
          v))))
    ([w x]
     (with-writer w tell!
       (let [acc (:result w)
             v (rf acc x)]
         (tell! {:rf/arity 2, :rf/value v, :rf/args [acc x]})
         v)))))

(defn bind [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result w]
       (rf (assoc result :output (into (exec-writer result) (exec-writer w)))
           (f (:result w)))))))

(defn transcribe
  "Variation of `transduce` that builds a `writer`.
  The `xform` argument is parameterized with a tell! function
  that accumulates output when called."
  ([tell->xform f coll]
   (transcribe tell->xform f (f) coll))
  ([tell->xform f init coll]
   (as-writer tell!
     (let [f ((tell->xform tell!) f)
           ret (if (instance? clojure.lang.IReduceInit coll)
                 (.reduce ^clojure.lang.IReduceInit coll f init)
                 (clojure.core.protocols/coll-reduce coll f init))]
       (f ret)))))

(comment
  ;; With `tell!` we can accumulate output alongside a result.

  (as-writer tell!
    (tell! :foo)
    (tell! :bar)
    (tell! :baz)
    "result")
  ;; =>
  {:result "result", :output [:foo :bar :baz]}

  ,)

(comment
  ;; Writer can be very useful within the context of transducers.
  ;; Instead of accumulating data by means of side-effects,
  ;; we can accumulate that data alongside our result purely.

  ;; The following example demonstrates what basic interactions
  ;; between a transducer and `Writer` look like.
  ;; Specifically, when your "step" function returns a `writer`,
  ;; and reducing functions are aware of that `writer` context.

  ;; Define a "step" function that returns the input number `n`,
  ;; while accumulating an output determining whether `n` is even or odd.
  (defn tell-even-or-odd [n]
    (as-writer tell!
      (if (odd? n)
        (tell! :odd)
        (tell! :even))
      n))

  (tell-even-or-odd 2)
  ;; =>
  {:result 2, :output [:even]}

  (sequence (map tell-even-or-odd)
            (range 1 10))
  ;; =>
  '({:result 1, :output [:odd]}
    {:result 2, :output [:even]}
    {:result 3, :output [:odd]}
    {:result 4, :output [:even]}
    {:result 5, :output [:odd]}
    {:result 6, :output [:even]}
    {:result 7, :output [:odd]}
    {:result 8, :output [:even]}
    {:result 9, :output [:odd]})

  ;; Bind the `writer` instances produced by `tell-even-or-odd`,
  ;; accumulating all output and returning the last result.
  (transduce (map tell-even-or-odd)
             writer-rf
             (range 1 10))
  ;; =>
  [9 [:odd :even :odd :even :odd :even :odd :even :odd]]

  ;; Bind the `writer` instances produced by `tell-even-or-odd`,
  ;; accumulating all output and each intermediary result.
  (transduce (map tell-even-or-odd)
             bi-writer-rf
             (range 1 10))
  ;; =>
  [[1 2 3 4 5 6 7 8 9]
   [:odd :even :odd :even :odd :even :odd :even :odd]]

  ;; Bind the `writer` instances produced by `tell-even-or-odd`,
  ;; accumulating all output and summing together the result.
  (transduce (map tell-even-or-odd)
             (bind-writer-rf +)
             (range 1 10))
  ;; =>
  [45 [:odd :even :odd :even :odd :even :odd :even :odd]]

  ;; Sometimes it can be helpful to see how a reduction was performed.
  ;; With `trace-rf`, we can visualize each reduction of the `Writer` bind over `+` as data.
  (transduce (map tell-even-or-odd)
             (trace-rf (bind-writer-rf +))
             (range 1 5))
  ;; =>
  [[10 [:odd :even :odd :even]]
   [{:arity 0,
     :value {:result 0, :output []}}
    {:arity 2,
     :value {:result 1, :output [:odd]},
     :args [{:result 0, :output []}
            {:result 1, :output [:odd]}]}
    {:arity 2,
     :value {:result 3, :output [:odd :even]},
     :args [{:result 1, :output [:odd]}
            {:result 2, :output [:even]}]}
    {:arity 2,
     :value {:result 6, :output [:odd :even :odd]},
     :args [{:result 3, :output [:odd :even]}
            {:result 3, :output [:odd]}]}
    {:arity 2,
     :value {:result 10, :output [:odd :even :odd :even]},
     :args [{:result 6, :output [:odd :even :odd]}
            {:result 4, :output [:even]}]}
    {:arity 1,
     :value [10 [:odd :even :odd :even]],
     :args [{:result 10, :output [:odd :even :odd :even]}]}]]

  ,)

(comment
  ;; Run `tell-even-or-odd` twice, producing double the output.
  (transduce (comp (map tell-even-or-odd)
                   (bind tell-even-or-odd))
             bi-writer-rf
             (range 1 10))
  ;; =>
  [[1 2 3 4 5 6 7 8 9]
   [:odd  :odd
    :even :even
    :odd  :odd
    :even :even
    :odd  :odd
    :even :even
    :odd  :odd
    :even :even
    :odd  :odd]]

  ;; Run `tell-even-or-odd` thrice, producing triple the output.
  ;; Sum the result.
  (transduce (comp (map tell-even-or-odd)
                   (bind tell-even-or-odd)
                   (bind tell-even-or-odd))
             (bind-writer-rf +)
             (range 1 10))
  ;; =>
  [45
   [:odd  :odd  :odd
    :even :even :even
    :odd  :odd  :odd
    :even :even :even
    :odd  :odd  :odd
    :even :even :even
    :odd  :odd  :odd
    :even :even :even
    :odd  :odd  :odd]]

  ;; Run `tell-even-or-odd`, increment the result,
  ;; then run `tell-even-or-odd` again.
  ;; Accumulate each intermediary result.
  ;; Either of the following expressions will do.
  (transduce (comp (map tell-even-or-odd)
                   (bind (comp pure inc))
                   (bind tell-even-or-odd))
             bi-writer-rf
             (range 1 10))
  (transduce (comp (map tell-even-or-odd)
                   (map (purely inc))
                   (bind tell-even-or-odd))
             (trace-rf bi-writer-rf)
             (range 1 10))
  ;; =>
  [[2 3 4 5 6 7 8 9 10]
   [:odd  :even
    :even :odd
    :odd  :even
    :even :odd
    :odd  :even
    :even :odd
    :odd  :even
    :even :odd
    :odd  :even]]

  ,)

(comment
  ;; Rather than having each xform construct writers independently,
  ;; you may also leverage a transducer context that provides access
  ;; to a single `tell!` function. This reduces overall allocations,
  ;; with the caveat of your xform being parameterized.
  (transcribe (fn [tell!]
                (comp (map (fn tell-even-or-odd [n]
                             (if (odd? n)
                               (tell! :odd)
                               (tell! :even))
                             n))
                      (map inc)))
              +
              (range 1 5))
  ;; =>
  {:result 14, :output [:odd :even :odd :even]}

  (transcribe (fn [tell!]
                (comp (map (fn tell-even-or-odd [n]
                             (if (odd? n)
                               (tell! :odd)
                               (tell! :even))
                             n))
                      (map inc)
                      (filter even?)
                      (map (fn tell-even [n]
                             (tell! :even)
                             n))))
              +
              (range 1 5))
  ;; =>
  {:result 6, :output [:odd :even :even :odd :even :even]}

  ,)

(comment
  ;; How did `+` sum together the values?
  (transduce (map #(* 10 %))
             (trace-rf +)
             (range 1 5))
  ;; =>
  [100
   [{:arity 0, :value 0}
    {:arity 2, :value 10, :args [0 10]}
    {:arity 2, :value 30, :args [10 20]}
    {:arity 2, :value 60, :args [30 30]}
    {:arity 2, :value 100, :args [60 40]}
    {:arity 1, :value 100, :args [100]}]]

  ,)

;; Local Variables:
;; eval: (define-clojure-indent (as-writer 1))
;; eval: (define-clojure-indent (with-writer 2))
;; End:
