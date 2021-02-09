;Nathan Gillette
;Clojure Main Project

(ns main.core)
(declare simplify)
(def p1 '(and x (or x (and y (not z)))))
(def p2 '(and (and z false) (or x true)))
(def p3 '(or true a))
(defn and-simplify
  [andArgs]
  (let [noTrue (distinct (filter (fn [data] (not(true? data)))andArgs))]        ;Returns a list without any true statements
    ;The filter command removes any duplicate statements
    (cond
      (some false? noTrue)false                                               ;Returns false if the list simplifies to false
      (= (count noTrue) 1)true                                                ;Returns true if the list simplifies to true
      (= (count noTrue) 2) (first(rest noTrue))                                 ;Returns the simplified list
      :else noTrue)))


(defn or-simplify
  [orArgs]
  (let [noFalse (distinct (filter (fn [data] (not(false? data)))orArgs))]        ;Returns a list without any true statements
    ;The filter command removes any duplicate statements
    (cond
      (some true? noFalse)true                                               ;Returns false if the list simplifies to false
      (= (count noFalse) 1)false                                                ;Returns true if the list simplifies to true
      (= (count noFalse) 2) (first(rest noFalse))                                 ;Returns the simplified list
      :else noFalse)))

(defn not-simplify
  [notArgs]
  (cond
    (some true? notArgs) false                                                      ;Returns false if the list simplifies to false
    (some false? notArgs) true
    (and(seq? (last notArgs))(= 'not (first(last notArgs))))(last(last notArgs))   ;Not Not Case
    (and(seq? (last notArgs))(= 'and (first(last notArgs))))(simplify(cons 'or(map #(list 'not %)(rest(last notArgs)))))      ;not-and DeMorgans
    (and(seq? (last notArgs))(= 'or (first(last notArgs))))(simplify(cons 'and (map #(list 'not %) (rest (last notArgs)))))
    :else notArgs;not-or DeMorgans
    )
  )


(defn lookup
  ;"Look up a value, i, in map m and returns the result if it exists. Otherwise returns i."
  [i m]
  (get m i i))
(defn deep-substitute
  ;"Given a map of replacement key/value pairs, m, and a list, l, returns a list with values from l,
   ;but with any elements equal to a key in m replaced with the corresponding val in m.
   ;If l contains nested lists, recursively performs replacement in those lists as well."
  [l m]
  (map (fn [i]
         (if (seq? i)
           (deep-substitute i m)
           (lookup i m)))
       l))

(defn evalexp
  [exp bindings]
  (simplify (deep-substitute exp bindings)))

(defn simplify
  [arg1]
  (cond
    (seq? arg1)(let[simpleArgs (map simplify arg1)]
                 (cond
                   (=(first arg1) 'and)(and-simplify simpleArgs)
                   (=(first arg1) 'or)(or-simplify simpleArgs)
                   (=(first arg1) 'not)(not-simplify simpleArgs)))
    :else arg1))



