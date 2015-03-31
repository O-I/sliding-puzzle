; Written by Nurullah Akkaya
; https://github.com/nakkaya
; See Cheaplist in Clojure for more information
; http://nakkaya.com/2011/10/10/cheaplist-in-clojure/
(ns sliding-puzzle.cheaplist)

(def que-size 5)

(defn qsort-aux [[pivot & tail] pred]
  (when pivot
    (let [pred? (partial pred pivot)]
      (lazy-cat (qsort-aux (filter pred? tail) pred)
                [pivot]
                (qsort-aux (remove pred? tail) pred)))))

(defn qsort [xs f] (qsort-aux (seq xs) f))

(deftype CheapList [pred cheap bulk]
  Object
  (toString [this] (str (seq this)))

  clojure.lang.IPersistentVector
  (cons [this curr]
    (let [[item-curr pri-curr] curr
          eol (last cheap)
          [item-eol pri-eol] eol]

      (cond
       ;;empty list
       (nil? eol)
       (CheapList. pred (conj cheap curr) (assoc bulk item-curr pri-curr))

       ;;list contains item
       (bulk item-curr)
       (.cons
        (CheapList. pred
                    (filter (fn [[k]]
                              (not= k item-curr)) cheap)
                    (dissoc bulk item-curr))
        curr)

       ;;cheaper then the curr most expensive
       (pred eol curr)
       (if (>= (count cheap) que-size)
         (CheapList. pred
                     (qsort (conj (drop-last cheap) curr) pred)
                     (assoc bulk item-eol pri-eol))
         (CheapList. pred
                     (qsort (conj cheap curr) pred)
                     (assoc bulk item-curr pri-curr)))

       ;;new item
       :default (CheapList. pred cheap (assoc bulk item-curr pri-curr)))))

  clojure.lang.ISeq
  (first [this]
    (first cheap))

  (more [this]
    (if (= (count cheap) 1)
      (let [bulk (dissoc bulk (-> cheap first first))]
        (CheapList. pred
                    (take que-size (qsort bulk pred))
                    bulk))
      (CheapList. pred (rest cheap) (dissoc bulk (-> cheap first first)))))

  (seq [this]
    (when (not (empty? bulk))
      (lazy-seq (cons (first this) (rest this)))))

  (equiv [this o]
    (and (= CheapList (type o))
         (= (seq this) (seq o))))

  (count [this]
    (count bulk))

  (empty [this]
    (empty? bulk))

  clojure.lang.ILookup
  (valAt [this key]
    (bulk key))

  (valAt [this key not-found]
    (or (.valAt this key) not-found))

  clojure.lang.IFn
  (invoke [this key]
    (get this key))

  (invoke [this key not-found]
    (get this key not-found)))

(defn cheap-list
  [pred & keyvals]
  (reduce conj (CheapList. pred [] {}) keyvals))
