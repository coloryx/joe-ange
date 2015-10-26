(satisfies? IFn #{1})


(extend-type js/RegExp
  IFn
  (-invoke
    ([this a]
     (re-find this a))))

(filter #"^foo" ["haha" "foobar" "baz" "foobaz"])

(filter #(re-find #"^foo" %) ["haha" "foo" "foobar" "baz" "foobaz" "sfoob"])

(deftype User [firstname lastname])

(def person (User. "Triss" "Merigold"))

(.-firstname person)

(defn make-user
  [firstname lastname]
  (User. firstname lastname))

(defrecord User [firstname lastname])

(def person (User. "Yenefer" "of Bengerberg"))

(:firstname person)

(get person :firstname)


