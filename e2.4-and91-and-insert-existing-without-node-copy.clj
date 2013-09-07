(defmulti insert' (fn [e list p] (empty? list)))
(defmethod insert' true [e list p] (if (= p e) (throw "Key already in tree") [e [] []]))
(defmethod insert' false [e [x l r :as all] p] (cond (< e x) [x (insert' e l p) r]
                                          :else [x l (insert' e r x)]))
(defn insert [e l] (try (insert' e l (first l)) (catch Exception e l)))
