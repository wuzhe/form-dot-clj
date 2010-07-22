
(ns form-dot-clj.server-side
  "Server side validation checks"
  (:use clojure.contrib.def))

(defn maxlength
  "Performs a maxlength check on a string.
   This test should only fail when submitting using a script."
  [length]
  (fn [s]
    (if (> (count s) length)
      {:error "Too long."}
      {})))
  
(defn pattern
  "Performs a pattern match on a string. Pattern is a string.
   The pattern is in the style of HTML5 and will be matched the whole string,
   a leading ^ and trailing $ are not required."
  [pattern error-message]
  (let [full-match (str "^" pattern "$")
        re (re-pattern full-match)]
  (fn [s]
    (if-not (re-find re s)
      {:error error-message}
      {}))))

(defn no-match
  "Executes the given regex on a string. Returns an error
   if there is a match"
  [re error-message]
  (fn [s]
    (if (re-find re s)
      {:error error-message}
      {})))

(defn check-integer
  "Returns a function to convert and check and integer."
  [min max error-message]
  (fn [s]
    (try
      (let [i (Integer. s)]
        (if-not (and (>= i min) (<= i max))
          {:error error-message}
          {:value i}))
      (catch NumberFormatException e
        {:error error-message}))))
      
(defn email
  "Returns a function to check if an email address is valid.
   Maximum length:
     http://stackoverflow.com/questions/386294/maximum-length-of-a-valid-email-id
   Uses regex based on:
     http://www.w3.org/TR/html5/states-of-the-type-attribute.html#e-mail-state"
  [error-message]
  (let [re #"^(?i)[-a-z0-9~!$%^&*_=+}{\'?]+(\.[-a-z0-9~!$%^&*_=+}{\'?]+)*@[-a-z0-9_][-a-z0-9_]*(\.[-a-z0-9_]+)*$"]
    (fn [s]
      (if-not (and (<= (.length s) 256)
                   (re-find re s))
        {:error error-message}
        {}))))

(defn check-date
  "Returns a function to check if a date is correct"
  [min-date max-date error-message]
  (fn [s] {}))

(defvar- validation-fns
  {:maxlength maxlength
   :pattern pattern
   :no-match no-match
   :integer check-integer
   :email email
   :date check-date})


(defn generate-check-fn
  "Generate a check function for the given check"
  [check args]
  (if (contains? validation-fns check)
    (apply (validation-fns check) args)))
   
(defn generate-check-fns
  "Generate server side check functions from a field definition"
  [field]
  (remove nil?
          (map (fn [x] (generate-check-fn x (field x)))
               (field :validation-seq))))