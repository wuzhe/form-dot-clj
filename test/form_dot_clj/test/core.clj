(ns form-dot-clj.test.core
  "Tests for form-dot-clj as a whole"
  (:use clojure.test)
  (:use form-dot-clj.core)
  (:use form-dot-clj.html-controls))

(def-field string-field
  [:maxlength 10]
  [:pattern "[a-z]+" "pattern error"]
  [:match #"moo" "match error"]
  [:no-match #"cow" "no-match error"])

(def-field int-field [:integer -2 10 "integer error"])
(def-field float-field [:float -1.2 2.5 "float error"])
(def-field email-field [:email "email error"])
(def-field date-field [:date "2010-08-18" "2010-08-19" "date error"])
(def-field url-field [:url "url error"])
(def-field bool-field [:boolean])

(def-form test-form
  {}
  :string-field (textbox string-field)
  :int-field (textbox int-field)
  :float-field (textbox float-field)
  :email-field (textbox email-field)
  :date-field (textbox date-field)
  :url-field (textbox url-field)
  :bool-field (textbox bool-field))

(defn validate-ok
  "Checks that the given parameters validate with no errors"
  [params]
  (let [[validated errors] (validate test-form params)]
    (is (empty? errors) "Empty errors")))
  
(defn validate-error
  "Checks that the given parameters validates with the given errors:"
  [params expected-errs]
  (let [[validated errors] (validate test-form params)]
    (is (= errors expected-errs) "Expected errors")))

(deftest validation
  (validate-ok {"string-field" "moo"})
  (validate-ok {"int-field" "10"})
  (validate-ok {"int-field" "-1"})
  (validate-ok {"float-field" "2.5"})
  (validate-ok {"float-field" "-0.9"})
  (validate-ok {"float-field" "1"})
  (validate-ok {"email-field" "fred@example.com"})
  (validate-ok {"date-field" "2010-08-18"})
  (validate-ok {"date-field" "2010-08-19"})
  (validate-ok {"url-field" "http://www.example.com"})
  (validate-ok {"url-field" "http://example.co.uk"})
  (validate-ok {"bool-field" "moo"})
  (validate-ok {"bool-field" "yes"})
  (validate-ok {"bool-field" "no"})
  (validate-ok {"bool-field" ""})
  (validate-error {"string-field" "abcdefghijk"} 
                  {"string-field" "Too long."})
  (validate-error {"string-field" "with space"}
                  {"string-field" "pattern error"})
  (validate-error {"string-field" "nocow"}
                  {"string-field" "match error"})
  (validate-error {"string-field" "moocow"}
                  {"string-field" "no-match error"})
  (validate-error {"int-field" "moo"}
                  {"int-field" "integer error"})
  (validate-error {"int-field" "11"}
                  {"int-field" "integer error"})
  (validate-error {"int-field" "-3"}
                  {"int-field" "integer error"})
  (validate-error {"float-field" "moo"}
                  {"float-field" "float error"})
  (validate-error {"float-field" "-2.1"}
                  {"float-field" "float error"})
  (validate-error {"float-field" "3"}
                  {"float-field" "float error"})
  (validate-error {"email-field" "fred"}
                  {"email-field" "email error"})
  (validate-error {"email-field" "fred@example#"}
                  {"email-field" "email error"})
  (validate-error {"date-field" "2010-08-09"}
                  {"date-field" "date error"})
  (validate-error {"date-field" "2010-08-11"}
                  {"date-field" "date error"})
  (validate-error {"date-field" "moo"}
                  {"date-field" "date error"})
  (validate-error {"url-field" "moo"}
                  {"url-field" "url error"}))
