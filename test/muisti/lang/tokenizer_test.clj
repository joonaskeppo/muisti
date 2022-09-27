(ns muisti.lang.tokenizer-test
  (:require [clojure.test :refer [deftest is testing]]
            [muisti.utils :refer [read-project-file]]
            [muisti.lang.tokenizer :as tk :refer [scan-tokens]]))

(defn read-tokens [file]
  (-> (read-project-file "dev-resources" (str file ".mu"))
      scan-tokens))

(defn make-tokens [items]
  (map tk/make-token items))

(defmacro testing-with-tokens
  {:style/indent 1}
  [name [tokens-name] & body]
  `(testing ~name
     (let [~tokens-name (read-tokens ~name)]
       ~@body)))

(defmacro testing-tokens-as-equal-to
  {:style/indent 1}
  [name pre-tokens]
  `(testing ~name
     (let [tokens# (read-tokens ~name)]
       (is (= (make-tokens ~pre-tokens) tokens#)))))

;; --- tests ---

(deftest test-front-matter
  (testing-tokens-as-equal-to
   "front-matter-and-text"
   [{:type ::tk/front-matter :line 1 :lexeme ":author \"Joonas Keppo\"\n:tags   [:example :mdc]"}
    {:type ::tk/text :lexeme "Some text here." :line 4}
    {:type ::tk/eof :line 5}]))

(deftest test-comments
  (testing-tokens-as-equal-to
   "comments"
    ;; comments are completely ignored
   [{:type ::tk/front-matter :line 6 :lexeme ":author \"Joonas Keppo\"\n:tags   [:example :mdc]"}
    {:type ::tk/newline :line 11}
    {:type ::tk/text :lexeme "Some text here." :line 11}
    {:type ::tk/eof :line 12}]))

(deftest test-tabs
  (testing-with-tokens
   "tabs"
   [tokens]
   (is (= (make-tokens
           [{:type ::tk/tab :line 1 :value 1}
            {:type ::tk/tab :line 2 :value 2}
            {:type ::tk/tab :line 3 :value 2}
            {:type ::tk/tab :line 4 :value 3}])
          (filter #(= ::tk/tab (:type %)) tokens)))))

(deftest test-component
  (testing-tokens-as-equal-to
   "single-component"
   [{:type ::tk/text :line 1 :lexeme "This is a link-component: "}
    {:type ::tk/component :line 1 :lexeme "[:link {:to :the-thing} *My /Book/ Notes*]"}
    {:type ::tk/text :line 1 :lexeme "... more text here."}
    {:type ::tk/eof :line 2}]))

(deftest test-headings
  (testing-with-tokens
   "headings"
   [tokens]
   (is (= (make-tokens
           [{:type ::tk/heading :lexeme "First heading" :value 1 :line 1}
            {:type ::tk/heading :lexeme "Second" :value 2 :line 3}
            {:type ::tk/heading :lexeme "Third" :value 3 :line 5}
            {:type ::tk/heading :lexeme "Even tabbed headings are OK!" :value 4 :line 7}
            {:type ::tk/heading :lexeme "Last one" :value 4 :line 8}])
          (filter #(= ::tk/heading (:type %)) tokens)))))

(deftest test-blockquotes
  (testing-tokens-as-equal-to
   "blockquote"
   [{:type ::tk/blockquote :line 1 :lexeme "hello\nthere"}
    {:type ::tk/newline :line 3}
    {:type ::tk/newline :line 4}
    {:type ::tk/blockquote :line 4 :lexeme "new quote"}
    {:type ::tk/eof :line 5}]))

(deftest test-list-items
  (testing-tokens-as-equal-to
   "unordered-list-items"
   [{:type ::tk/unordered-bullet :line 1}
    {:type ::tk/text :line 1 :lexeme "this is a list item"}
    {:type ::tk/newline :line 2}
    {:type ::tk/tab :line 2 :value 1}
    {:type ::tk/unordered-bullet :line 2}
    {:type ::tk/text :line 2 :lexeme "sub item"}
    {:type ::tk/newline :line 3}
    {:type ::tk/tab :line 3 :value 2}
    {:type ::tk/unordered-bullet :line 3}
    {:type ::tk/text :line 3 :lexeme "nesting deeper"}
    {:type ::tk/newline :line 4}
    {:type ::tk/newline :line 5}
    {:type ::tk/unordered-bullet :line 5}
    {:type ::tk/text :line 5 :lexeme "new list"}
    {:type ::tk/newline :line 8}
    {:type ::tk/tab :line 8 :value 1}
    {:type ::tk/text :line 8 :lexeme "-not a list item"}
    {:type ::tk/newline :line 9}
    {:type ::tk/tab :line 9 :value 1}
    {:type ::tk/text :line 9 :lexeme "*neither is this"}
    {:type ::tk/newline :line 10}
    {:type ::tk/unordered-bullet :line 10}
    {:type ::tk/text :line 10 :lexeme "but this is"}
    {:type ::tk/eof :line 11}])
  (testing-tokens-as-equal-to
   "ordered-list-items"
   [{:type ::tk/ordered-bullet :line 1}
    {:type ::tk/text :line 1 :lexeme "this is an item"}
    {:type ::tk/newline :line 2}
    {:type ::tk/tab :line 2 :value 1}
    {:type ::tk/ordered-bullet :line 2}
    {:type ::tk/text :line 2 :lexeme "nested"}
    {:type ::tk/newline :line 3}
    {:type ::tk/ordered-bullet :line 3}
    {:type ::tk/text :line 3 :lexeme "second item"}
    {:type ::tk/newline :line 4}
    {:type ::tk/newline :line 5}
    {:type ::tk/text :line 5 :lexeme "1.invalid item"}
    {:type ::tk/eof :line 6}]))

(deftest test-formatters
  (testing-tokens-as-equal-to
   "formatted-text"
   [{:type ::tk/text :lexeme "Text can have " :line 1}
    {:type ::tk/bold :line 1}
    {:type ::tk/text :lexeme "bold" :line 1}
    {:type ::tk/bold :line 1}
    {:type ::tk/text :lexeme ", " :line 1}
    {:type ::tk/code-inline :lexeme "code" :line 1}
    {:type ::tk/text :lexeme ", italic, or " :line 1}
    {:type ::tk/strikethrough :line 1}
    {:type ::tk/text :lexeme "strikethrough" :line 1}
    {:type ::tk/strikethrough :line 1}
    {:type ::tk/text :lexeme " formatters." :line 1}
    {:type ::tk/newline :line 2}
    {:type ::tk/text :lexeme "They can " :line 2}
    {:type ::tk/bold :line 2}
    {:type ::tk/text :lexeme "also be " :line 2}
    {:type ::tk/strikethrough :line 2}
    {:type ::tk/text :lexeme "nested", :line 2}
    {:type ::tk/strikethrough :line 2}
    {:type ::tk/bold :line 2}
    {:type ::tk/eof :line 3}])
  (testing-tokens-as-equal-to
   "code"
   [{:type ::tk/text :line 1 :lexeme "We can have "}
    {:type ::tk/code-inline :line 1 :lexeme "multiline\ninline code."}
    {:type ::tk/newline :line 3}
    {:type ::tk/code-block :line 3 :lexeme "Multiline\ncode\nblocks"}
    {:type ::tk/newline :line 8}
    {:type ::tk/text :line 8 :lexeme "filler text"}
    {:type ::tk/eof :line 9}]))

(deftest test-escape-char
  (testing-tokens-as-equal-to
   "escape-char"
   [{:type ::tk/text :line 1 :lexeme "*This* should *NOT* be bolded or /this/ italicized."}
    {:type ::tk/eof :line 2}]))
