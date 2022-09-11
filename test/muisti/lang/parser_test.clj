(ns muisti.lang.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [muisti.utils :refer [read-project-file]]
            [muisti.lang.utils :refer [empty-hiccup?]]
            [muisti.lang.parser :as parser]
            [muisti.lang.tokenizer :as tk]))

(deftest test-empty
  (testing "with zero-length input"
    (let [output (parser/parse "")]
      (is (empty? (:attrs output)))
      (is (empty-hiccup? (:hiccup output)))))
  (testing "with whitespace-only input")
  (let [output (parser/parse "   \t \n \r   \n")]
    (is (empty? (:attrs output)))
    (is (empty-hiccup? (:hiccup output)))))

(deftest test-front-matters
  (testing "with valid edn front matter"
    (let [input-1  "{:a 1 :b [:some :keyword] :c 100}"
          input-2  "\n \t{:a 1\n\t:b    [:some\n:keyword] :c\t 100}\t \n"
          output-1 (parser/parse input-1)
          output-2 (parser/parse input-2)]
      (is (= output-1 output-2))
      (is (= {:a 1 :b [:some :keyword] :c 100} (get-in output-1 [:attrs :front-matter])))
      (is (empty-hiccup? (:hiccup output-1))))))

(deftest test-headings
  (testing "with multiple headings only"
    (let [output (parser/parse "#\tfirst\n\n##  second\n### third\n# fourth")]
      (is (empty? (:attrs output)))
      (is (= [:div [:h1 "first"] [:h2 "second"] [:h3 "third"] [:h1 "fourth"]]
             (:hiccup output))))))

(deftest test-text-formatters
  (testing "with simple unformatted text"
    (let [output (parser/parse "Hello there. Another sentence.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello there. Another sentence."]]
             (:hiccup output)))))
  (testing "with multiple lines of a single paragraph of unformatted text"
    (let [output (parser/parse "Hello there.\nAnother sentence.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello there." "Another sentence."]]
             (:hiccup output)))))
  (testing "with multiple paragraphs of unformatted text"
    (let [output (parser/parse "Hello there.\n\nAnother paragraph.\n\n\nThird paragraph.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello there."] [:p "Another paragraph."] [:p "Third paragraph."]]
             (:hiccup output)))))
  (testing "with bold"
    (let [output (parser/parse "Hello with *bold*.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello with " [:strong "bold"] "."]]
             (:hiccup output)))))
  (testing "with italics"
    (let [output (parser/parse "Hello with /italics/.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello with " [:em "italics"] "."]]
             (:hiccup output)))))
  (testing "with strikethrough"
    (let [output (parser/parse "Hello with ~strikethrough~.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello with " [:del "strikethrough"] "."]]
             (:hiccup output)))))
  (testing "with inline code"
    (let [output (parser/parse "Hello with `code`.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello with " [:pre "code"] "."]]
             (:hiccup output))))))

(deftest test-lists
  (testing "with flat unordered list"
    (let [output (parser/parse "- Hello\n- World")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ul
                        [:li "Hello"]
                        [:li "World"]]]]
             (:hiccup output)))))
  (testing "with flat unordered list with formatting"
    (let [output (parser/parse "- Hello /there/\n- *World*")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ul
                        [:li "Hello " [:em "there"]]
                        [:li [:strong "World"]]]]]
             (:hiccup output)))))
  (testing "with flat ordered list"
    (let [output (parser/parse "1. Hello\n2. World")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ol [:li "Hello"] [:li "World"]]]]
             (:hiccup output)))))
  (testing "with flat ordered list with formatting"
    (let [output (parser/parse "1. Hello /there/\n2. *World*")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ol
                        [:li "Hello " [:em "there"]]
                        [:li [:strong "World"]]]]]
             (:hiccup output)))))
  (testing "with simple nested unordered list"
    (let [output (parser/parse "- Hello\n\t- There")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ul [:li "Hello"] [:li [:ul [:li "There"]]]]]]
             (:hiccup output)))))
  (testing "with longer nested unordered list"
    (let [src    (read-project-file "dev-resources" "unordered-list-items-2.mu")
          output (parser/parse src)]
      (is (empty? (:attrs output)))
      (is (= [:div [:p
                    [:ul
                     [:li "Things to say"]
                     [:li [:ul
                           [:li "More things here"]]]]
                    [:ul
                     [:li "Back to original"]
                     [:li [:ul
                           [:li "2"]
                           [:li "3"]]]]]]
             (:hiccup output)))))
  (testing "with nested ordered list"
    (let [output (parser/parse "1. Hello\n\t1. There")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ol [:li "Hello"] [:li [:ol [:li "There"]]]]]]
             (:hiccup output))))))

(deftest test-code-blocks
  (testing "with multiline code block"
    (let [output (parser/parse "```\nMultiline\ncode\nblock\n```")]
      (is (empty? (:attrs output)))
      (is (= [:div [:code "Multiline\ncode\nblock"]]
             (:hiccup output))))))

(deftest test-components
  ;; TODO: trying to parse a :link without `root-path` should return an error
  (testing "with :link component"
    (let [output          (parser/parse "[:link [literature nineteen-eighty-four] *Orwell's /Majestic/ Book*]"
                                        {:root-path "/notes/"})
          component-token {:line   1
                           :value  nil
                           :type   ::tk/component
                           :lexeme "[:link [literature nineteen-eighty-four] *Orwell's /Majestic/ Book*]"}]
      (is (= [:div [:p [:a {:href "/notes/literature/nineteen-eighty-four"}
                        [:strong "Orwell's " [:em "Majestic"] " Book"]]]]
             (:hiccup output)))
      (is (= {:links {component-token ["literature" "nineteen-eighty-four"]}}
             (:attrs output)))))
  (testing "with :tag component"
    (let [output          (parser/parse "[:tag [:test/tag :other/tag] *Orwell's /Majestic/ Book*]")
          component-token {:line   1
                           :value  nil
                           :type   ::tk/component
                           :lexeme "[:tag [:test/tag :other/tag] *Orwell's /Majestic/ Book*]"}]
      (is (= [:div [:p [:span [:strong "Orwell's " [:em "Majestic"] " Book"]]]]
             (:hiccup output)))
      (is (= {:tags {component-token [:test/tag :other/tag]}}
             (:attrs output)))))
  (testing "with :link component inside :tag component"
    (let [output     (parser/parse "\n\n[:tag [:nesting :wow]\n\n[:link [lit book] /Book/]]"
                                   {:root-path "/notes/"})
          tag-token  {:line   3
                      :value  nil
                      :type   ::tk/component
                      :lexeme "[:tag [:nesting :wow]\n\n[:link [lit book] /Book/]]"}
          link-token {:line   5
                      :value  nil
                      :type   ::tk/component
                      :lexeme "[:link [lit book] /Book/]"}]
      (is (= {:links {link-token ["lit" "book"]}
              :tags  {tag-token [:nesting :wow]}}
             (:attrs output)))
      (is (= [:div [:p [:span [:a {:href "/notes/lit/book"}
                               [:em "Book"]]]]]
             (:hiccup output)))))
  (testing "with hiccup fallback component, without props"
    (let [output (parser/parse "[:mark /highlighted/]")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:mark [:em "highlighted"]]]]
             (:hiccup output)))))
  (testing "with hiccup fallback component, with props"
    (let [output (parser/parse "[:mark {:class \"classy\"} /highlighted/]")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:mark {:class "classy"} [:em "highlighted"]]]]
             (:hiccup output)))))
  (testing "with :link component inside hiccup fallback"
    (let [output     (parser/parse "[:mark [:link [my thing] Book Notes]]"
                                   {:root-path "/notes/"})
          link-token {:line   1
                      :value  nil
                      :type   ::tk/component
                      :lexeme "[:link [my thing] Book Notes]"}]
      (is (= {:links {link-token ["my" "thing"]}}
             (:attrs output)))
      (is (= [:div [:p [:mark
                        [:a {:href "/notes/my/thing"}
                         "Book Notes"]]]]
             (:hiccup output))))))

(deftest test-blockquotes
  (testing "with single formatted blockquote"
    (let [output (parser/parse "> /first/ line\n> *second*")]
      (is (empty? (:attrs output)))
      ;; blockquotes don't break paragraphs due to single newlines
      (is (= [:div [:blockquote [:p
                                 [:em "first"] " line"
                                 [:strong "second"]]]]
             (:hiccup output)))))
  (testing "with two newlines inside a single blockquote"
    (let [output (parser/parse "> first line\n>\n> second")]
      (is (empty? (:attrs output)))
      (is (= [:div [:blockquote
                    [:p "first line"]
                    [:p "second"]]]
             (:hiccup output)))))
  (testing "with two blockquotes"
    (let [output (parser/parse "> first line\n\t > second\n\n> new quote")]
      (is (empty? (:attrs output)))
      (is (= [:div
              [:blockquote [:p "first line" "second"]]
              [:blockquote [:p "new quote"]]]
             (:hiccup output))))))
