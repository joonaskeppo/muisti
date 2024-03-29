(ns muisti.lang.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [muisti.utils :refer [read-project-file]]
            [muisti.lang.utils :refer [empty-hiccup?]]
            [muisti.lang.parser :as parser]))

(deftest test-empty
  (testing "with zero-length input"
    (let [output (parser/parse "")]
      (is (empty? (:attrs output)))
      (is (empty-hiccup? (:hiccup output)))))
  (testing "with whitespace-only input"
    (let [output (parser/parse "   \t \n \r   \n")]
      (is (empty? (:attrs output)))
      (is (empty-hiccup? (:hiccup output))))))

(deftest test-front-matters
  (testing "with valid edn front matter"
    (let [input-1  ":a 1 :b [:some :keyword] :c 100"
          input-2  "\n \t:a 1\n\t:b    [:some\n:keyword] :c\t 100\t \n"
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

(deftest test-basic-text
  (testing "with simple unformatted text"
    (let [output (parser/parse "Hello there. Another sentence.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello there. Another sentence."]]
             (:hiccup output)))))
  (testing "with multiple lines of a single paragraph of unformatted text"
    (let [output (parser/parse "Hello there.\nAnother sentence.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello there. Another sentence."]]
             (:hiccup output)))))
  (testing "with multiple paragraphs of unformatted text"
    (let [output (parser/parse "Hello there.\n\nAnother paragraph.\n\n\nThird paragraph.")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p "Hello there."] [:p "Another paragraph."] [:p "Third paragraph."]]
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
    (let [output (parser/parse "- Hello [:b there]\n- [:i World]")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ul
                        [:li "Hello " [:strong "there"]]
                        [:li [:em "World"]]]]]
             (:hiccup output)))))
  (testing "with flat ordered list"
    (let [output (parser/parse "1. Hello\n2. World")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ol [:li "Hello"] [:li "World"]]]]
             (:hiccup output)))))
  (testing "with flat ordered list with formatting"
    (let [output (parser/parse "1. Hello [:i there]\n2. [:b World]")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ol
                        [:li "Hello " [:em "there"]]
                        [:li [:strong "World"]]]]]
             (:hiccup output)))))
  (testing "with simple nested unordered list"
    (let [output (parser/parse "- Hello\n\t- There")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ul [:li "Hello"] [:ul [:li "There"]]]]]
             (:hiccup output)))))
  (testing "with longer nested unordered list"
    (let [src    (read-project-file "dev-resources" "unordered-list-items-2.mu")
          output (parser/parse src)]
      (is (empty? (:attrs output)))
      (is (= [:div [:p
                    [:ul
                     [:li "Things to say"]
                     [:ul
                      [:li "More things here"]]]
                    [:ul
                     [:li "Back to original"]
                     [:ul
                      [:li "2"]
                      [:li "3"]]]]]
             (:hiccup output)))))
  (testing "with unordered list containing ordered list"
    (let [src    (read-project-file "dev-resources" "unordered-with-ordered-list.mu")
          output (parser/parse src)]
      (is (= [:div [:p [:ul
                        [:li "Things"]
                        [:ol
                         [:li "Hey"]
                         [:li "There"]]]]]
             (:hiccup output)))))
  (testing "with nested ordered list"
    (let [output (parser/parse "1. Hello\n\t1. There")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:ol [:li "Hello"] [:ol [:li "There"]]]]]
             (:hiccup output))))))

(deftest test-code-blocks
  (testing "with multiline code block"
    (let [output (parser/parse "```\nMultiline\ncode\nblock\n```")]
      (is (empty? (:attrs output)))
      (is (= [:div [:pre "Multiline\ncode\nblock"]]
             (:hiccup output))))))

;; TODO: add tests for incomplete/invalid components!
;; NOTE: this seems to currently cause the parser to hang...

(deftest test-components
  ;; TODO: trying to parse a :link without `root-path` should return an error
  (testing "with :link component"
    (let [output          (parser/parse "[:link [literature nineteen-eighty-four] [:b Orwell's [:i Majestic] Book]]"
                                        {:root-path "/notes/"})]
      (is (= [:div [:p [:a {:href "/notes/literature/nineteen-eighty-four"}
                        [:strong "Orwell's " [:em "Majestic"] " Book"]]]]
             (:hiccup output)))
      (is (= {:links #{{:note/id ["literature" "nineteen-eighty-four"]}}}
             (:attrs output)))))
  (testing "with :tag component"
    (let [output (parser/parse "[:tag :test/tag :other/tag]")]
      (is (= [:div [:p [:span {:class "mu-tags-group"}
                        [:span {:class "mu-tag"} ":test/tag"]
                        [:span {:class "mu-tag"} ":other/tag"]]]]
             (:hiccup output)))
      (is (= {:tags #{:test/tag :other/tag}}
             (dissoc (:attrs output) :muisti.meta/tags)))
      (is (= 1 (get-in output [:attrs :muisti.meta/tags :other/tag 0 :line]))
               (get-in output [:attrs :muisti.meta/tags :test/tag 0 :line]))))
  (testing "with :tag inside heading"
    (let [output (parser/parse "# Test[:tag :some/tag]")]
      (is (= [:div
              [:h1 "Test" [:span {:class "mu-tags-group"}
                           [:span {:class "mu-tag"} ":some/tag"]]]]
             (:hiccup output)))))
  (testing "with :link components inside :tag component"
    (let [output (parser/parse "\n\n[:tag :nesting :wow]\n[:link [lit book] [:i Book]]\n[:link [another thing] My Thing]"
                               {:root-path "/notes/"})]
      (is (= {:links #{{:note/id ["lit" "book"]}
                       {:note/id ["another" "thing"]}}
              :tags  #{:nesting :wow}}
             (dissoc (:attrs output) :muisti.meta/tags)))
      (is (= [:div [:p
                    [:span {:class "mu-tags-group"}
                     [:span {:class "mu-tag"} ":nesting"]
                     [:span {:class "mu-tag"} ":wow"]]
                    [:a {:href "/notes/lit/book"} [:em "Book"]]
                    [:a {:href "/notes/another/thing"} "My Thing"]]]
             (:hiccup output)))))
  (testing "with hiccup fallback component, without props"
    (let [output (parser/parse "[:mark [:em highlighted]]")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:mark [:em "highlighted"]]]]
             (:hiccup output)))))
  (testing "with hiccup fallback component, with props"
    (let [output (parser/parse "[:mark {:class \"classy\"} [:em highlighted]]")]
      (is (empty? (:attrs output)))
      (is (= [:div [:p [:mark {:class "classy"} [:em "highlighted"]]]]
             (:hiccup output)))))
  (testing "with :link component inside hiccup fallback"
    (let [output (parser/parse "[:mark [:link [my thing] Book Notes]]"
                               {:root-path "/notes/"})]
      (is (= {:links #{{:note/id ["my" "thing"]}}}
             (:attrs output)))
      (is (= [:div [:p [:mark
                        [:a {:href "/notes/my/thing"}
                         "Book Notes"]]]]
             (:hiccup output)))))
  (testing "with :link component around text"
    (let [output (parser/parse "Test link: [:link [projects mu] my link]!"
                               {:root-path "/notes/"})]
      (is (= {:links #{{:note/id ["projects" "mu"]}}}
             (:attrs output)))
      (is (= [:div
              [:p "Test link: " [:a {:href "/notes/projects/mu"} "my link"] "!"]]
             (:hiccup output)))))
  ;; fix for regex issue
  (testing "with keyword inside component"
    (let [output (parser/parse "Should not bork: [:code :keyword]!"
                               {:root-path "/notes/"})]
      (is (= [:div
              [:p "Should not bork: " [:code ":keyword"] "!"]]
             (:hiccup output))))))

(deftest test-blockquotes
  (testing "with single formatted blockquote"
    (let [output (parser/parse "> [:i first] line\n> [:b second]")]
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
    (let [output (parser/parse "> first line.\n\t > second.\n\n> new quote")]
      (is (empty? (:attrs output)))
      (is (= [:div
              [:blockquote [:p "first line. second."]]
              [:blockquote [:p "new quote"]]]
             (:hiccup output))))))
