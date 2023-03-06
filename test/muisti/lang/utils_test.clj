(ns muisti.lang.utils-test
  (:require [clojure.test :refer [deftest is testing]]
            [muisti.lang.utils :refer [read-delimited]]))

(deftest test-read-delimited
  (testing "with invalid input"
    (is (nil? (read-delimited "tasdas d `asdasd` asdasd" "`" "`"))))
  (testing "with inline code containing escaped delimiters"
    (is (= "`this \\`is a \\`code`"
           (read-delimited "`this \\`is a \\`code` bleep bloop" "`" "`"))))
  (testing "with code block"
    (is (= "```\nMultiline\ncode\nblock\n```"
           (read-delimited "```\nMultiline\ncode\nblock\n```\nthis part is ignored"
                           "```\n" "\n```"))))
  (testing "with code block without delimiters"
    (is (= "Multiline\ncode\nblock"
           (read-delimited "```\nMultiline\ncode\nblock\n```\nthis part is ignored"
                           "```\n" "\n```"
                           {:drop-delimiters true}))))
  (testing "with component with stringified square brackets"
    (is (= "[:content [:here [\"[ignoreme]]]\"]]]"
           (read-delimited "[:content [:here [\"[ignoreme]]]\"]]] asdasd" "[" "]"
                           {:escape-strings true})))))
