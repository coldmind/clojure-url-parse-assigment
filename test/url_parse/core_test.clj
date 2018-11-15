(ns url-parse.core-test
  (:require [clojure.test :refer :all]
            [url-parse.core :refer :all]))

(deftest test-1
  (testing "Successful case 1"
    (let [pattern (new-pattern "host(test.com);")]
      (is (= (recognize pattern "http://test.com") nil)))))

(deftest test-2
  (testing "Successful case 2"
    (let [pattern (new-pattern "host(test.com); path(users/?userid);")]
      (is (= (recognize pattern "http://test.com/users/1") '([:userid "1"]))))))

(deftest test-assigment
  (testing "Test all cases from assigment")
  (let [twitter (new-pattern "host(twitter.com); path(?user/status/?id);")
        dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")]
    (is (= (recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
           '([:user "bradfitz"] [:id "562360748727611392"])))
    (is (= (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
           '([:offset "1"] [:id "1905065-Travel-Icons-pack"])))
    (is (= (recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset")
           nil))
    (is (= (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
           nil))))
