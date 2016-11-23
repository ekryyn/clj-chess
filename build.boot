(set-env!
  :resource-paths #{"src" "test"}
  :dependencies
  '[
    [me.raynes/conch "0.8.0"]
    [adzerk/boot-test "1.1.2"]
    [quil "2.5.0"]
    ])

(require
  '[adzerk.boot-test :refer [test]]
  )

(task-options!
  pom {:project 'chess
       :version "0.1.0"}
  jar {:manifest {"Foo" "bar"}})
