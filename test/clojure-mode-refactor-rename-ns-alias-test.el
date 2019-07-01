;;; clojure-mode-refactor-rename-ns-alias-test.el --- Clojure Mode: refactor rename ns alias  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'clojure-mode)
(require 'ert)

(def-refactor-test test-rename-ns-alias
  "(ns cljr.core
       (:require [my.lib :as lib]))

     (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

     (+ (lib/a 1) (b 2))"
  "(ns cljr.core
       (:require [my.lib :as foo]))

     (def m #::foo{:kw 1, :n/kw 2, :_/bare 3, 0 4})

     (+ (foo/a 1) (b 2))"
  (clojure--rename-ns-alias-internal "lib" "foo"))

(def-refactor-test test-rename-ns-alias-with-missing-as
  "(ns cljr.core
       (:require [my.lib :as lib]))

     (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

     (+ (lib/a 1) (b 2))"
  "(ns cljr.core
       (:require [my.lib :as lib]))

     (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

     (+ (lib/a 1) (b 2))"
  (clojure--rename-ns-alias-internal "foo" "bar"))

(provide 'clojure-mode-refactor-rename-ns-alias-test)

;;; clojure-mode-refactor-rename-ns-alias-test.el ends here
