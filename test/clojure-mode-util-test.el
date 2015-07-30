;;; clojure-mode-util-test.el --- Clojure Mode: util test suite  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015 Bozhidar Batsov <bozhidar@batsov.com>

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

;;; Commentary:

;; The unit test suite of Clojure Mode

;;; Code:
(require 'clojure-mode)
(require 'cl-lib)
(require 'ert)

(let ((project-dir "/home/user/projects/my-project/")
      (clj-file-path "/home/user/projects/my-project/src/clj/my_project/my_ns/my_file.clj")
      (project-relative-clj-file-path "src/clj/my_project/my_ns/my_file.clj")
      (clj-file-ns "my-project.my-ns.my-file"))

  (ert-deftest project-relative-path ()
    :tags '(utils)
    (cl-letf (((symbol-function 'clojure-project-dir) (lambda () project-dir)))
      (should (string= (clojure-project-relative-path clj-file-path)
                       project-relative-clj-file-path))))

  (ert-deftest expected-ns ()
    :tags '(utils)
    (cl-letf (((symbol-function 'clojure-project-relative-path)
               (lambda (&optional current-buffer-file-name)
                 project-relative-clj-file-path)))
      (should (string= (clojure-expected-ns clj-file-path) clj-file-ns))))

  (ert-deftest expected-ns-without-argument ()
    :tags '(utils)
    (cl-letf (((symbol-function 'clojure-project-relative-path)
               (lambda (&optional current-buffer-file-name)
                 project-relative-clj-file-path)))
      (should (string= (let ((buffer-file-name clj-file-path))
                         (clojure-expected-ns))
                       clj-file-ns)))))

(provide 'clojure-mode-util-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; clojure-mode-util-test.el ends here
