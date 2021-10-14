
(in-package ql-dist)

(defun install-dist (filepath &key (prompt t) replace)
  (let ((temp-file (qmerge "tmp/install-dist-distinfo.txt")))
    (ensure-directories-exist temp-file)
    (delete-file-if-exists temp-file)
    (copy-file filepath temp-file)
    (let* ((new-dist (make-dist-from-file temp-file))
           (old-dist (find-dist (name new-dist))))
      (when old-dist
        (if replace
          (uninstall old-dist)
          (restart-case
            (error "A dist named ~S is already installed."
                   (name new-dist))
            (replace ()
                     :report "Replace installed dist with new dist"
                     (uninstall old-dist)))))
      (format t "Installing dist ~S version ~S.~%"
              (name new-dist)
              (version new-dist))
      (when (or (not prompt)
                (press-enter-to-continue))
        (ensure-directories-exist (base-directory new-dist))
        (copy-file temp-file (relative-to new-dist "distinfo.txt"))
        (ensure-release-index-file new-dist)
        (ensure-system-index-file new-dist)
        (enable new-dist)
        (setf (preference new-dist) (get-universal-time))
        (when old-dist
          (clear-dist-systems old-dist))
        (clear-dist-systems new-dist)
        new-dist))))

(install-dist "/home/radiance/shirakumo-dist.txt")
