
(ql:quickload :radiance)
(radiance:startup)
(let ((domain (uiop:getenv "DOMAIN")))
  (format t "Adding domain to Radiance's config: ~A~%" domain)
  (pushnew domain (ubiquitous:value :domains) :test #'string=))
(sb-ext:quit)
