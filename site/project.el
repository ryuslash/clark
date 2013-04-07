(require 'org-publish)

(setq org-publish-project-alist
      '(("clark-files"
         :base-directory "./"
         :publishing-directory "_publish/"
         :recursive nil
         :base-extension "css"
         :publishing-function org-publish-attachment)
        ("clark-org"
         :base-directory "./"
         :publishing-directory "_publish/"
         :recursive nil
         :base-extension "org"
         :publishing-function org-publish-org-to-html)
        ("clark-site"
         :components ("clark-org" "clark-files"))))
