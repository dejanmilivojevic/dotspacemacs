(defconst my-graphql-packages
  '(
    ob-graphql
    ))

(defun my-graphql/init-ob-graphql ()
  (use-package ob-graphql
    :defer t
    :after org
    :config
    (add-to-list 'org-babel-load-languages '(graphql . t))))
