(defpackage #:package-browser
  (:export
   #:show))

(in-package #:package-browser)

(defun get-package-names-list ()
  (sort
   (loop for pkg in (list-all-packages) collect
         (package-name pkg))
   #'string-lessp))

(defun select-package (name intf)
  (setf (capi:list-panel-unfiltered-items (sym-pane intf))
        (let (syms)
          (do-external-symbols (sym (find-package name)
                                    (sort syms #'string-lessp
                                          :key #'symbol-name))
            (push sym syms)))
        ))

(defun select-sym (sym)
  (editor::editor-show-call-graph sym :called-by)
  ;;(inspect sym)
  (cond
   ((boundp sym)
    (capi-toolkit:browse-object (symbol-value sym)))
   ((fboundp sym)
    (capi-toolkit:browse-object (symbol-function sym)))
()   (t
    (capi-toolkit:browse-object sym))
   )
  (pprint
   (dspec:find-name-locations dspec:*dspec-classes* sym)))

(capi:define-interface package-browser ()
  ()
  (:panes
   (pkg-pane capi:list-panel
             :items (get-package-names-list)
             :visible-min-height '(:character 20)
             :visible-min-width  '(:character 30)
             :callback-type      :data-interface
             :selected-items nil
             :selection-callback 'select-package)
   (sym-pane capi:list-panel
             :accessor sym-pane
             :visible-min-height '(:character 20)
             :visible-min-width  '(:character 50)
             :selected-items nil
             :callback-type      :data
             :selection-callback 'select-sym
             :items nil))
  (:layouts
   (main-layout capi:row-layout '(pkg-pane sym-pane)))
  (:default-initargs
   :title "Package Browser"))

(defun show ()
  (capi:display (make-instance 'package-browser)))
