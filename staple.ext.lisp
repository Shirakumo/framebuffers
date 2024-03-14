(defmethod staple:packages ((_ (eql (asdf:find-system "framebuffers"))))
  '(:org.shirakumo.framebuffers))

(defmethod staple:subsystems ((_ (eql (asdf:find-system "framebuffers"))))
  '())
