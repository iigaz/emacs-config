;;; programming-cpp.el --- Utilities for programming in C/C++. -*- lexical-binding: t; -*-

;; Compile
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   (concat "g++ -g \""
                           buffer-file-name
                           "\" -o \""
                           (file-name-sans-extension buffer-file-name)
                           "\" ")))))

;; Formatting Style
(c-add-style "microsoft"
             '("stroustrup"
               (c-offsets-alist
                (innamespace . -)
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (template-args-cont . +))))

;;; Lsp-mode
(with-eval-after-load 'lsp-mode
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'c-mode-hook 'lsp))

;;; Dap-mode
(with-eval-after-load 'dap-mode
  (require 'dap-cpptools)
  (dap-register-debug-template
   "cpptools::Run Configuration"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run Configuration"
         :MIMode "gdb"
         :program "${workspaceFolder}/${fileBasenameNoExtension}"
         :cwd "${workspaceFolder}"))

  (dap-register-debug-template
   "Debug C++"
   (list :type "cppdbg"
         :request "launch"
         :name "Debug C++"
         :MIMode "gdb"
         :program "${fileBasenameNoExtension}"
         :cwd "${fileDirname}"
         :stopAtEntry nil
         :args nil
         :dap-compilation "g++ -g ${file} -o ${fileDirname}${pathSeparator}${fileBasenameNoExtension}"
         :setupCommands '((("description" . "Enable pretty-printing for gdb")
                           ( "text" . "-enable-pretty-printing")
                           ("ignoreFailures" . "true"))
                          (("description" . "Set Disassembly Flavor to Intel")
                           ( "text" . "-gdb-set disassembly-flavor intel")
                           ("ignoreFailures" . "true"))))))

;;; programming-cpp.el ends here
