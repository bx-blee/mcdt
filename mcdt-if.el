;;; mcdt-if.el --- Mail Templating, Distribution and Tracking -*- lexical-binding: t; -*-

;;; Commentary:
;;; MTDT (Mail Templating, Distribution and Tracking) FKA:  MCDT (Mail Composition, Distribution and Tracking)
;;; Libre-Halaal Constant Contact For Everyone
;;;
;;; Blee Panel Documentation Is At:
;;; /bisos/panels/blee-core/mail/mailings/emacs-mcdt/mcdt-conceptAndDesign/_nodeBase_/fullUsagePanel-en.org
;;;
;;; Given a self-contained mailingFile (usually called content.mail) or within a mailingFile-buffer, using macros, mcdt creates
;;; a series of commands that allow for customized-compostion, sending and distribution of those
;;; messages.
;;;
;;; The mailingFile should be in correct RFC-822 format. For example, you can not have an empty To: field.
;;;
;;; MTDT consists of:
;;;    mtdt-lib.el    --- General purpose libraries
;;;    mtdt-newMail.el   --- Initial outgoing email
;;;    mtdt-contextedMail.el  --- Replies and Forwards
;;;
;;; Customized-compostion of mailings, takes of two forms.
;;;  - direct editing -- :extSrcBase nil
;;;  - external source editing -- :extSrcBase "."  -- The result is then
;;;    included in the mailingFile through a dblock.
;;;
;;; Primary command usage interfaces are:
;;; - mcdt:setup-and-compose/with-curBuffer -- Used the mailing buffer -- Uses /with-file  in turn.
;;; - mcdt:setup-and-compose/with-file  -- Used in Blee Panels
;;; - mcdt:setup/with-curBuffer -- Just create the mailing ffunction as mcdt:compose/mailingName.
;;;
;;; Compose results into a new frame and a ready buffer in a /tmp ephemera base.
;;;
;;;
;;;----------------------------------------------------------------------------
;;; OLD AND OBSOLETE COMMENTS BELOW KEPT TILL FULLY ABSORBED:
;;; This used to be Machine Generated File through: mailingProc.sh vis_basic_method_prep
;;; based on: /libre/ByStar/InitialTemplates/mailing/templates/msend-mailing-template.el
;;;
;;;    ======== bxms-compose-MailingName           -- Originate A Fresh Message -- Or Augment An Existing Message
;;;    ======== bxms-batch-MailingName          -- = bx-msend-MailingName + (msend-mail-and-exit)
;;;    ========
;;;    ======== bxms-web-url-MailingName            -- BROWSER ORIGINATION -- (Send Link)
;;;    ======== bxms-web-mailto-MailingName         -- BROWSER ORIGINATION -- (Click On A mailto: URL)
;;;    ========
;;;    ======== x bxms-compose-MailingName         -- BBDB ORIGINATION  -- Interactive on One or on Each one-by-one
;;;    ======== x bxms-batch-MailingName        -- BBDB ORIGINATION  -- Batch on One or on Each one-by-one
;;;    ======== x bxms-toall-MailingName          -- BBDB ORIGINATION  -- Interactive on ALL
;;;    ========
;;;    ======== bxms-bbdb-compose-MailingName  -- BBDB USAGE        -- Interactive on One
;;;    ======== bxms-bbdb-batch-MailingName    -- BBDB USAGE        -- Batch on One
;;;    ======== bxms-bbdb-toall-MailingName    -- BBDB USAGE        -- Interactive on ALL in To:
;;;----------------------------------------------------------------------------
;;
;;; Code:

(require 'f)
(require 'loop)
(require 'message)
(require 'mailing-from-base)
(require 'msend-lib)
(require 'org-msg)

(defconst mcdt:compose:fashion::basic "Basic" "Basic Plain Text Mail Composition.")
(defconst mcdt:compose:fashion::orgMsg "OrgMsg" "OrgMsg Mail Composition.")
(defconst mcdt:compose:fashion::latex "LaTeX" "LaTeX Mail Composition.")

(defvar mcdt:compose:fashion
  mcdt:compose:fashion::basic
  "Selected and Effective compose fashion.")

(defvar mcdt:reply:templates:base
  (expand-file-name "~/bpos/usageEnvs/fullUse/mailings/reply")
  "Basedir of where LaTeX templates are.")

(defvar mcdt:reply:templates:leftToRight
  (expand-file-name "~/bpos/usageEnvs/fullUse/mailings/reply/ltr-basicLaTeX")
  "Basedir of where leftToRight LaTeX languages templates are.")

(defvar mcdt:reply:templates:rightToLeft
  (expand-file-name "~/bpos/usageEnvs/fullUse/mailings/reply/rtl-basicLaTeX")
  "Basedir of where leftToRight LaTeX languages templates are.")

(defvar mcdt:compose:ephemera:base "/bisos/tmp"
  "Basedir of where ephemera compositions go.")

(defun mcdt:compose:fashion/setup (<fashion)
  "Based on <fashion, set things up for composition."
  (cond
   ((eq <fashion  mcdt:compose:fashion::basic)
    (setq mcdt:compose:fashion <fashion)
    (when org-msg-mode
      (org-msg-mode -1)))
   ((eq <fashion  mcdt:compose:fashion::orgMsg)
    (setq mcdt:compose:fashion <fashion)
    (when (not org-msg-mode)
      (org-msg-mode)))
   ((eq <fashion  mcdt:compose:fashion::latex)
    (setq mcdt:compose:fashion <fashion)
    (when org-msg-mode
      (org-msg-mode -1)))
   (t
    (error "Bad input"))
   ))


;;
;; (mcdt:compose$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail")
;; (macroexpand-all (mcdt:compose$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail"))
;;
(defmacro mcdt:compose$mailing-defun (<mailingFilePath)
  "The macro defines a function to be invoked to compose a msg from a template.
<MAILINGFILEPATH is expected to be a static path.
interactive p is needed so that there are some params.
With compose can edit content and headers, with originate only headers."
  `(fset (intern (concat "mcdt:compose/" (mcdt:mailing:getName|with-file ,<mailingFilePath)))
	 (lambda (args)
	   (interactive "p")
	   (mcdt:compose|with-file ,<mailingFilePath args)
	   )
	 ))

;;
;; (mcdt:oriiginate$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail")
;; (macroexpand-all (mcdt:originate$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail"))
;;
(defmacro mcdt:originate$mailing-defun (<mailingFilePath)
  "<MAILINGFILEPATH is expected to be a static path.
The macro defines a function to be invoked to originate a msg from template.
interactive p is needed so that there are some params.
With compose can edit content and headers, with originate only headers."
  `(fset (intern (concat "mcdt:originate/" (mcdt:mailing:getName|with-file ,<mailingFilePath)))
	 (lambda (args)
	   (interactive "p")
	   (mcdt:originate|with-file ,<mailingFilePath args)
	   )
	 ))

;;
;; (mcdt:batch$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail")
;; (macroexpand-all (mcdt:batch$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail"))
;;
(defmacro mcdt:batch$mailing-defun (<mailingFilePath)
  "Macro defines a function to be invoked to batch send a msg with a template in <MAILINGFILEPATH.
NOTYET, instead of fset intern, try defun -- would be simpler."
  `(fset (intern (concat "mcdt:batch/"  (mcdt:mailing:getName|with-file ,<mailingFilePath)))
	 (lambda (args)
	   (interactive "p")
	   ($:mcdt:batch|with-file ,<mailingFilePath args)
	   )
	 ))

(defun $:mcdt:batch|with-file (<mailingFilePath args)
  "Out of macro work of mcdt:batch$mailing-defun.
ModuleLocal."
    (funcall (intern (concat "mcdt:compose/" (mcdt:mailing:getName|with-file <mailingFilePath))) args)
    (msend-mail-and-exit)
    )

;;
;; (mcdt:setup$with-filePath "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail")
;; (macroexpand-all (mcdt:setup$with-filePath "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail"))
;;
(defmacro mcdt:setup$with-filePath (<mailingFilePath)
  "Create/Define all mcdt:xx commands based on <mailingFilePath.
Base function for other mcdt:setup:s map to this.
Needs to be a macro, so that ,<mailingFilePath is captured.
Is expected to be invoked once for each <mailingFilePath.
For use by elisp applications."
  `(progn
     (mcdt:compose$mailing-defun ,<mailingFilePath)
     (mcdt:originate$mailing-defun ,<mailingFilePath)
     (mcdt:batch$mailing-defun ,<mailingFilePath)
     )
  )

;;
;; (macroexpand-all '(mcdt:setup$with-curBuffer))
;;
;; Example  "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail"
;;
(defmacro $:mcdt:setup$with-curBuffer ()
  "Create/Define all mcdt:xx commands based on current buffer file-name.
$: indicates it is private to this module. Is not expected to be invoked by any external user.
NOTYET, try 'make-symbol'
interactive P is necessary.
gensym did not work, ended up using mcdt:cur:buffer which works but is obviously not the right way.
NOTYET, we need a wrapper around mcdt:setup/with-curBuffer.
Is not expected to be invoked by any external user. $: indicates it is local to this module.
External user uses mcdt:setup/with-curBuffer which then invoke macros based on current buffer.
"
  `(fset (intern "mcdt:setup:gened/with-curBuffer")
	 (lambda (params)
	   (interactive "p")
	   (setq mcdt:cur:buffer (buffer-file-name))
	   (mcdt:compose$mailing-defun mcdt:cur:buffer)
  	   (mcdt:batch$mailing-defun  mcdt:cur:buffer)
	   )
	 )
  )

;;;
;;; The expansion is needed
;;;
($:mcdt:setup$with-curBuffer)

;;
;; (mcdt:bbdb-compose$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:bbdb-compose$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:bbdb-compose$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template"
  (let* (
	($baseFuncName "mcdt:bbdb-compose/")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (params)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda (params)
	     (interactive "p")
	     (bx:mailing:bbdb:compose ,<mailingPath params)
	     ))
    ))

;;
;; (mcdt:bbdb-batch$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:bbdb-batch$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:bbdb-batch$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template."
  (let* (
	($baseFuncName "mcdt:bbdb-batch/")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (params)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda (params)
	     (interactive "p")
	     (bx:mailing:bbdb:batch ,<mailingPath params)
	     ))
    ))

;;
;; (mcdt:bbdb-toall$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:bbdb-toall$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:bbdb-toall$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template"
  (let* (
	($baseFuncName "mcdt:bbdb-toall/")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (params)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda (params)
	     (interactive "p")
	     (bx:mailing:bbdb:toall ,<mailingPath params)
	     ))
    ))

;;
;; (mcdt:bbdb:compose$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:bbdb:compose$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:bbdb:compose$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template"
  (let* (
	($baseFuncName "mcdt:bbdb:compose/")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (records)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda (records)
	     (interactive "p")
	     (bxms-bbdb-compose-from-base ,<mailingPath records)
	     ))
    ))

;;
;; (mcdt:bbdb:toall$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:bbdb:toall$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:bbdb:toall$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template"
  (let* (
	($baseFuncName "mcdt:bbdb:toall/")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (records)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda (records)
	     (interactive "p")
	     (bbdb-mall-from-base ,<mailingPath records)
	     ))
    ))

;;
;; (mcdt:bbdb:batch$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:bbdb:batch$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:bbdb:batch$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template"
  (let* (
	($baseFuncName "mcdt:bbdb:batch/")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (records)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda (records)
	     (interactive "p")
	     (bxms-bbdb-batch-from-base ,<mailingPath records)
	     ))
    ))

;;
;; (mcdt:web:mailto-pre$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:web:mailto-pre$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:web:mailto-pre$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template"
  (let* (
	($baseFuncName "mcdt:web:mailto|")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName "-pre"))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (records)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda ()
	     (bx-mmailto-from-base-pre ,<mailingPath)
	     ))
    ))

;;
;; (mcdt:web:mailto-post$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:web:mailto-post$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:web:mailto-post$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template"
  (let* (
	($baseFuncName "mcdt:web:mailto|")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName "-post"))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (records)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda ()
	     (bx-mmailto-from-base-post ,<mailingPath)
	     ))
    ))

;;
;; (mcdt:web:mailto$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;; (macroexpand-all (mcdt:web:mailto$mailing-defun "~/BUE/mailings/start/family.fa/blank/basicText.fa"))
;;
(defmacro mcdt:web:mailto$mailing-defun (<mailingPath)
  "The macro defines a function to be invoked to batch send a message based on a template"
  (let* (
	($baseFuncName "mcdt:web:mailto|")
	($mailingName (mcdt:mailing:getName|with-file <mailingPath))
	($fullFuncName (concat $baseFuncName $mailingName "-post"))
	)
    ;;;
    ;;; Equivalent of (defun ,fullFuncName (records)
    ;;;
    `(fset (intern ,$fullFuncName)
	   (lambda ()
	     (interactive)

	     (setq  a-murl-pre-hook nil)
	     ;; NOTYET
	     (add-hook 'a-murl-pre-hook 'bxms:web:mailto:start-family.fa-blank-basicText.fa-pre)

	     (setq  a-murl-post-hook nil)
	     (add-hook 'a-murl-post-hook 'bxms:web:mailto:start-family.fa-blank-basicText.fa-post)

	     ))
    ))

;; (defun bxms:web:url:start-family.fa-blank-basicText.fa-pre ()
;;   ""
;;   (bx-murl-from-base-pre "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;;   )

;; (defun bxms:web:url:start-family.fa-blank-basicText.fa-post ()
;;   ""
;;   (bx-murl-from-base-post "~/BUE/mailings/start/family.fa/blank/basicText.fa")
;;   )

;; (defun bxms-web-url-start-family.fa-blank-basicText.fa ()
;;   ""
;;   (interactive)

;;   (setq  a-murl-pre-hook nil)
;;   (add-hook 'a-murl-pre-hook 'bxms:web:url:start-family.fa-blank-basicText.fa-pre)

;;   (setq  a-murl-post-hook nil)
;;   (add-hook 'a-murl-post-hook 'bxms:web:url:start-family.fa-blank-basicText.fa-post)
;;   )




;; Example  "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail"
(defun mcdt:setup/with-curBuffer (args)  "
** For use by external users. Primary Buffer Based User Interface.
"
  (interactive "p")
  (let* (
	 ($mailingFilePath (buffer-file-name))
	)
    (when $mailingFilePath
      (with-selected-window (frame-selected-window)
	(save-excursion
	  (mcdt:setup:gened/with-curBuffer args) ;; gened by macro
	  ))
      )
    (when (not $mailingFilePath)
      (message "Buffer Does Not Have A File -- Skipped")
      )
    )
  )


;;
;; (mcdt:mailing:getName|with-file "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail")
(defun mcdt:mailing:getName|with-file (<mailingFilePath)
  "Return the value of x-mailingname field of header of <mailingFilePath.
May be called from within macros with <mailingFilePath and not the mailingBuf being available.
x-mailingname should be all lower case.
Kills the mailingBuf."
  (let* (
	 ($mailingBuf (switch-to-buffer (find-file <mailingFilePath)))
	 ($result)
	 )
    (save-excursion
      (setq $result (mcdt:mailing:getName/with-buffer $mailingBuf))
      (kill-buffer $mailingBuf)
      )
    $result
    ))


;;
;; "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail"
;; (mcdt:mailing:getName/with-curBuffer)
;;
(defun mcdt:mailing:getName/with-buffer (<mailingBuf)
  "Return the value of x-mailingname field of header. x-mailingname should be all lower case."
  (interactive)
  (let* (
	 (result nil)
	)
    (setq result (bx:mail:header:field:get-from-buffer 'x-mailingname <mailingBuf))
    result
    )
  )

;;
;; (mcdt:mailing:getName/with-curBuffer)
;;
(defun mcdt:mailing:getComposeFwrk|with-buffer (<mailingBuf)
  "Return the value of x-composefwrk field of header. It should be all lower case.
Valid values are 'msgOrg or 'message. If field does not exist, message is assumed.
"
  (let* (
	 (result nil)
	)
    (setq result (bx:mail:header:field:get-from-buffer 'x-composefwrk <mailingBuf))
    (unless result
      (setq result "message"))
    result
    )
  )

;;
;; (bx:mail:header:field:get-from-buffer 'x-mailingparams (find-file "~/BUE/mailings/start/family.fa/blank/basicLatex.fa/basicLatex/mailingStatic/content.mail"))
;; (message "%s" (mcdt:mailing:params|from-buf )
;;
(defun mcdt:mailing:params|from-buf (<mailingBuf)
  "Return params as a list based on the string of X-MailingParams:.
x-mailingparams should be all lower-case.
params can be retrieved with plist."
  (let* (
	 ($paramsAsStr (bx:mail:header:field:get-from-buffer 'x-mailingparams <mailingBuf))
	 (params (append (list :name 'someName)
			 (read (concat "(" $paramsAsStr ")"))))
	 )
    (message "mcdt:mailing:params|from-buf: paramsAsStr=%s" $paramsAsStr)
    params))

;;; (message (buffer-name (mcdt:mailing|latest-unsent-mail-buf)))
(defun mcdt:mailing|latest-unsent-mail-buf ()
  "Return most recently created unsent mail buffer."
  (let ((found nil))
    (loop-for-each each (buffer-list)
                   (message (buffer-name each))
                   (when (s-contains? "unsent mail" (buffer-name each))
                     (setq found each)
                     (loop-break)))
    found))


(defun mcdt:mailing:compose|get-function-name (<mailingName)
  "Given <mailingName, return name of compose function"
  (concat "mcdt:compose/" <mailingName))

(defun mcdt:mailing:originate|get-function-name (<mailingName)
  "Given <mailingName, return name of originate function"
  (concat "mcdt:originate/" <mailingName))


(defun mcdt:setup-and-compose/with-file (<mailingFilePath)
  "Given a mailing file, initiate an outgoing message.
Used for example, in dblocks such as bxPanel:mailing/compose.
  - visit the file
  - setup  mcdt:compose/mailingName if needed
  - invoke mcdt:compose/mailingName go to the to field
 "
  (interactive)
  (let (
	($mailingName nil)
	($mailingBuf nil)
	($funcSymbol nil)
	)
    (find-file-read-only-other-frame <mailingFilePath)
    (setq $mailingBuf (current-buffer))
    (setq $mailingName (mcdt:mailing:getName/with-buffer $mailingBuf))
    (setq $funcSymbol (intern (mcdt:mailing:compose|get-function-name $mailingName)))
    (when (commandp $funcSymbol)
      ;;(switch-to-buffer $mailingBuf)
      (call-interactively $funcSymbol)
      )
    (when (not (commandp $funcSymbol))
      (mcdt:setup$with-filePath <mailingFilePath)
      (call-interactively $funcSymbol)
      )
    ;;; Most recent buffer should now be the "*unsent mail<n>*"
    (switch-to-buffer (mcdt:mailing|latest-unsent-mail-buf))
    )
  )

(defun mcdt:setup-and-compose/with-curBuffer () "\
** Convert buffer to filePath and call mcdt:setup-and-compose/with-file then."
  (interactive)
  (mcdt:setup-and-compose/with-file (buffer-file-name))
  )

;;
;;
;;
(defun mcdt:compose|with-file (<mailingFilePath args)
  "Out of macro work of mcdt:compose$mailing-defun.
ModuleLocal.
"
  (let* (
	 ($mailingBuf (switch-to-buffer (find-file <mailingFilePath)))
	 ($mailingParams (mcdt:mailing:params|from-buf $mailingBuf))
         ($ephemeraMailingFilePath nil)
	 (<extSrcBase (or (plist-get $mailingParams :extSrcBase) nil))
	 )
    (message (s-lex-format "mailingParams: ${$mailingParams} extSrcBase ${<extSrcBase}"))

    (unless <extSrcBase
      (mcdt:compose|basedOnMailingTemplateFile  <mailingFilePath)
      )
    (when <extSrcBase
      (setq $ephemeraMailingFilePath
	    (mcdt:compose:ephemera|copyToBase <mailingFilePath <extSrcBase))
      (mcdt:compose|basedOnMailingTemplateFile $ephemeraMailingFilePath)
      (mcdt:compose:ephemera|mailBufRecord
       (file-name-directory $ephemeraMailingFilePath)
       (buffer-name (mcdt:mailing|latest-unsent-mail-buf))
       )
      (display-buffer
       (switch-to-buffer (mcdt:mailing|latest-unsent-mail-buf)))
      )
    ))

;;;
;;; (mcdt:compose|basedOnMailingTemplateFile "/bxo/r3/iso/piu_mbFullUsage/mailings/compose/family/from/org-tex/content.orgMsg")
;;;
(defun mcdt:compose|basedOnMailingTemplateFile (<mailingFilePath)
  "Visits file and calls TemplateBuf version."
  (interactive)
  (save-excursion
  (mcdt:compose|basedOnMailingTemplateBuf (find-file <mailingFilePath))))


(defun mcdt:compose|basedOnMailingTemplateBuf (<mailingBuf)
  "Given a mailingBuf, run compose-mail and replace its content with template.
When composeFwrk is message, stay in message mode,
When composeFwrk is msgOrg, switch to org-msg-edit-mode."
  (let* (
	 ($mailingParams (mcdt:mailing:params|from-buf <mailingBuf))
         ($ephemeraMailingFilePath nil)
	 (<extSrcBase (or (plist-get $mailingParams :extSrcBase) nil))
	 ($composeFwrk (mcdt:mailing:getComposeFwrk|with-buffer <mailingBuf))
	 )
    (message (s-lex-format "mailingParams: ${$mailingParams} extSrcBase ${<extSrcBase} $composeFwrk=${$composeFwrk}"))

    (compose-mail)
    (erase-buffer)
    (insert-buffer-substring <mailingBuf)
    (message-goto-to)

    (cond
     ((string-equal $composeFwrk "message")
      (message-mode)
      )
     ((string-equal $composeFwrk "msgOrg")
      (org-msg-edit-mode)
      )
     (t
      (message (s-lex-format "Unknown $composeFwrk=${$composeFwrk}"))))
    ))


(defun mcdt:setup-and-originate/with-file (<mailingFilePath)
  "Given a mailing file, initiate an outgoing message.
Used for example, in dblocks such as bxPanel:mailing/originate.
  - visit the file
  - setup  mcdt:originate/mailingName if needed
  - invoke mcdt:originate/mailingName go to the to field
NOTYET, delete the BCC field, if there is one.
 "
  (interactive)
  (let (
	($mailingName nil)
	($mailingBuf nil)
	($funcSymbol nil)
	)
    (find-file-read-only <mailingFilePath)
    (setq $mailingBuf (current-buffer))
    (setq $mailingName (mcdt:mailing:getName/with-buffer $mailingBuf))
    (setq $funcSymbol (intern (mcdt:mailing:originate|get-function-name $mailingName)))
    (when (commandp $funcSymbol)
      ;;(switch-to-buffer $mailingBuf)
      (call-interactively $funcSymbol)
      )
    (when (not (commandp $funcSymbol))
      (mcdt:setup$with-filePath <mailingFilePath)
      (call-interactively $funcSymbol)
      )
    ;;; Most recent buffer should now be the "*unsent mail<n>*"
    (switch-to-buffer (mcdt:mailing|latest-unsent-mail-buf))
    (message-goto-bcc)
    (beginning-of-line 1)
    (kill-whole-line)
    (message-goto-to)
    )
  )

(defun mcdt:setup-and-originate/with-curBuffer () "
** Converts buffer to filePath and calls mcdt:setup-and-compose/with-file then.
  "
  (interactive)
  (mcdt:setup-and-originate/with-file (buffer-file-name))
  )


;;
;;
;;
(defun mcdt:originate|with-file (<mailingFilePath args)
  "Out of macro work of mcdt:originate$mailing-defun.
ModuleLocal.
"
  (let* (
	 ($mailingBuf (switch-to-buffer (find-file <mailingFilePath)))
	 ($mailingParams (mcdt:mailing:params|from-buf $mailingBuf))
         ($ephemeraMailingFilePath nil)
	 (<extSrcBase (or (plist-get $mailingParams :extSrcBase) nil))
	 )
    (message (s-lex-format "mailingParams: ${$mailingParams} extSrcBase ${<extSrcBase}"))
    (text-mode) ;; bxms-compose-from-base checks for major-mode
    (bxms-compose-from-base (expand-file-name (file-name-directory <mailingFilePath)) args)
    (display-buffer
     (switch-to-buffer (mcdt:mailing|latest-unsent-mail-buf)))
    ))


;;
;; (bx:ephemera:dated|pathName-in "/tmp")
;;
(defun bx:ephemera:dated|pathName-in (<baseDir)
  "Return (format-time-string \"%Y-%m-%d-%H-%M-%S\") plus a counter.
Which makes for a uniq file name.
If that date to a second exists, do a plus counter.
NOTYET, counter has not been implemented yet."
  (let* (
	 ($ephemeraUniqe (format-time-string "%Y-%m-%d-%H-%M-%S"))
	 ($result (f-join <baseDir $ephemeraUniqe))
	 )
    $result))

;;
;; (mcdt:compose:ephemera|copyToBase "~/BUE/mailings/start/family.fa/blank/basicText.fa/content.mail" ".")
;;
(defun mcdt:compose:ephemera|copyToBase (<mailingFilePath <extSrcBase)
  "Copy recursively <extSrcBase to mcdt:compose:ephemera:base."
  (let* (
	 ($mailingBaseDir (expand-file-name (file-name-directory <mailingFilePath)))
	 ($srcBase (f-join $mailingBaseDir <extSrcBase))
	 ($destBase (bx:ephemera:dated|pathName-in mcdt:compose:ephemera:base))
	 ($mailingRelativeToExtSrcBase (f-relative <mailingFilePath $srcBase))
	 ($ephemeraMailingFilePath (f-join $destBase $mailingRelativeToExtSrcBase))
	 ($shellCmndResult)
	 )
    (setq $shellCmndResult
	  (shell-command-to-string
	   (format "cp -r %s %s" $srcBase $destBase)))
    (message $shellCmndResult)
    $ephemeraMailingFilePath
    ))

(defun mcdt:compose:ephemera|mailBufRecord (<ephemeraMailingBaseDir <bufName)
  "Record name of <buf at <ephemeraMailingFilePath as mail.buf. It can then be obtained."
    (with-temp-file (concat (file-name-as-directory <ephemeraMailingBaseDir) "mail.buf")
      (insert (s-lex-format "${<bufName}"))))

(defun mcdt:compose:ephemera|mailBufObtain (<ephemeraMailingFilePath)
  "Record name of <buf at <ephemeraMailingFilePath as mail.buf. It can then be obtained."
  (let* (
         ($fileName (concat (file-name-directory <ephemeraMailingFilePath) "mail.buf"))
         )
    (if (file-readable-p $fileName)
        (f-read (concat (file-name-directory <ephemeraMailingFilePath) "mail.buf"))
      nil)))

;;;
;;; (mcdt:mailing:content|filePath)
;;;
(defun mcdt:mailing:content|findFile ()
  "Return nil if file does not exist. Based on existence, determine name of mailing content file.
NOTYET: mode could come from ($composeFwrk (mcdt:mailing:getComposeFwrk|with-buffer <mailingBuf))
We first look for content.msgOrg.
"
  (let* (
         ($filePath nil)
         )
    (cond
     ((file-readable-p (setq $filePath (f-join default-directory "content.orgMsg")))
      (find-file $filePath)
      (org-msg-edit-mode)
      )
     ((file-readable-p (setq $filePath (f-join default-directory "content.mail")))
      (find-file $filePath)
      (message-mode)
      )
     (t
      $filePath
      ))))

;;;
;;;
(defun mcdt/gotoMailBuf ()
  "Go to the unsent buffer or to mailing's content.mail buffer. Primarily used in mailing.mastex."
  (interactive)
  (let* (
         ($ephemeraMailBufName (mcdt:compose:ephemera|mailBufObtain default-directory))
         )
    (when $ephemeraMailBufName
      (display-buffer
       (switch-to-buffer $ephemeraMailBufName)))
    (unless $ephemeraMailBufName
      (mcdt:mailing:content|findFile)
      )
    ))


(defun mcdt:content:update/mailBufAndVisit ()
  "Goto mailBuf, update it, raise it. preview it.
Called when content has been edited and is ready."
  (mcdt/gotoMailBuf)
  (org-dblock-update-buffer-bx)
  (mml-preview)
  )

(defun mcdt:content:tex/buildSentinel (<proc <event)
  "This is triggered after build is complete. We go back to the mail buffer.
dblock update it and perview."
  (message (s-lex-format "mcdt:content:tex/buildSentinel triggered for proc=${<proc} event=${<event}"))
  (mcdt:content:update/mailBufAndVisit)
  )

(defun mcdt:content:tex/buildReleaseAndMailBuf ()
  "LaTeX build+release + display build continue in sentinel when build is complete."
  (interactive)
  (message "From buildReleaseAndMailBuf -- Running: lcntProc.sh -v -n showRun -i buildResultsRelease")
  (let* (
         ($tmpName (make-temp-name "-output"))
         ($bufName (format "*%s*" $tmpName))
         ($process nil))
    (save-buffer)
    (setq $process (start-process "lcntProc.sh" $bufName
                                  "/bin/sh" "-c" "lcntProc.sh -v -n showRun -i buildResultsRelease"))
    (set-process-sentinel $process 'mcdt:content:tex/buildSentinel)
    (display-buffer (switch-to-buffer $bufName))))

(defun mcdt:gnus:reply|ephemeraSetup ()
  "Triggered when replying with Gnus, after the article has been setup"
  (message "mcdt:gnus:reply|ephemeraSetup was triggered, likely from gnus-message-setup-hook")
  (let* (
	 ($point)
         ($ephemeraMailingFilePath:ltr nil)
         ($ephemeraMailingFilePath:rtl nil)
	 )
    (setq $point (search-forward "--citation follows this line (read-only)--" nil t))
    (when $point
      (forward-line -1)
      (insert "\n")
      (insert "\n#+BEGIN: bx:mtdt:content/actions")
      (insert "\n#+END:")
      (insert "\n")

      (setq $ephemeraMailingFilePath:ltr
	    (mcdt:compose:ephemera|copyToBase
             mcdt:reply:templates:leftToRight
             "."))

      (setq $ephemeraMailingFilePath:rtl
	    (mcdt:compose:ephemera|copyToBase
             mcdt:reply:templates:rightToLeft
             "."))

      (save-excursion
        (message-carefully-insert-headers (list (cons 'X-tmp-mailingPath-ltr $ephemeraMailingFilePath:ltr)))
        (message-carefully-insert-headers (list (cons 'X-tmp-mailingPath-rtl $ephemeraMailingFilePath:rtl)))
        (message-sort-headers)
        )

      (org-dblock-update-buffer-bx)
      )

    $point
    ))

(defun mcdt:originate:orgMsg|plusSetup ()
  "Addition originate features.

"
  (message "mcdt:originate:orgMsg|plusSetup was triggered, likely from gnus-message-setup-hook")
  (when org-msg-mode
    (let* (
	   ($point)
	   )
      (setq $point (search-forward "--citation follows this line (read-only)--" nil t))
      (when $point
        (forward-line -1)
        (insert "\n")
        (insert "\n#+BEGIN: bx:mtdt:content/actions")
        (insert "\n#+END:")
        (insert "\n"))

      (when (not $point)
        (forward-line -1)
        (insert "\n")
        (insert "\n#+BEGIN: bx:mtdt:content/actions")
        (insert "\n#+END:")
        (insert "\n"))

      (org-dblock-update-buffer-bx)
    $point
    )))



(defun mcdt:mailing:baseDir|set (<baseDir)
  "Setup the specified ephemeraBaseDir for current unsent mailBuf."
  (setq default-directory <baseDir)
  (save-excursion
    (message-carefully-insert-headers (list (cons 'X-tmp-mailingPath <baseDir)))
    (message-sort-headers)
    )
  (mcdt:gnus:reply|orgHtmlDblockSetup)
  (mcdt:compose:ephemera|mailBufRecord <baseDir (current-buffer))
  )

(defun mcdt:gnus:reply|orgHtmlDblockSetup ()
  "Called ephermaBase has been selected.
Is idempotent."
  (message "mcdt:gnus:reply|orgHtmlDblockSetup Entered")
  (let* (
	 ($orgHtmlDblockPoint nil)
         ($citationLinePoint nil)
	 )
    (save-excursion
      (goto-char (point-min))
      (setq $orgHtmlDblockPoint
            (search-forward "#+BEGIN: bx:file-insert:org:html :file" nil t))
      )

    (unless $orgHtmlDblockPoint
      (save-excursion
        (goto-char (point-min))
        (setq $citationLinePoint
              (search-forward "--citation follows this line (read-only)--" nil t))
        )

      (when $citationLinePoint
        (goto-char $citationLinePoint)
        (forward-line -1)
        (insert "\n")
        (insert "\n#+BEGIN: bx:file-insert:org:html :file \"./rel/mailing-html/index.html\"")
        (insert "\n#+END:")
        (insert "\n")
        ))))


;;;(add-hook 'message-setup-hook 'mcdt:gnus:reply|ephemeraSetup)
;;;(remove-hook 'message-setup-hook 'mcdt:gnus:reply|ephemeraSetup)

(add-hook 'gnus-message-setup-hook 'mcdt:gnus:reply|ephemeraSetup 91)

;;;
;;; (mcdt:compose-mail/basic)
;;;
(defun mcdt:compose-mail/basic ()
  "When org-msg mode is active, invoke compose-mail without it."
  (let* (
	 ($gnus-message-setup-hook gnus-message-setup-hook)
	 )
    ;;;

    (when org-msg-mode
      (message (s-lex-format "disabling orgMsg:: org-msg-mode was: ${org-msg-mode}"))
      (message (s-lex-format "before:: gnus-message-setup-hook was: ${gnus-message-setup-hook}"))
      (org-msg-mode -1)
      )
    (message (s-lex-format "gnus-message-setup-hook is: ${gnus-message-setup-hook}"))
    (compose-mail)
    ;;(setq gnus-message-setup-hook $gnus-message-setup-hook)
    ))

;;;
;;; (mcdt:compose-mail/orgMsg)
;;;
(defun mcdt:compose-mail/orgMsg ()
  "When org-msg mode is active, invoke compose-mail without it."
  (let* (
	 ($gnus-message-setup-hook gnus-message-setup-hook)
	 )
    ;;; (message (s-lex-format "mailingParams: ${$mailingParams} extSrcBase ${<extSrcBase} $composeFwrk=${$composeFwrk}"))

    (when (not org-msg-mode)
      (message (s-lex-format "enabling orgMsg:: org-msg-mode was: ${org-msg-mode}"))
      (message (s-lex-format "before:: gnus-message-setup-hook was: ${gnus-message-setup-hook}"))
      (org-msg-mode)
      )
    (message (s-lex-format "gnus-message-setup-hook is: ${gnus-message-setup-hook}"))
    (compose-mail)
    ;;(setq gnus-message-setup-hook $gnus-message-setup-hook)
    ))

;;;
;;; (mcdt:compose-mail/selected)
;;;
(defun mcdt:compose-mail/selected ()
  "When org-msg mode is active, invoke compose-mail without it."
  (let* (
	 ($gnus-message-setup-hook gnus-message-setup-hook)
	 )
    (message (s-lex-format "org-msg-mode=${org-msg-mode}"))
    (compose-mail)
    ))


;;;
;;; (process-id (get-buffer-process (lsip-buffer-for-host "localhost")))
;;; (process-buffer (lsip-buffer-for-host "localhost"))
;;;
;; (defun mcdt:content:tex/buildReleaseAndMailBuf%% ()
;;   "Abandoned. Could not get it it to work.
;; Signal was not being seen by Sentinel.
;; Simpler version above works fine.
;; Needs to be revisited."
;;   (interactive)
;;   (message "From buildReleaseAndMailBuf -- Running: lcntProc.sh -v -n showRun -i buildResultsRelease; kill -SIGINT $$")
;;   (lsip-local-run-command-here "lcntProc.sh -v -n showRun -i buildResultsRelease; kill -SIGINT $$")
;;   (let* (
;;          ($shell-buffer (lsip-buffer-for-host "localhost"))
;;          ($shell-process (get-buffer-process $shell-buffer))
;;          )
;;     (message (format "%s" (process-command $shell-process)))
;;     (set-process-sentinel $shell-process 'mcdt:content:tex/buildSentinel)
;;     ;;;
;;     ;;; This is the signal that triggers the sentinel
;;     ;;; Rest happens in there
;;     (message (format "%s From buildReleaseAndMailBuf -- Running: kill -SIGINT $$" $shell-process))
;;     (sleep-for 1)
;;     (lsip-local-run-command-here "kill -SIGINT $$")
;;     (interrupt-process $shell-process)
;;   )))


;; (defun mcdt:bbdb:actions-placeHolder% (mailingName)
;;   "This is a place holder for now, to be sorted out later."
;;   (setq bbdb-action-alist
;; 	(append
;; 	 bbdb-action-alist
;; 	 '(("bxms-compose-start-family.fa-blank-basicText.fa"
;; 	    ;;
;; 	    (setq bbdb-action-hook nil)
;; 	    (add-hook 'bbdb-action-hook 'bxms:bbdb:compose:start-family.fa-blank-basicText.fa)
;; 	    ))))


;;   (setq bbdb-action-alist
;; 	(append
;; 	 bbdb-action-alist
;; 	 '(("bxms-toall-start-family.fa-blank-basicText.fa"
;; 	    ;;
;; 	    (setq bbdb-action-hook nil)
;; 	    (add-hook 'bbdb-action-hook 'bxms:bbdb:toall:start-family.fa-blank-basicText.fa)
;; 	    ))))

;;   (setq bbdb-action-alist
;; 	(append
;; 	 bbdb-action-alist
;; 	 '(("bxms-batch-start-family.fa-blank-basicText.fa"
;; 	    ;;
;; 	    (setq bbdb-action-hook nil)
;; 	    (add-hook 'bbdb-action-hook 'bxms:bbdb:batch:start-family.fa-blank-basicText.fa)
;; 	    ))))
;;   )

(provide 'mcdt-if)
