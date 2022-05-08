;;;-*- mode: Emacs-Lisp; lexical-binding: t ; -*-
;;;
;;;

(require 'easymenu)
(require 'mcdt-if)

;; (mcdt:menu:plugin|install modes:menu:global (s-- 2))
(defun mcdt:menu:plugin|install (<menuLabel <menuDelimiter)
  "Adds this as a submenu to menu labeled <menuLabel at specified delimited <menuDelimiter."

  ;; Determine starting value of mcdt:compose:fashion
  (setq  mcdt:compose:fashion mcdt:compose:fashion::basic)
  (when org-msg-mode
    (setq  mcdt:compose:fashion mcdt:compose:fashion::orgMsg))
  (mcdt:compose:fashion/setup mcdt:compose:fashion)

  (easy-menu-add-item
   <menuLabel
   nil
   (mcdt:menu:select|define :active t)
   <menuDelimiter
   )
  )

;;
;;
(defun mcdt:menuItem:selected|define ()
  "Return a menuItem vector. Requires dynamic update."
  (car
   `(
     [,(format "Compose with fashion :  %s"
	       mcdt:compose:fashion)
      (mcdt:compose-mail/selected)
      :help "With Selected Fashion, compose-mail"
      :active t
      :visible t
      ]
     )))

(defun mcdt:menuItem:setup-withCurBuffer|define ()
  "Return a menuItem vector."
  (car
   `(
     [,(format "MCDT Setup With Current Buffer")
      (mcdt:setup/with-curBuffer)
      :help "Mail Composition Distribution and Tracking (MCDT) Setup With Current Buffer -- (mcdt:setup/with-curBuffer)"
      :active t
      :visible t
      ]
     )))


;;
;; [[elisp:(popup-menu (symbol-value (browsers:menu:help|define)))][This Menu]]
;; (popup-menu (symbol-value (mcdt:menu:select|define)))
;;
(defun mcdt:menu:select|define (&rest <namedArgs)
  "Return mcdt:menu:select
:active and :visible can be specified as <namedArgs.
"
  (let (
	(<visible (get-arg <namedArgs :visible t))
	(<active (get-arg <namedArgs :active t))
	($thisFuncName (compile-time-function-name))
	)

    ;; (setq $:browsers:menu:browse-url:at-point:active <active)
    ;; (setq $:browsers:menu:browse-url:at-point:visible <visible)

    (easy-menu-define
      mcdt:menu:select
      nil
      "Menu For Configuration Of browse-url-browser-function"
      `(,(format "Select Outmailer:: %s" mcdt:compose:fashion)
	:help "Show And Set Relevant Parameters"
	:visible ,<visible
	:active ,<active
	,(s-- 3)
	[
	,(format "**selected fashion = %s**" mcdt:compose:fashion)
	  (describe-variable 'mcdt:compose:fashion)
	  :help "Describe current value of browse-url-browser-function"
	  :active t
	  :visible t
	  ]
	,(s-- 4)
	 [
	  "Basic"
	  (mcdt:compose:fashion/setup mcdt:compose:fashion::basic)
	  :help "Select basic composition fashion."
	  :active t
	  :visible t
	  :style radio
	  :selected ,(eq  mcdt:compose:fashion mcdt:compose:fashion::basic)
	  ]
	 [
	  "OrgMsg"
	  (mcdt:compose:fashion/setup mcdt:compose:fashion::orgMsg)
	  :help "Select orgMsg composition fashion."
	  :active t
	  :visible t
	  :style radio
	  :selected ,(eq mcdt:compose:fashion mcdt:compose:fashion::orgMsg)
	  ]
	 [
	  "LaTeX"
	  (mcdt:compose:fashion/setup  mcdt:compose:fashion::latex)
	  :help "Select latex composition fashion."
	  :active t
	  :visible t
	  :style radio
	  :selected ,(eq mcdt:compose:fashion mcdt:compose:fashion::latex)
	  ]
	 ,(s-- 5)
	 ,(s-- 6)
	 ,(s-- 7)
	 ,(s-- 8)
	 ))

    (easy-menu-add-item
     mcdt:menu:select
     nil
     (mcdt:menuItem:selected|define)
     (s-- 6))

    (easy-menu-add-item
     mcdt:menu:select
     nil
     (mcdt:menuItem:setup-withCurBuffer|define)
     (s-- 7))

    (easy-menu-add-item
     mcdt:menu:select
     nil
     (bx:menu:panelAndHelp|define
      "/bisos/git/auth/bxRepos/blee-binders/bisos-core/sync/_nodeBase_"
      $thisFuncName
      (intern (symbol-name (gensym))))
     (s-- 8))

    'mcdt:menu:select
    ))


(provide 'mcdt-menu)
