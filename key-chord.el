;;; key-chord.el --- map pairs of simultaneously pressed keys to commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2005, 2008, 2012 David Andersson

;; Author: David Andersson <l.david.andersson(at)sverige.nu>
;; Maintainer: LemonBreezes <look@strawberrytea.xyz>
;; Package-Version: 0.7.1
;; Package-Requires: ((emacs "24"))
;; Keywords: keyboard chord input
;; URL: https://github.com/LemonBreezes/key-chord

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This package implements support for mapping a pair of simultaneously
;; pressed keys to a command and for mapping the same key being pressed
;; twice in quick succession to a command. Such bindings are called
;; "key chords".

;;; Code:

(defgroup key-chord nil
  "Map pairs of simultaneously pressed keys to commands."
  :group 'bindings)

(defcustom key-chord-two-keys-delay 0.1
  "Max time delay between two key press to be considered a key chord."
  :type 'float)

(defcustom key-chord-one-key-delay 0.2
  "Max time delay between two press of the same key to be considered a key chord.
This should normally be a little longer than `key-chord-two-keys-delay'."
  :type 'float)

(defcustom key-chord-in-macros nil
  "If nil, don't expand key chords when executing keyboard macros.

If non-nil, expand chord sequenses in macros, but only if a similar
chord was entered during the last interactive macro recording. (This
carries a bit of guesswork. We can't know for sure when executing
whether two keys were typed quickly or slowly when recorded. Be careful
enabling this as it could cause key chords to be expanded in places
where they shouldn't be)."
  :type 'boolean)

(defcustom key-chord-one-key-min-delay 0.0
  "Minimum delay (in seconds) between two presses for a double-tap key-chord (using the same key)
to be recognized.  If the delay between two identical key presses is less than this value (as when holding a key),
the chord will not trigger."
  :type 'float)

(defcustom key-chord-typing-detection nil
  "If non-nil, try to detect when user is typing text and disable chord detection temporarily.
This helps avoid accidental chord triggering during fast typing. This
also dramatically improves the performance of key-chord when typing text
by acting like a debounce."
  :type 'boolean)

(defcustom key-chord-typing-speed-threshold 0.1
  "Maximum delay (in seconds) between keystrokes to be considered part of typing flow.
If keys are pressed faster than this threshold, key-chord detection will be temporarily disabled."
  :type 'float)

(defcustom key-chord-typing-reset-delay 0.5
  "Time (in seconds) after which to reset typing detection if no keys are pressed.
After this much idle time, key-chord detection will be re-enabled."
  :type 'float)

(defcustom key-chord-use-key-tracking t
  "If non-nil, track which keys are used in chords to optimize lookups.
This improves performance by avoiding unnecessary key-chord-lookup-key calls.
However, it could potentially cause issues if key chords are defined
outside of the normal key-chord-define* functions."
  :type 'boolean)

;; Internal vars
(defvar key-chord-mode nil)

;; Shortcut for key-chord-input-method: no need to test a key again if it
;; didn't matched a chord the last time. Improves feedback during autorepeat.
(defvar key-chord-last-unmatched nil)

;; Macro heuristics: Keep track of which chords was used when the last macro
;; was defined. Or rather, only the first-char of the chords. Only expand
;; matching chords during macro execution.
(defvar key-chord-in-last-kbd-macro nil)
(defvar key-chord-defining-kbd-macro nil)

;; Typing detection variables
(defvar key-chord-in-typing-flow nil
  "Non-nil when user appears to be typing text rather than executing commands.")
(defvar key-chord-last-key-time nil
  "Time when the last key was pressed.")
(defvar key-chord-typing-timer nil
  "Timer to reset typing detection mode.")

;; Key tracking for optimization
(defvar key-chord-keys-in-use (make-vector 256 nil)
  "Vector indicating which keys are used in any chord.")

;;;###autoload
(define-minor-mode key-chord-mode
  "Map pairs of simultaneously pressed keys to commands.

See functions `key-chord-define-global', `key-chord-define-local',
and `key-chord-define' and variables `key-chord-two-keys-delay'
and `key-chord-one-key-delay'."
  :global t
  (setq input-method-function
        (and key-chord-mode
             'key-chord-input-method))
  (key-chord-reset-typing-detection))

;;;###autoload
(defun key-chord-define-global (keys command)
  "Define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

Note that KEYS defined locally in the current buffer will have
precedence."
  (interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-chord-define (current-global-map) keys command))

;;;###autoload
(defun key-chord-define-local (keys command)
  "Locally define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode."
  (interactive "sSet key chord locally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-chord-define (current-local-map) keys command))

(defun key-chord-unset-global (keys)
  "Remove global key-chord of the two keys in KEYS."
  (interactive "sUnset key chord globally (2 keys): ")
  (key-chord-define (current-global-map) keys nil))

(defun key-chord-unset-local (keys)
  "Remove local key-chord of the two keys in KEYS."
  (interactive "sUnset key chord locally (2 keys): ")
  (key-chord-define (current-local-map) keys nil))

(defun key-chord-update-keys-in-use (key1 key2 command)
  "Update the keys-in-use vector based on the chord being defined or removed."
  (if command
      (progn
        ;; Adding a chord - mark keys as in use
        (aset key-chord-keys-in-use key1 t)
        (aset key-chord-keys-in-use key2 t))
    ;; Removing a chord - need to check if keys are used in other chords
    (let ((remove-key1 t)
          (remove-key2 t))
      ;; Check if keys are used in any other chords
      (let ((maps (list (current-global-map) (current-local-map))))
        (dolist (map (append maps (current-minor-mode-maps)))
          (when map
            ;; Check if key1 is used in any other chord
            (dotimes (k 256)
              (when (and (not (and (= k key2) (= key1 key2)))
                         (or (key-chord-lookup-key1 map (vector 'key-chord key1 k))
                             (key-chord-lookup-key1 map (vector 'key-chord k key1))))
                (setq remove-key1 nil)))

            ;; Check if key2 is used in any other chord
            (dotimes (k 256)
              (when (and (not (and (= k key1) (= key1 key2)))
                         (or (key-chord-lookup-key1 map (vector 'key-chord key2 k))
                             (key-chord-lookup-key1 map (vector 'key-chord k key2))))
                (setq remove-key2 nil))))))

      ;; Remove keys from tracking vector if not used elsewhere
      (when remove-key1 (aset key-chord-keys-in-use key1 nil))
      (when remove-key2 (aset key-chord-keys-in-use key2 nil)))))

;;;###autoload
(defun key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed."
  (when (/= 2 (length keys))
    (error "Key-chord keys must have two elements"))
  (let ((key1 (aref keys 0))
        (key2 (aref keys 1)))
    ;; Check that both keys are bytes
    (unless (and (integerp key1) (< key1 256)
                 (integerp key2) (< key2 256))
      (error "Key-chord keys must both be bytes (characters with codes < 256)"))
    
    (if (eq key1 key2)
        (define-key keymap (vector 'key-chord key1 key2) command)
      (define-key keymap (vector 'key-chord key1 key2) command)
      (define-key keymap (vector 'key-chord key2 key1) command))
    ;; Update the key tracking
    (key-chord-update-keys-in-use key1 key2 command)))

(defun key-chord-lookup-key1 (keymap key)
  "Like lookup-key but no third arg and no numeric return value."
  (let ((res (lookup-key keymap key)))
    (and (not (numberp res))
         res)))

(defun key-chord-lookup-key (key)
  "Lookup KEY in all current key maps."
  (let ((maps (current-minor-mode-maps))
        res)
    (while (and maps (not res))
      (setq res (key-chord-lookup-key1 (car maps) key))
      (setq maps (cdr maps)))
    (or res
        (and (current-local-map)
             (key-chord-lookup-key1 (current-local-map) key))
        (key-chord-lookup-key1 (current-global-map) key))))

(defun key-chord-describe ()
  "List key chord bindings in a help buffer.

Two key chords will be listed twice and there will be Prefix
Commands. Please ignore that."
  (interactive)
  (describe-bindings [key-chord]))

(defun key-chord-reset-typing-detection ()
  "Reset typing detection state when key-chord-mode is toggled."
  (setq key-chord-in-typing-flow nil)
  (setq key-chord-last-key-time nil)
  (when key-chord-typing-timer
    (cancel-timer key-chord-typing-timer)
    (setq key-chord-typing-timer nil)))

(defun key-chord-reset-typing-mode ()
  "Reset the typing detection mode."
  (setq key-chord-in-typing-flow nil)
  (setq key-chord-typing-timer nil))

(defun key-chord-check-typing-flow (current-time)
  "Check if user is in typing mode based on timing between keystrokes."
  (when key-chord-typing-detection
    ;; Cancel existing timer if any
    (when key-chord-typing-timer
      (cancel-timer key-chord-typing-timer))

    ;; Set idle timer to reset typing mode after idle period
    (setq key-chord-typing-timer
          (run-at-time key-chord-typing-reset-delay nil
                       #'key-chord-reset-typing-mode))

    ;; Check if we're in typing flow based on timing
    (unless key-chord-in-typing-flow
      (when key-chord-last-key-time
        (let ((elapsed (float-time (time-subtract current-time key-chord-last-key-time))))
          (when (< elapsed key-chord-typing-speed-threshold)
            (setq key-chord-in-typing-flow t)))))

    ;; Update last key time
    (setq key-chord-last-key-time current-time)))

(defun key-chord-input-method (first-char)
  "Input method controlled by key bindings with the prefix `key-chord'."
  ;; Check typing mode (but not during macro execution)
  (unless executing-kbd-macro
    (key-chord-check-typing-flow (current-time)))

  (cond
   ;; Skip non-byte characters
   ((not (and (integerp first-char)
              (< first-char 256)))
    (list first-char))

   ;; Skip chord detection if in typing mode (but not during macro execution)
   ((and key-chord-typing-detection
         key-chord-in-typing-flow
         (not executing-kbd-macro))
    (setq key-chord-last-unmatched first-char)
    (list first-char))

   ;; Skip chord detection during macro execution if key-chord-in-macros is nil
   ;; or if the key is not in a recorded chord
   ((and executing-kbd-macro
         (or (not key-chord-in-macros)
             (not (memq first-char key-chord-in-last-kbd-macro))))
    (setq key-chord-last-unmatched first-char)
    (list first-char))

   ;; Optimization: Skip chord detection completely if key tracking is enabled
   ;; and first-char is not used in any chord
   ((and key-chord-use-key-tracking
         (not (aref key-chord-keys-in-use first-char)))
    (setq key-chord-last-unmatched first-char)
    (list first-char))

   ;; Continue with chord detection for keys that might be part of a chord
   ((not (eq first-char key-chord-last-unmatched))
    (let ((res (key-chord-lookup-key (vector 'key-chord first-char))))
      (if (not res)
          (progn
            (setq key-chord-last-unmatched first-char)
            (list first-char))
        (let ((start-time (current-time))
              (delay (if (key-chord-lookup-key
                          (vector 'key-chord first-char first-char))
                         key-chord-one-key-delay
                       key-chord-two-keys-delay)))
          (cond
           ((input-pending-p)
            (let ((next-char (read-event nil nil delay)))
              (if (null next-char)
                  (progn
                    (setq key-chord-last-unmatched first-char)
                    (list first-char))
                (if (and (eq first-char next-char)
                         (or (< (float-time (time-subtract (current-time) start-time))
                                key-chord-one-key-min-delay)
                             executing-kbd-macro))
                    (progn
                      (setq unread-command-events (cons next-char unread-command-events))
                      (setq key-chord-last-unmatched first-char)
                      (list first-char))
                  ;; Optimization: Only do lookup if next-char is a valid key in any chord
                  ;; or if key tracking is disabled
                  (if (and (or (not key-chord-use-key-tracking)
                               (aref key-chord-keys-in-use next-char))
                           (key-chord-lookup-key (vector 'key-chord first-char next-char)))
                      (progn
                        (setq key-chord-defining-kbd-macro
                              (cons first-char key-chord-defining-kbd-macro))
                        (list 'key-chord first-char next-char))
                    (setq unread-command-events (cons next-char unread-command-events))
                    (when (eq first-char next-char)
                      (setq key-chord-last-unmatched first-char))
                    (list first-char))))))
           (t ; no input pending
            (let ((next-char (read-event nil nil delay)))
              (if (null next-char)
                  (progn
                    (setq key-chord-last-unmatched first-char)
                    (list first-char))
                (if (and (eq first-char next-char)
                         (or (< (float-time (time-subtract (current-time) start-time))
                                key-chord-one-key-min-delay)
                             executing-kbd-macro))
                    (progn
                      (setq unread-command-events (cons next-char unread-command-events))
                      (setq key-chord-last-unmatched first-char)
                      (list first-char))
                  ;; Optimization: Only do lookup if next-char is a valid key in any chord
                  ;; or if key tracking is disabled
                  (if (and (or (not key-chord-use-key-tracking)
                               (aref key-chord-keys-in-use next-char))
                           (key-chord-lookup-key (vector 'key-chord first-char next-char)))
                      (progn
                        (setq key-chord-defining-kbd-macro
                              (cons first-char key-chord-defining-kbd-macro))
                        (list 'key-chord first-char next-char))
                    (setq unread-command-events (cons next-char unread-command-events))
                    (when (eq first-char next-char)
                      (setq key-chord-last-unmatched first-char))
                    (list first-char)))))))))))
   (t ; key was last unmatched
    (setq key-chord-last-unmatched first-char)
    (list first-char))))

(defun key-chord--start-kbd-macro (_append &optional _no-exec)
  (setq key-chord-defining-kbd-macro nil))
(advice-add 'start-kbd-macro :after #'key-chord--start-kbd-macro)

(defun key-chord--end-kbd-macro (&optional _repeat _loopfunc)
  (setq key-chord-in-last-kbd-macro key-chord-defining-kbd-macro))
(advice-add 'end-kbd-macro :after #'key-chord--end-kbd-macro)

(provide 'key-chord)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; key-chord.el ends here
