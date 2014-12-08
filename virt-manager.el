;;; virt-manager.el --- Emacs major mode for driving virtual machines

;; Copyright (C) 2014 Jan Synáček

;; Author: Jan Synáček <jan.synacek@gmail.com>
;; URL: https://github.com/jsynacek/emacs-virt-manager
;; Maintainer: Jan Synáček <jan.synacek@gmail.com>
;; Created: Dec 2014
;; Keywords: virt-manager

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Emacs major mode for managing virtual machines.

;;; TODO:

;; - variable to control confirmation of reboots, shutdowns, ...
;; - make parsing immune to extra spaces in machine names
;; - document code...
;; - implement ability to view available snapshots (bind to <tab> ?)
;; - display amount of snapshots
;; - make vm-snapshot-delete-current-machine take a prefix arg and if it's there,
;;   delete current snapshot
;; - implement virsh edit (use emacsclient as editor?)

;;; Code:

(defvar vm-buffer-name "*virtual machines*")

(defvar vm-default-connection
  (let ((default-connection "qemu:///system")
	(connection (getenv "VIRSH_DEFAULT_CONNECT_URI")))
    (if (or (null connection)
	    (string= connection ""))
	default-connection
      connection)))

;; faces
(make-face 'vm-header-face)
(set-face-attribute 'vm-header-face nil
		    :inherit 'default
		    :weight 'bold)

(make-face 'vm-machine-face)
(set-face-attribute 'vm-machine-face nil
		    :inherit 'default
		    :foreground "#268bd2" ; solarized blue
		    :weight 'bold)

(make-face 'vm-machine-state-running-face)
(set-face-attribute 'vm-machine-state-running-face nil
		    :inherit 'default
		    :foreground "#859900" ; solarized green
		    :weight 'bold)

(make-face 'vm-machine-state-paused-face)
(set-face-attribute 'vm-machine-state-paused-face nil
		    :inherit 'default
		    :foreground "#b58900" ; solarized yellow
		    :weight 'bold)

(make-face 'vm-machine-state-shut-off-face)
(set-face-attribute 'vm-machine-state-shut-off-face nil
		    :inherit 'default
		    :foreground "#586e75") ; solarized base01

(defun vm-state-to-face (state)
  (cond
   ((string= "running" state)
    'vm-machine-state-running-face)
   ((string= "paused" state)
    'vm-machine-state-paused-face)
   ((string= "shut off" state)
    'vm-machine-state-shut-off-face)
   (t 'italic)))

;; buffer content

(defun vm-get-virtual-machines ()
  (mapcar
   (lambda (line)
     (split-string line "  +" t))
   (split-string
    (with-temp-buffer
      (vm-virsh t "list" "--all")
      (goto-char (point-min))
      (kill-whole-line 2) ;; TODO messes up the kill ring
      (buffer-string))
    "[\n]" t)))

(defun vm-insert-header ()
  (insert (propertize (format "%3s %-30s %-10s" ;; TODO make customizable
			      "Id" "Machine" "State")
		      'font-lock-face 'vm-header-face)
	  "\n"
	  (propertize (make-string 43 ?-)
		      'font-lock-face 'vm-header-face)
	  "\n"))

(defun vm-insert-line (line-list)
  ""
  (let ((id (car line-list))
	(machine (cadr line-list))
	(state (mapconcat
		#'identity
		(nthcdr 2 line-list)
		" ")))
    (insert
     ; id
     (format "%3s" id)
     " "
     ; machine
     (propertize (format "%-30s" machine)
		 'font-lock-face 'vm-machine-face)
     " "
     ; state
     (propertize (format "%-10s" state)
		 'font-lock-face (vm-state-to-face state))
     " ")))

(defun vm-insert-content ()
  (vm-insert-header)
  (mapc
   (lambda (line-list)
     (progn
       (vm-insert-line line-list)
       (newline)))
   (vm-get-virtual-machines)))


(defun vm-get-current-machine ()
  (let ((current-line (buffer-substring-no-properties
		       (line-beginning-position)
		       (line-end-position))))
    (split-string current-line)))

(defun vm-refresh ()
  (interactive)
  (let ((buf (get-buffer-create vm-buffer-name)))
    (with-current-buffer buf
      (let ((read-only-flag buffer-read-only)
	    (point (point)))
	(setq buffer-read-only nil)
	(delete-region (point-min) (point-max))
	(vm-insert-content)
	(goto-char point)
	(setq buffer-read-only read-only-flag)))))

(defun vm-header-line-p ()
  (save-excursion
    (forward-line 0)
    (looking-at "^[ ]+Id[ ]+Machine[ ]+")))

(defun vm-divider-line-p ()
  (save-excursion
    (forward-line 0)
    (looking-at "^---+$")))

;; movement
(defun vm-first-line ()
  (goto-char (point-min))
  (forward-line 2))

(defun vm-next-line ()
  (interactive)
  (forward-line)
  (when (vm-divider-line-p)
    (forward-line))
  (when (looking-at "[\n]*$")
    (forward-line -1)))

(defun vm-previous-line ()
  (interactive)
  (forward-line -1)
  (when (or (vm-divider-line-p)
	    (vm-header-line-p))
    (vm-first-line)))

;; virsh util

(defun join-string (&rest args)
  (mapconcat #'identity
	     args
	     " "))

(defmacro with-current-machine (&rest body)
  `(let ((machine (cadr (vm-get-current-machine))))
     ,@body))

(defmacro vm-virsh (destination &rest args)
  `(let ((ret
	  (call-process "virsh"  nil ,destination nil "-c" vm-default-connection ,@args)))
     (when (> ret 0)
       (error "Executing '%s' failed" (join-string ,@args)))
     ret))

;; machine state altering

(defun vm-change-current-machine-state (state)
  (with-current-machine
    (vm-virsh nil state machine)
    (message (format "State of %s changing to '%s'. Refresh the buffer." machine state))
    (vm-refresh)))

(defun vm-start-current-machine ()
  (interactive)
  (vm-change-current-machine-state "start"))

(defun vm-reboot-current-machine ()
  (interactive)
  (vm-change-current-machine-state "reboot"))

(defun vm-shutdown-current-machine ()
  (interactive)
  (vm-change-current-machine-state "shutdown"))

(defun vm-destroy-current-machine ()
  (interactive)
  (vm-change-current-machine-state "destroy"))

(defun vm-suspend-current-machine ()
  (interactive)
  (vm-change-current-machine-state "suspend"))

(defun vm-resume-current-machine ()
  (interactive)
  (vm-change-current-machine-state "resume"))

(defun vm-snapshot-current-machine (name)
  (interactive "sSnapshot name: ")
  (with-current-machine
    (if (not (string= name ""))
	(progn
	  (vm-virsh nil "snapshot-create-as" machine name)
	  (message "Snapshot '%s' of %s created" name machine))
      (error "No snapshot name given, aborting"))))

(defun vm-snapshot-delete-current-machine (&optional current)
  (interactive "P")
  (with-current-machine
   (let ((snapshots
	   (split-string
	    (with-temp-buffer
	      (vm-virsh t "snapshot-list" machine "--name")
	      (buffer-string))
	    "[\n]+"
	    t)))
     (if snapshots
	 (let ((snapshot
		(ido-completing-read "Snapshot: " snapshots)))
	   (vm-virsh nil "snapshot-delete" machine snapshot)
	   (message "Snapshot '%s' of %s deleted" snapshot machine))
       (message "No snapshots available")))))

;; main

(defun virt-manager ()
  (interactive)
  (let ((buf (get-buffer-create vm-buffer-name)))
    (with-current-buffer buf
      (unless buffer-read-only
	(vm-insert-content))
      (virt-manager-mode))
    (switch-to-buffer buf)
    (goto-line 3)))

(defvar virt-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'vm-next-line)
    (define-key map (kbd "p") 'vm-previous-line)
    (define-key map (kbd "s") 'vm-start-current-machine)
    (define-key map (kbd "r") 'vm-reboot-current-machine)
    (define-key map (kbd "S") 'vm-shutdown-current-machine)
    (define-key map (kbd "d") 'vm-destroy-current-machine)
    (define-key map (kbd "h") 'vm-suspend-current-machine)
    (define-key map (kbd "u") 'vm-resume-current-machine)
    (define-key map (kbd "t") 'vm-snapshot-current-machine)
    (define-key map (kbd "T") 'vm-snapshot-delete-current-machine)
    (define-key map (kbd "g") 'vm-refresh)
    map))

(define-derived-mode virt-manager-mode special-mode "Virtual Manager"
  "TODO Doc"
  ;(setq buffer-read-only t)
  (buffer-disable-undo))

(provide 'virt-manager)
