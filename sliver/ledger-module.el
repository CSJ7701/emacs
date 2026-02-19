;;; name: Ledger
;;; depends:
;;; conflicts: hledger
;;; description:


(use-package ledger-mode)
(load (format "%smodules/ledger-functions.el" user-emacs-directory))

(setq ledger-reports
      '(("Balance" "%(binary) -f ~/Personal/Ledger/Main.ledger bal not \\(Budget or Equity\\)")
       ("Monthly Balance" "%(binary) -f ~/Personal/Ledger/Main.ledger bal -p %(month) -S amount")
       ("2 Month Expenses" "python ~/Personal/Ledger/Commands/Two_Month.py")       
       ("Budget (This Month)" "python ~/Personal/Ledger/Commands/Budget.py %(month)")
       ("Budget (Other Month)" "python ~/Personal/Ledger/Commands/Budget.py %(othermonth)")
       ("Net Worth" "%(binary) -f ~/Personal/Ledger/Main.ledger bal Assets Liabilities")))
