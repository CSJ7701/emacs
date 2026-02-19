;;; name: Hledger
;;; depends:
;;; conflicts: ledger
;;; description:

(use-package ledger-mode)

(load (format "%smodules/ledger-functions.el" user-emacs-directory))

(setq ledger-binary-path "hledger"
      ledger-mode-should-check-version nil
      ledger-report-auto-width nil
      ledger-report-links-in-register nil
      ledger-report-native-highlighting-arguments '("--color=always"))

(setq ledger-reports
      '(("Balance" "%(binary) -f ~/Personal/Ledger/Main.ledger bal not:Budget not:Equity")
	("Balance (Monthly)" "%(binary) -f ~/Personal/Ledger/Main.ledger bal -p %(month) -S")
	("Register" "%(binary) -f ~/Personal/Ledger/Main.ledger reg")
	("Register (Account)" "%(binary) -f ~/Personal/Ledger/Main.ledger reg %(account)")
	("Budget (This Month)" "python ~/Personal/Ledger/Commands/Hledger/Budget.py --month %(month)")
	("Budget (Other Month)" "python ~/Personal/Ledger/Commands/Hledger/Budget.py --month %(othermonth)")
   ))
