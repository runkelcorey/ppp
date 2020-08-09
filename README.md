# ppp
Code to evaluate the SBA's first disclosure of Paycheck Protection Program loans.

## How to use
First, download the Small Business Administration's [loan-level data](https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp). Extract the files to a folder named `data/`. Then, follow `import.R`, which will combine the loans into one large (723.2MB) flat file, clean up the odd date format, and opine on helpful, new variables. Do whatever you like after that! Some plots I've made reside in `plots/`, and their generative scripts (minus the Yale themes) live in `visualize/`.

## Featured work
This repository was created to facilitate work that I did for the [Yale Program on Financial Stability](https://som.yale.edu/faculty-research-centers/centers-initiatives/program-on-financial-stability).

* [Paycheck Protection Program highlights numerous oversight concerns even as the SBA makes first disclosures](https://som.yale.edu/blog/paycheck-protection-program-highlights-numerous-oversight-concerns-even-as-the-sba-makes-first-disclosures)
* [Paycheck Protection Program spread loans widely, if not evenly](https://som.yale.edu/blog/paycheck-protection-program-spread-loans-widely-if-not-evenly)
