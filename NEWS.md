# HybriFlow 0.3.0

* The follow-up report now opens with a participant flow diagram (Figure 1). It
  shows how many QC-passed women have unreadable visit dates, how many are seen
  at baseline only, and how many have 1 or more follow-up visits, split by total
  number of visits. This replaces the bar chart of women seen at each visit,
  which was easy to confuse with the number of visits per woman.
* The visit counts in the diagram come from the same data as the follow-up
  workbook, so the figure and the tables always agree.
* Persistence is now defined strictly: a baseline genotype *persisted* only if it
  is detected at baseline and at every follow-up visit. A genotype that is
  missed at any follow-up visit counts as *cleared*, even if it is detected
  again later (these cases are still listed in `intermittent_types`, and the
  woman is flagged in `relapse`). Previously a genotype counted as persisted if
  it was detected at the last visit, regardless of the visits in between. This
  changes the six categories, the genotype-level figure and the summary tables.
* Dates are now read and checked more carefully. The visit code in the Sample ID
  increases with every visit, so it is used to settle dates that can be read two
  ways (`1092018` = 9 Jan or 9 Oct; `11032025` = 11 Mar or 3 Nov). Dates written
  with a 2-digit year (`070422`, `90522`) are now read in full when the data
  show a single day/month order, so those women can be placed in a visit order.
  Every decision is saved to `Visit_dates_checked_against_visit_code.csv`;
  cases that cannot be settled are marked "check" and are not guessed. This can
  change visit order, the baseline sample and the follow-up classification for
  the women affected.
* The participant flow diagram (Figure 1) now continues down to the outcome
  categories: it shows which women are excluded at each step, the women with the
  full number of visits that are analysed, how many are placed in the outcome
  categories, and how many are set aside (with the reason, for example
  "genotype not determined" or "baseline not HPV-positive").
* Repeats marked with a letter after the date (`070422b`, `140622B`) are now
  recognised as repeats of their original sample, in the QC-passed database
  ("latest" and "original" now include them) and in the follow-up analysis.
* In the follow-up analysis, when a visit has an original sample and repeats, the
  **latest repeat** is used (it used to be the original). A repeat is run because
  the first result was not trusted. This changes the classification of some
  women, and is recorded in the `Settings_used` sheet.

