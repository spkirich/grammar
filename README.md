# grammar

A context-free grammar processing library for Haskell enjoyers

## Development

The library is under development yet.
There is some work to be done:

- [ ] normalization;
    - [x] elimination of useless nonterminals;
    - [x] elimination of long productions;
    - [ ] elimination of epsilon-productions;
    - [ ] elimination of unit productions;
    - [ ] reducing to Chomsky normal form;
    - [ ] elimination of left recursion;
    - [ ] reducing to Greibach weak normal form;
    - [ ] reducing to Kuroda normal form;
- [ ] simple regularity check;
- [ ] regular approximation.

## Acknowledges

This library is inspired by lectures of Alexey Belousov and
[Antonina Nepeivoda](https://github.com/TonitaN). Moreover, it is
based upon an amazing
[work](https://github.com/StarikTenger/RegularLanguageProblem) by
[Andrei Ilin](https://github.com/StarikTenger),
[Edgar Makarov](https://github.com/Robby-the-paladin) and
[Sofya Belyakova](https://github.com/SoFa325).
