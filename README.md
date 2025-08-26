# Quiz pour apprendre le syllabaire Hangul

Ce projet fournit un petit quiz en ligne de commande pour pratiquer le syllabaire coréen.

## Utilisation

```bash
cabal run hangul-quiz -- [options]
```

Le programme pose par défaut trois questions aléatoires et affiche le score final.

Options disponibles :

- `-n N`, `--tests N` : nombre de questions (utiliser `inf` pour un quiz infini).
- `-d DIR`, `--direction DIR` : sens des questions : `st` (syllabe → transcription),
  `ts` (transcription → syllabe) ou `both`.

Pendant un quiz infini, tapez `:q` pour quitter.

## Tests

Les tests unitaires peuvent être exécutés avec :

```bash
cabal test
```
