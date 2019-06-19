# Feedback

## Jean-Michel Begon

La partie taquin était parfaite. 

Parmi les tests que j'ai effectué avec d'autres puzzles, un a rencontré des soucis pour le solver sans heuristique. En particulier, vous ne trouviez pas toutes les solutions et elle n'était pas toujours par ordre croissant pour ce puzzle.

Pour la partie avec heuristique, j'ai eu un problème de timeout sur un des puzzles où l'heuristique n'était pas très informative. Ca doit être dû à la complexité de la fonction rp-solve-heuristic

J'ai également quelques remarques sur les spécifications. Par exemple pour la fonction fun, que signifient « favoris » et « laissés de côté » ? En outre, on ne voit pas en quoi `visited`, `queue` et `waiting` influent sur le résultat de la fonction.