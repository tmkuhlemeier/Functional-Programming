module MzMap where

type MazeMap = [String]
--the mazemap expressed in characters in cells
mazeMap = reverse ["#########################", "#########################", "#...........#...........#", "#E###.#####.#.#####.###E#", "#.###.#####.#.#####.###.#", "#.......................#", "#.###.#.#########.#.###.#", "#.....#.....#.....#.....#", "#####.##### # #####.#####", "#####.#           #.#####", "#####.# ####-#### #.#####", "#####.# #       # #.#####", "#    .  #       #  .    #", "#####.# ######### #.#####", "#####.#           #.#####", "#####.# ######### #.#####", "#...........#...........#", "#.###.#####.#.#####.###.#", "#E..#...............#..E#", "###.#.#.#########.#.#.###", "#.....#.....#.....#.....#", "#.#########.#.#########.#", "#.......................#", "#########################", "#########################"]