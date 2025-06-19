# Chess data and stat 

Hi, this part of the repo is dedicated to chess, welcome!

## Puzzle rush
Puzzle Rush is a puzzle solving mode on `chess.com` where one has to solve as many chess puzzles as possible up to three mistakes. There are three time controls—3 mins, 5 mins and no time. I enjoy this mode very much and play it a lot. My current record (16.05.2025) for 3 mins is 25 puzzles and I want to improve. 

The initial idea was to simply scrap my data from `chess.com` API, but it's unfortunately not enough for me. So I created two Jupyter files to track my progress.  

1. **[Puzzle rush progress for the current month](puzzle-rush/puzzle_rush.ipynb):** a script that takes an input from the command line, adds it to a csv and then creates a plot for the current month. 
2. **[Total puzzle rush stat](puzzle-rush/total_stat_puzzle_rush.ipynb):** some basic descriptive stat for the whole journey.

## 100 fails rapid and blitz 
I want to improve my game and win more, but I get so sad when I lose. Counting rating also sucks. So, I decided to flip all of it and count losses instead in this challenge form. 

[This simple script](100-fails-challenges/100_challenge.ipynb) shows my progress on the challenges. Nothing fancy, it takes my info from `chess.com` API and compares it with the start date. 