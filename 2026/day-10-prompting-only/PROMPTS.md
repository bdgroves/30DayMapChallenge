# Day 10 · The prompts

The rule for this map: every line of code, FORTRAN and Python, is written by Claude from a prompt.
No hand edits. When something needs fixing, the fix is asked for here, in words, and the request is
added to this page.

## 1. The plan (September 2026)

> A map of Big Creek's watershed near Groveland where every line of code came from prompts, with the
> prompts posted alongside.

## 2. The prompt that made it (October 3, 2026)

> How about map 10, we make with cobol or Fortran prompts to make it old school like

That one sentence is the whole brief. Claude picked FORTRAN over COBOL because FORTRAN is where
computer maps began: SYMAP, the line-printer mapping program from Harvard's Laboratory for Computer
Graphics in the 1960s, was written in it and made its dark tones by printing several characters on
top of each other. So the map is a line-printer map. From that sentence Claude wrote:

- `BIGCRK.f`: the FORTRAN, in fixed form and upper case, nothing past column 72. It reads the grid,
  sorts the basin into eight elevation classes of whole hundreds of feet, and prints the map and a
  legend with ASA carriage control, overprinting for the dark classes.
- `make.py`: the Python on either side of it. It fetches the basin, streams and elevation, averages
  them into printer cells (a character is 1/10 inch wide and 1/6 inch tall, so each cell is 5/3 as
  tall as it is wide on the ground), compiles and runs the FORTRAN with gfortran, and prints the
  result on pretend green-bar paper.

## 3. Changes since

Claude's own, after looking at the first render (still no hand edits):

- The eighth class came out empty, because classes were whole hundreds of feet wide and the basin
  tops out just under 4,000 ft. Classes are now multiples of 25 ft, so the top class ends just
  above the highest cell.
- The LOW and HIGH line printed the rounded class limits; it now prints the real lowest and highest
  cells.
- The paper sat on the footer rule; the map was made a little shorter.

Asked for by Brooks: none yet.
