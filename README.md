# BuildMEP AutoCAD LISP Tools

Free AutoCAD LISP routines for MEP engineers: HVAC, fire protection, electrical and plumbing takeoffs.
Maintained by [BuildMEP](https://buildmep.com).

## MEPCOUNT: Block Counter

Counts every block in a selection (or the whole current space) and reports quantities grouped by block name, or by block name + layer.

- Dynamic blocks are counted under their real name (not `*U123`)
- Xrefs and AutoCAD's internal anonymous blocks (`*X`, `*U`, `*T`) are skipped
- Output to the command line, an AutoCAD table in the drawing, and/or a CSV file for Excel

**Command:** `MEPCOUNT`

Full guide with screenshots: https://buildmep.com/autocad-block-count-lisp/

### Install
1. Download `MEPCOUNT.lsp`
2. In AutoCAD, type `APPLOAD`, select the file and click **Load**
3. To load it in every drawing, add it to the **Startup Suite** in the same dialog
4. Type `MEPCOUNT`

### Requirements
Full AutoCAD or an AutoCAD-based product (AutoCAD MEP, Architecture, Civil 3D). Uses Visual LISP ActiveX functions, so it does not run in AutoCAD LT.

### Limits
- Blocks nested inside other blocks are not opened
- Stacked duplicates are counted twice (run `OVERKILL` on a copy first)

## More free tools
- [Free MEP calculators](https://buildmep.com/) (pipe sizing, pump head, heat load)

## License
MIT. Free to use, modify and share. Test on a copy of your drawing first.
