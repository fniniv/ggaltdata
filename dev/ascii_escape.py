"""Replace non-ASCII characters in R/*.R with \\uXXXX escapes (CRAN asks for ASCII code).

Comments are kept ASCII by dropping accents; strings get escapes, so the text R prints is unchanged.
    python dev/ascii_escape.py
"""
import pathlib
import unicodedata

for p in pathlib.Path("R").glob("*.R"):
    out = []
    for line in p.read_text(encoding="utf-8").splitlines():
        if line.lstrip().startswith("#"):
            line = unicodedata.normalize("NFKD", line).encode("ascii", "ignore").decode()
        else:
            line = "".join(c if ord(c) < 128 else "\\u%04x" % ord(c) for c in line)
        out.append(line)
    p.write_text("\n".join(out) + "\n", encoding="utf-8")
left = [(str(p), i + 1) for p in pathlib.Path("R").glob("*.R")
        for i, l in enumerate(p.read_text(encoding="utf-8").splitlines()) if any(ord(c) > 127 for c in l)]
print("non-ASCII left:", left)
