# /usr/bin/env python3
import os
import glob

FEATURES = ["dots", "equivalence", "metavar", "misc"]
VERBOSE_FEATURE_NAME = {
  "dots": "'...' operator",
  "equivalence": "Equivalences",
  "metavar": "Metavariables",
  "misc": "Others",
}

LANG_DIR_TO_EXT = {"python": "py"}
EXCLUDE = ["TODO", "GENERIC", "fuzzy", "lint"]

def get_emoji(count: int):
  if count == 0:
    return "\U0001F6A7"
  elif count < 5:
    return "\U0001F536"
  else:
    return "\U00002705"

def print_to_html(stats):
    def append_td(l, name):
      l.append("<td>")
      l.append(name)
      l.append("</td>")

    tags = ['<table style="text-align:center">', '<tr>']
    languages = stats.keys()
    append_td(tags, "")
    for lang in languages:
      append_td(tags, f"<b>{lang}</b>")
    tags.append('</tr>')

    for f in FEATURES:
      tags.append('<tr>')
      append_td(tags, f"{VERBOSE_FEATURE_NAME.get(f)}")
      for lang in languages:
        append_td(tags, f"{get_emoji(stats[lang].get(f, 0))}")
      tags.append('</tr>')
    tags.append("</table>")
    return "\n".join(tags)

def compute_stats(dir_name: str, lang_dir: str):
    path = os.path.join(dir_name, lang_dir)
    count_per_feature = {}
    for f in FEATURES:
       count_per_feature[f] = len(glob.glob1(path,f"{f}*.{LANG_DIR_TO_EXT.get(lang_dir, lang_dir)}"))
    return count_per_feature

def main(dir_name: str) -> None:
    files = os.listdir(dir_name)
    language_dirs = [f for f in files if os.path.isdir(f) and not f in EXCLUDE]
    stats = {lang: compute_stats(dir_name, lang) for lang in language_dirs}
    print(f"{print_to_html(stats)}")

if __name__ == '__main__':
  main(os.getcwd())