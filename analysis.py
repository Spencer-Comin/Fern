import os
from pathlib import PurePath


def count_semantic_lines(lines, comment_char):
    return len(list(filter(
        lambda ln: ln and not ln.startswith(comment_char),
        map(str.strip, lines)
    )))


# very basic, ignores C++ style /* ... */ and other non-newline-terminated comment styles
comment_types = {
    '.h': '//',     # C/C++ headers
    '.cpp': '//',   # C++ source
    '.pl': '%',     # C source
    '.txt': '#',    # CMakeLists.txt
    '.frn': '//',   # Fern
    '.py': '#',     # Python
}
excluded_dirs = [
    PurePath('./build')
]
total_count = 0

for path, _, filenames in os.walk('.'):
    path = PurePath(path)
    if any(map(lambda ex: path.is_relative_to(ex), excluded_dirs)):
        continue
    for filename in map(lambda name: os.path.join(path, name), filenames):
        _, ext = os.path.splitext(filename)
        if ext in comment_types:
            with open(filename) as file:
                file_count = count_semantic_lines(file, comment_types[ext])
                print(f'\t{filename}: {file_count} SLOC')
                total_count += file_count

print(f'TOTAL: {total_count} SLOC')
