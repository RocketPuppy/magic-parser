#!/usr/bin/python

f = open('types.txt', 'r')
splits = f.read()
splits = splits.split(',')

f2 = open('out.txt', 'w')
for s in splits:
    s = s.strip()
    w = "<|>\ndo  try (pluralize \"{0}\") <|> string \"{0}\"\n    return {1}\n".format(s.lower(), s.capitalize())
    f2.write(w)
f2.close()