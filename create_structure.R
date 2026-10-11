writeLines(capture.output(fs::dir_tree(".", recurse = 3)), "STRUCTURE.md")
