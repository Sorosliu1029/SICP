#!/usr/bin/env bash

chapter=$(echo $1 | awk -F'.' '{print $1}')
exercise=$(echo $1 | awk -F'.' '{print $2}')

echo -n "Git add chapter$chapter/$chapter-$exercise.scm"
git add chapter$chapter/$chapter-$exercise.scm && echo "... Done"

echo -n "Mark in README.md"
sed -Ei "s/\:white_large_square\:( \[$chapter.$exercise\])/\:white_check_mark\:\1/g" README.md && echo "... Done"