#!/usr/bin/env bash

BASEDIR=$(dirname "$0")
CHAPTER=$(echo $1 | awk -F'.' '{print $1}')
EXERCISE=$(echo $1 | awk -F'.' '{print $2}')

FILE_PATH="$BASEDIR/chapter$CHAPTER/$CHAPTER-$EXERCISE.scm"
README_PATH="$BASEDIR/README.md"

echo -n "Mark in README.md"
sed -Ei "s/\:white_large_square\:( \[$CHAPTER.$EXERCISE\])/\:white_check_mark\:\1/g" $README_PATH && echo " ... Done"

echo -n "Git add $FILE_PATH and $README_PATH"
git add $FILE_PATH $README_PATH && echo " ... Done"

# check if file is staged
git diff --cached --exit-code --quiet $FILE_PATH
# if not staged, exit and error
if [ $? -ne 1 ]; then
  echo "Error: $FILE_PATH is not staged"
  exit 1
fi

echo "Git commit"
git commit -m "finish exercise $CHAPTER.$EXERCISE" && echo "✅ Done"
