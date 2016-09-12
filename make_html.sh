#!/bin/bash

git checkout stable
git pull
git merge emqtt.com
make html
cp -r build/html/* ../emqtt.com/public/docs/v1/
git reset --hard HEAD~

git checkout emq20
git pull
git merge emqtt.com
make html
cp -r build/html/* ../emqtt.com/public/docs/v2/
git reset --hard HEAD~

git checkout master
