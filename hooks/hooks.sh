#!/usr/bin/env bash

set -eu

hook="$(basename "$0")"

tmp_dir="$(mktemp -d)"

trap 'printf "rm -rf %s\n" "$tmp_dir"; rm -rf "$tmp_dir"' EXIT

git checkout-index --prefix="$tmp_dir/" -a

ln -s "$(realpath node_modules)" "$tmp_dir"
ln -s "$(realpath output)" "$tmp_dir"
ln -s "$(realpath .spago)" "$tmp_dir"

cd "$tmp_dir"

git init
git add .
git commit -m 'Test commit'

case $hook in
    pre-commit)
        npm run lint
        ;;

    pre-push)
        npm test
        npm start
        ;;

    *)
        echo Unknown hook "$hook"
        exit 1
        ;;
esac
