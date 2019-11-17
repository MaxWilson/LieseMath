pushd ~%dp0
git worktree add ..\LieseMathPublish gh-pages
git checkout master && npm run build && (robocopy /s deploy ..\LieseMathPublish || echo OK) && pushd ..\BMathPublish && git add . && git commit -m "New Version" && git push && popd
