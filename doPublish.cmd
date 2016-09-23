pushd \code\BadanarniMath
git checkout master && xcopy public\* publish\ /y && webpack -p && git checkout gh-pages && xcopy publish\* /y . && git add . && git commit
