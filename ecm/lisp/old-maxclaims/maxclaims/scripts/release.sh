REPO=$HOME/MaxclaimsNew/maxclaims

(cd $REPO && git pull)

su -c 'sv stop maxclaims' \
 && sbcl --eval '(pushnew :maxclaims-make-release *features*)'  --load $REPO/scripts/load.lisp \
 && su -c 'sv start maxclaims'


