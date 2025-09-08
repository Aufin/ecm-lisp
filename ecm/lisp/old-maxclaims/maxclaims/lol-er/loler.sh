#!/bin/bash

set -e

#cd ~/SourceCode/lisp
export lispd=`pwd`

## assumes that the lol-er directory will be in $lispd
## so the paths will be ~/SourceCode/lisp/lol-er and
## ~/SourceCode/lisp/clbuid

function clbuilder {
    echo "building clbuild ... "
    cd $lispd
    darcs get http://common-lisp.net/project/clbuild/clbuild
    cd clbuild && chmod +x ./clbuild
    ./clbuild update sbcl
    echo "building threaded sbcl ... "
    cp $lispd/lol-er/customize-target-features.lisp $lispd/clbuild/source/sbcl/
    ./clbuild compile-implementation sbcl &&
    echo "sbcl built."
    echo "installing [slime] .."
    ./clbuild install slime
    echo "[Done]"
    # patcher
    #cp $lispd/lol-er/dependencies $lispd/clbuild/
    #cp $lispd/lol-er/wnpp-projects $lispd/clbuild/
    echo "Installing web/network dependencies ..."
    ./clbuild install ucw  lisp-on-lines relational-objects-for-lisp rfc2388-binary rfc2388 drakma closure-html cxml cxml-stp iolib cl-fad cl-irc md5 ironclad asdf-system-connections computed-class vecto
    echo "Done."
    pwd
}

function macsbcl {
    echo "building clbuild ... "
    cd $lispd
    darcs get http://common-lisp.net/project/clbuild/clbuild
    cd clbuild && chmod +x ./clbuild
    ./clbuild update sbcl
    echo "building non-threaded sbcl for OSX/ppc ... "
    cp $lispd/lol-er/macosxppc.features.lisp $lispd/clbuild/source/sbcl/customize-target-features.lisp
    .//clbuild compile-implementation sbcl &&
    echo "sbcl built."
    ./clbuild install slime
     # patcher
    cp $lispd/lol-er/dependencies $lispd/clbuild/
    cp $lispd/lol-er/wnpp-projects $lispd/clbuild/
    echo "Building lisp-on-lines ..."
    ./clbuild install lisp-on-lines
    echo "Done."
    pwd
}   

function ccler {
    echo "building clbuild ... "
    cd $lispd
    darcs get http://common-lisp.net/project/clbuild/clbuild ./cclbuild
    cd cclbuild && chmod +x ./clbuild
    echo "getting LoL for clozure common lisp .. "
    # the following files are no longer needed. they're in clbuild upstream.
    # cp $lispd/lol-er/dependencies $lispd/cclbuild/
    # cp $lispd/lol-er/wnpp-projects $lispd/cclbuild/
    cp $lispd/lol-er/clbuild.conf $lispd/cclbuild/
    echo "Building lisp-on-lines .. "
    ./clbuild install ucw  lisp-on-lines relational-objects-for-lisp rfc2388-binary rfc2388 drakma closure-html cxml cxml-stp iolib cl-fad cl-irc md5 ironclad
    echo "Done."
}

function cclinks {

    if [ -h $lispd/cclbuild/systems/kinsasha.asd ]
	then
	rm -rf $lispd/cclbuild/systems/kinsasha.asd
    fi
    if [ -h $lispd/cclbuild/systems/ktour.asd ]
	then
	rm -rf  $lispd/cclbuild/systems/ktour.asd
    fi

    echo -n "linking local systems into ASDF ... "
    ln -s $lispd/kinsasha/kinsasha.asd $lispd/cclbuild/systems/kinsasha.asd &&
    echo -n "[kinsasha] "
    ln -s $lispd/knighttour/ktour.asd $lispd/cclbuild/systems/ktour.asd &&
    echo -n "[knighttour] "
    echo "[Done.]"
    cd $lispd/lol-er
    echo -n "removing old fasls ... "
    find . -name "*fasl" -exec rm {} \;
    echo "[Done]"
}

function links {

    if [ -h $lispd/clbuild/systems/kinsasha.asd ]
	then
	rm -rf $lispd/clbuild/systems/kinsasha.asd
    fi
    if [ -h $lispd/clbuild/systems/srsly.asd ]
	then
	rm -rf $lispd/clbuild/systems/srsly.asd
    fi
    if [ -h $lispd/clbuild/systems/ktour.asd ]
	then
	rm -rf  $lispd/clbuild/systems/ktour.asd
    fi
    if [ -h $lispd/clbuild/systems/maxclaims.asd ]
	then
	rm -rf $lispd/clbuild/systems/maxclaims.asd
    fi
    echo -n "linking local systems into ASDF ... "
    ln -s $lispd/srsly/srsly.asd $lispd/clbuild/systems/srsly.asd &&
    ln -s $lispd/kinsasha/kinsasha.asd $lispd/clbuild/systems/kinsasha.asd &&

    echo -n "[opensrs] "
    ln -s $lispd/knighttour/ktour.asd $lispd/clbuild/systems/ktour.asd &&
    echo -n "[knighttour] "	
    ln -s $lispd/maxclaims-dev/maxclaims.asd $lispd/clbuild/systems/maxclaims.asd &&
    echo -n "[maxclaims] "
    echo "[Done.]"
    cd $lispd/lol-er
    echo -n "removing old fasls ... "
    find . -name "*fasl" -exec rm {} \;
    echo "[Done]"
    cd $lispd/clbuild
    pwd
}


if [ $1 = "link" ]
then
    links
fi

if [ $1 = "ccl" ]
then
    if [ -d $lispd/old.cclbuild ]
    then
	echo -n "removing old.clbuild backup dir .."
	rm -rf $lispd/old.cclbuild
	echo "Done."
    fi
    if [ -d $lispd/cclbuild ]
    then
	echo -n "Moving clbuild to old.clbuild .. "
	mv $lispd/cclbuild $lispd/old.cclbuild
	echo "Done."
    fi
    ccler
    cclinks
    exit 0
fi

if [ $1 = "macsbcl" ]
then
    if [ -d $lispd/old.clbuild ]
    then
	echo -n "removing old.clbuild backup dir .."
	rm -rf $lispd/old.clbuild
	echo "Done."
    fi
    if [ -d $lispd/clbuild ]
    then
	echo -n "Moving clbuild to old.clbuild .. "
	mv $lispd/clbuild $lispd/old.clbuild
	echo "Done."
    fi
    macsbcl
    exit 0
fi


if [ $1 = "rebuild" ]
then
    if [ -d $lispd/clbuild ]
    then

	if [ -d $lispd/old.clbuild ]
	then
	    echo -n "removing old.clbuild backup dir .."
	    rm -rf $lispd/old.clbuild
	    echo "Done."
	fi

	echo -n "Moving clbuild to old.clbuild .. "
	mv $lispd/clbuild $lispd/old.clbuild
	echo "Done."
    fi
    clbuilder
    links
    exit 0
fi
