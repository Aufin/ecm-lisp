ARG build_cores=4
ARG gerbil_branch=ecm-dev

# The base distro. Alpine!
FROM alpine:3 as base
RUN apk update && apk add autoconf \
	automake \
	cmake \
	curl \
	g++ \
	gcc \
	git \
	libgcc \
	libsecp256k1-dev \
	libtool \
	libxml2-dev \
	libxml2-static \
	linux-headers \
	make \
	musl \
	musl-dev \
	nodejs \
	openssl-dev \
	openssl-libs-static \
	ruby \
	sqlite-dev \
	sqlite-static \
	xz-static \
	yaml-dev \
	yaml-static \
	zlib-static

# Now the lisp app + detachtty
FROM base as lisp
RUN apk add sudo sbcl
COPY <<EOF  /root/.sbclrc
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       "/srv/ecm/lisp/")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
EOF
# RUN cat /root/.sbclrc
COPY ecm/lisp/ /srv/ecm/lisp
RUN cd /tmp && git clone https://github.com/cosmos72/detachtty.git && \
	cd detachtty &&   \
	make &&      \
	make install && cd .. && rm -rf detachtty

RUN cd /tmp && /srv/ecm/lisp/old-maxclaims/maxclaims/ecm/make.sh && \
 mv ecm-application /usr/local/bin
RUN ln -s /srv/ecm/etc/ecm /root/.ecm
EXPOSE 4242/tcp
EXPOSE 4005/tcp


# Install Gerbil Scheme
FROM base as gerbil
ARG gerbil_branch
ARG build_cores
ENV GERBIL_BUILD_CORES=$build_cores
RUN echo "Building gerbil ${gerbil_branch} on ${build_cores} cores"
RUN cd /tmp && git clone "https://github.com/mighty-gerbils/gerbil.git" \
	&& cd gerbil && git checkout ${gerbil_branch} && ./configure --prefix=/opt/gerbil
RUN cd /tmp/gerbil && make -j${build_cores}
RUN cd /tmp/gerbil && mkdir -p /opt && make install
RUN rm -rf /tmp/gerbil

FROM base as caddyshack
RUN apk update && apk add caddy

FROM caddyshack AS ecm-appd
COPY --from=gerbil /opt/gerbil /opt/gerbil
COPY --from=lisp /usr/local/bin/ecm-application /usr/local/bin
COPY --from=lisp /usr/local/bin/detachtty /usr/local/bin
COPY --from=lisp /usr/local/bin/attachtty /usr/local/bin

RUN apk add nss-tools jq rlwrap

COPY ecm /srv/ecm
# RUN ls /srv


RUN mkdir -p /srv/ecm/var/log/ecm-lisp
RUN mkdir -p /srv/ecm/var/log/ecm-appd

RUN ln -s /srv/ecm/app/bin/ecm-appd.sh /usr/local/bin/ecm-appd
RUN ln -s /srv/ecm/etc/ecm /root/.ecm

RUN ls -l /root/.ecm

## ** Rebuild the app
ENV GERBIL_PATH="/srv/ecm/app/.gerbil"
ENV PATH=/opt/gerbil/bin:"${PATH}"
RUN cd /srv/ecm/app && ./build.ss

EXPOSE 80/tcp
EXPOSE 443/tcp
EXPOSE 8080/tcp
EXPOSE 4242/tcp
EXPOSE 4005/tcp

WORKDIR /srv/ecm
CMD ecm-appd
