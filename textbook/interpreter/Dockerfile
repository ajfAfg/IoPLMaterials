FROM ocaml/opam:ubuntu-22.10-ocaml-5.0
COPY miniml.opam .
RUN opam install . --deps-only --with-test
RUN rm -f miniml.opam

# Installation of development tools
RUN opam install utop
RUN sudo apt-get update
RUN sudo apt-get install -y rlwrap
