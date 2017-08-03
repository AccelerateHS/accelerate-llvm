# vim: nospell

FROM nvidia/cuda:8.0-devel-ubuntu16.04
LABEL maintainer "Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>"

ARG GHC_VERSION=8.0.2
ARG CABAL_VERSION=1.24
ARG LTS_SLUG=lts-9.0
ARG DEBIAN_FRONTEND=noninteractive

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
ENV PATH /root/.cabal/bin:/root/.local/bin:${PATH}
ENV LD_LIBRARY_PATH /usr/local/cuda/lib64:/usr/local/cuda/nvvm/lib64:${LD_LIBRARY_PATH}
RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/libcuda.so.1

RUN apt-get update \
 && apt-get install -y software-properties-common

RUN add-apt-repository -y ppa:hvr/ghc \
 && apt-get update \
 && apt-get install -y \
      curl \
      llvm-3.7 \
      pkg-config \
      wget

RUN curl -sSL https://get.haskellstack.org/ | sh

# Buggy versions of ld.bfd fail to link some Haskell packages:
# https://sourceware.org/bugzilla/show_bug.cgi?id=17689. Gold is
# faster anyways and uses less RAM.
RUN update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
RUN update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10

# GHC requires a specific LLVM version on the system PATH for its LLVM backend.
# This version is tracked here:
# https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM/Installing
#
# GHC 8.0 requires LLVM 3.7 tools (specifically, llc-3.7 and opt-3.7).
RUN update-alternatives --install "/usr/bin/llc" "llc" "/usr/bin/llc-3.7" 50
RUN update-alternatives --install "/usr/bin/opt" "opt" "/usr/bin/opt-3.7" 50

# Install llvm-4.0
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main" \
 && apt-get update \
 && apt-get install -y llvm-4.0-dev

# Setup stack
RUN stack --no-terminal --resolver=${LTS_SLUG} setup
RUN stack --no-terminal install c2hs

# Copy over just the cabal and stack file and install dependencies
WORKDIR /opt/accelerate-llvm
COPY ./README.md /opt/accelerate-llvm/README.md
COPY ./LICENSE /opt/accelerate-llvm/LICENSE
COPY ./stack-8.0.yaml /opt/accelerate-llvm/stack.yaml
COPY ./accelerate-llvm/accelerate-llvm.cabal /opt/accelerate-llvm/accelerate-llvm/
COPY ./accelerate-llvm-native/accelerate-llvm-native.cabal /opt/accelerate-llvm/accelerate-llvm-native/
COPY ./accelerate-llvm-ptx/accelerate-llvm-ptx.cabal /opt/accelerate-llvm/accelerate-llvm-ptx/
RUN stack --no-terminal build --only-snapshot

# Copy over the actual source files and build
COPY ./accelerate-llvm /opt/accelerate-llvm/accelerate-llvm
RUN stack --no-terminal build accelerate-llvm

COPY ./accelerate-llvm-native /opt/accelerate-llvm/accelerate-llvm-native
RUN stack --no-terminal build accelerate-llvm-native

COPY ./accelerate-llvm-ptx /opt/accelerate-llvm/accelerate-llvm-ptx
RUN stack --no-terminal build accelerate-llvm-ptx

COPY ./utils/ghci /opt/accelerate-llvm/
CMD ["bash"]

