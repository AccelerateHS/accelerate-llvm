# vim: nospell

# https://hub.docker.com/r/nvidia/cuda/
FROM nvidia/cuda:9.2-devel-ubuntu16.04
LABEL maintainer "Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>"

ARG DEBIAN_FRONTEND=noninteractive
ARG PREFIX=/opt/accelerate-llvm

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
ENV PATH /root/.cabal/bin:/root/.local/bin:${PATH}
ENV LD_LIBRARY_PATH /usr/local/cuda/lib64:/usr/local/cuda/nvvm/lib64:${LD_LIBRARY_PATH}
RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/libcuda.so.1

RUN add-apt-repository -y ppa:hvr/ghc \
 && apt-get update \
 && apt-get install -y software-properties-common curl netbase pkg-config wget

RUN curl -sSL https://get.haskellstack.org/ | sh

# Buggy versions of ld.bfd fail to link some Haskell packages:
# https://sourceware.org/bugzilla/show_bug.cgi?id=17689. Gold is
# faster anyways and uses less RAM.
RUN update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
RUN update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10

# Install LLVM
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main" \
 && add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" \
 && apt-get update \
 && apt-get install -y llvm-5.0-dev llvm-6.0-dev

# GHC requires a specific LLVM version on the system PATH for its LLVM backend.
# This version is tracked here:
# https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM/Installing
#
# GHC 8.4 requires LLVM 5.0 tools (specifically, llc and opt)
RUN update-alternatives --install "/usr/bin/llc" "llc" "/usr/bin/llc-5.0" 50
RUN update-alternatives --install "/usr/bin/opt" "opt" "/usr/bin/opt-5.0" 50

# Setup stack
WORKDIR ${PREFIX}
COPY ./stack-8.4.yaml ${PREFIX}/stack.yaml
RUN stack --no-terminal --color never setup
RUN stack --no-terminal --color never install c2hs

# Copy over just the cabal and stack file and install dependencies
COPY ./README.md ${PREFIX}/README.md
COPY ./LICENSE   ${PREFIX}/LICENSE
COPY ./accelerate-llvm/accelerate-llvm.cabal                ${PREFIX}/accelerate-llvm/
COPY ./accelerate-llvm-native/accelerate-llvm-native.cabal  ${PREFIX}/accelerate-llvm-native/
COPY ./accelerate-llvm-ptx/accelerate-llvm-ptx.cabal        ${PREFIX}/accelerate-llvm-ptx/
RUN stack --no-terminal --color never build --only-snapshot

# Copy over the actual source files and build
COPY ./accelerate-llvm ${PREFIX}/accelerate-llvm
RUN stack --no-terminal --color never build accelerate-llvm

COPY ./accelerate-llvm-native ${PREFIX}/accelerate-llvm-native
RUN stack --no-terminal --color never build accelerate-llvm-native

COPY ./accelerate-llvm-ptx ${PREFIX}/accelerate-llvm-ptx
RUN stack --no-terminal --color never build accelerate-llvm-ptx

CMD ["bash"]

