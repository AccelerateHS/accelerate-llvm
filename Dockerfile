# vim: nospell
FROM fpco/stack-build:lts-8.0
MAINTAINER Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>

# Install llvm-4.0
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main"
RUN apt-get update
RUN apt-get install -y llvm-4.0-dev

# Install ghc-8.0.2
RUN mkdir -p /root/.stack/global/
RUN echo "resolver: lts-8.0" > /root/.stack/global/stack.yaml
RUN stack setup

# Copy over just the cabal and stack file and install dependencies
WORKDIR /opt/accelerate-llvm
COPY ./stack-8.0.yaml /opt/accelerate-llvm/stack.yaml
COPY ./accelerate-llvm/accelerate-llvm.cabal /opt/accelerate-llvm/accelerate-llvm/
COPY ./accelerate-llvm-native/accelerate-llvm-native.cabal /opt/accelerate-llvm/accelerate-llvm-native/
COPY ./accelerate-llvm-ptx/accelerate-llvm-ptx.cabal /opt/accelerate-llvm/accelerate-llvm-ptx/
RUN stack build accelerate-llvm accelerate-llvm-native --only-dependencies

# Copy over the source files and build
COPY ./accelerate-llvm /opt/accelerate-llvm/accelerate-llvm
RUN stack build accelerate-llvm

COPY ./accelerate-llvm-native /opt/accelerate-llvm/accelerate-llvm-native
RUN stack build accelerate-llvm-native

