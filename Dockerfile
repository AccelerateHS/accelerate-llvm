# vim: nospell
FROM fpco/stack-build:lts-8.0
MAINTAINER Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>

# Install llvm-4.0
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main"
RUN apt-get update
RUN apt-get install -y llvm-4.0-dev

# Copy over the relevant files
WORKDIR /opt/accelerate-llvm
COPY ./stack-8.0.yaml /opt/accelerate-llvm/stack.yaml
RUN stack setup

COPY ./accelerate-llvm /opt/accelerate-llvm/accelerate-llvm
COPY ./accelerate-llvm-native /opt/accelerate-llvm/accelerate-llvm-native
COPY ./accelerate-llvm-ptx /opt/accelerate-llvm/accelerate-llvm-ptx

# Build accelerate-llvm-native
RUN stack build accelerate-llvm-native

