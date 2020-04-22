##################################
# Docker image for Final Project
##################################
# The project description can be found on BlackBoard,
#   but this file serves as a guide for testing
#   your code against the versions we will be using
#   for grading your assignments.
#
# To test your code with this container, first build the image:
#   docker build -t NAME_HERE .
# Then run the container and bind your code directory to the container:
#   docker run -it --rm -v ./:/code/ NAME_HERE
# And use the relevant compiler / interpreter on your code
#
# Make sure that, regardless of which language you choose, you also
#   submit a Makefile that responds to the following rules:
# - build
#     This command should "build" your code, whatever that means for your project.
#     ex. For python, this might pull in any pip dependencies, etc.
# - run
#     This command should "run" your code, putting the user into a prompt for SQL
#     queries.
FROM alpine:edge

# We allow for the following languages and versions:
# - Java 11 (OpenJDK)
# - C (using gcc v9.3.0-r1)
# - C++ (using g++ v9.3.0-r1)
# - Python3 v3.8.2-r6
# - Haskell (GHC v8.8.3-r0)
# - Ruby (v2.6.6-r4)
# - Javascript (using NodeJS v12.16.2-r0)
# We have also included vim and nano if you wish to edit files in the container.
#   I recommend that you do all of the editing on the host side and leave the container only
#   for testing that your code works with the versions we will be using.
RUN apk --no-cache add openjdk11-jdk gcc g++ python3 python3-dev py3-pip ghc ruby nodejs bash make vim nano curl

# Install stack for use with haskell
RUN curl -sSL https://get.haskellstack.org/ | sh

ENTRYPOINT ["/bin/bash"]