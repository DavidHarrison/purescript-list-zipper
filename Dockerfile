FROM node:5.9

RUN npm install -g purescript purescript-psa pulp bower
RUN adduser --disabled-password --gecos '' user && \
    adduser user sudo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R user /usr/src
