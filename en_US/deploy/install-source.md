# Install from Source code

## Dependencies

To compile and install EMQX from the source code, the following dependencies are needed: 

- Erlang/OTP OTP 24 or 25 
- GCC 4.8 or higher versions
- Git
- make
- openssl-devel
- libcurl-devel

You can use the Docker compilation environment [EMQX Builder](https://github.com/emqx/emqx-builder) to compile and install EMQX from source code.

With the following commands, you can create an EMQX Builder container, configure the port mapping (optional), and start the preview after the compilation is complete:

```bash
docker run -d --name emqx-builder \
  # -p 1883:1883 \
  # -p 8083:8083 \
  # -p 8084:8084 \
  # -p 8883:8883 \
  # -p 18083:18083 \
  ghcr.io/emqx/emqx-builder/5.0-17:1.13.4-24.2.1-1-ubuntu20.04 \
  bash -c "tail -f /dev/null"
```

## Compile and start EMQX

The EMQX repository is located at <https://github.com/emqx/emqx>, where the `master` branch is the latest EMQX 5.0 version, and the `main-*` branch corresponds to different minor versions (such as 4.4.x, 5.1. x).

To compile and run the latest version of EMQX, run: 

```bash
# docker exec -it emqx-builder bash
git clone https://github.com/emqx/emqx.git
cd emqx
make
_build/emqx/rel/emqx/bin/emqx console
```
