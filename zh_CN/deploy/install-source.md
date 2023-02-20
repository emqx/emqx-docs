# 源码编译安装

## 环境依赖

EMQX 源码编译安装需要以下环境依赖:

- Erlang/OTP OTP 24 或 25 版本
- GCC 4.8 或更高版本
- Git
- make
- openssl-devel
- libcurl-devel

您可以使用 EMQX 提供的 Docker 编译环境 [EMQX Builder](https://github.com/emqx/emqx-builder) 从源码编译 EMQX。

您可通过如下命令创建 EMQX Builder 容器，映射 EMQX 主要端口（可选）可以在编译完成后启动预览：

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

## 编译并启动

EMQX 仓库位于 <https://github.com/emqx/emqx>，其中 `master` 分支为最新的 EMQX 5.0 版本，`main-*` 分支则对应不同的次要版本（如 4.4.x、5.1.x）。

以下是编译并启动 EMQX 5.0 最新版本的操作步骤：

```bash
# docker exec -it emqx-builder bash
git clone https://github.com/emqx/emqx.git
cd emqx
make
_build/emqx/rel/emqx/bin/emqx console
```
